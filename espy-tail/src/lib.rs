use espy_ears::{Action, Binding, Block, Expression, If, Node, Statement};
use espy_eyes::Token;
use std::collections::HashMap;
use std::iter;

pub mod instruction {
    // Stack primatives: 0x00-0x0F
    pub const CLONE: u8 = 0x00;
    pub const WRITE: u8 = 0x01;
    pub const POP: u8 = 0x02;
    pub const COLLAPSE: u8 = 0x03;
    pub const JUMP: u8 = 0x04;
    pub const IF: u8 = 0x05;

    // Push ops: 0x10-0x2F
    pub const PUSH_UNIT: u8 = 0x10;
    pub const PUSH_I64: u8 = 0x11;
    pub const PUSH_FUNCTION: u8 = 0x12;
    pub const PUSH_TRUE: u8 = 0x13;
    pub const PUSH_FALSE: u8 = 0x14;

    // Operations: 0x30-0x4F
    pub const ADD: u8 = 0x30;
    pub const SUB: u8 = 0x31;
    pub const MUL: u8 = 0x32;
    pub const DIV: u8 = 0x33;
    pub const CALL: u8 = 0x34;
}

pub type ArchWidth = u32;
pub type ProgramCounter = ArchWidth;
pub type StackPointer = ArchWidth;
pub type BlockId = ArchWidth;

// These are practically used as functions which return iterators over bytes at this point.
// There isn't a good reason for this other than that they used to be stored for a little while,
// so if true functions make more sense for any reason this type can be removed.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Instruction {
    /// Copy a value from the given position and put it on the top of the stack.
    Clone(StackPointer),
    /// Pop a value off the stack and write it to the given position.
    Write(StackPointer),
    /// Pop a value off the stack.
    Pop,
    /// Pop a value off the stack and write it to the given position.
    /// Then, pop each value above this position and discard it.
    Collapse(StackPointer),
    /// Set the program counter.
    Jump(ProgramCounter),
    /// Pop a boolean value off the stack.
    /// If it is *false*, set the program counter.
    If(ProgramCounter),

    PushUnit,
    PushI64(i64),
    /// Pop the top `captures` values off the stack and onto a new stack,
    /// and then push a function containing the new stack and the proceeding block id to the current stack.
    PushFunction {
        captures: StackPointer,
        function: BlockId,
    },
    PushTrue,
    PushFalse,

    Add,
    Sub,
    Mul,
    Div,
    /// Pop the top value off the stack and push it to the function's stack.
    /// The next value is the function to be called.
    /// After pushing the value, jump to the function's block id.
    /// It will return a single value which is placed to the stack in its place.
    Call,
}

pub struct InstructionIter {
    instruction: Instruction,
    index: usize,
}

impl Iterator for InstructionIter {
    type Item = u8;

    fn next(&mut self) -> Option<Self::Item> {
        macro_rules! decompose {
            ($instruction:expr, $($arg:ident as $offset:literal..=$end:literal),*) => {
                match self.index {
                    0 => $instruction,
                    $(
                        $offset..=$end => $arg.to_le_bytes()[self.index - $offset],
                    )*
                    _ => return None,
                }
            };
        }
        let byte = match self.instruction {
            Instruction::Clone(from) => decompose!(instruction::CLONE, from as 1..=4),
            Instruction::Write(to) => decompose!(instruction::WRITE, to as 1..=4),
            Instruction::Pop => decompose!(instruction::POP,),
            Instruction::Collapse(to) => decompose!(instruction::COLLAPSE, to as 1..=4),
            Instruction::Jump(pc) => decompose!(instruction::JUMP, pc as 1..=4),
            Instruction::If(pc) => decompose!(instruction::IF, pc as 1..=4),

            Instruction::PushUnit => decompose!(instruction::PUSH_UNIT,),
            Instruction::PushI64(literal) => decompose!(instruction::PUSH_I64, literal as 1..=8),
            Instruction::PushFunction { captures, function } => {
                decompose!(instruction::PUSH_FUNCTION, captures as 1..=4, function as 5..=8)
            }
            Instruction::PushTrue => decompose!(instruction::PUSH_TRUE,),
            Instruction::PushFalse => decompose!(instruction::PUSH_FALSE,),

            Instruction::Add => decompose!(instruction::ADD,),
            Instruction::Sub => decompose!(instruction::SUB,),
            Instruction::Mul => decompose!(instruction::MUL,),
            Instruction::Div => decompose!(instruction::DIV,),
            Instruction::Call => decompose!(instruction::CALL,),
        };
        self.index += 1;
        Some(byte)
    }
}

impl IntoIterator for Instruction {
    type Item = u8;
    type IntoIter = InstructionIter;
    fn into_iter(self) -> Self::IntoIter {
        InstructionIter {
            instruction: self,
            index: 0,
        }
    }
}

#[derive(Clone, Debug, Default, PartialEq, Eq)]
struct Program {
    blocks: Vec<Vec<u8>>,
}

impl Program {
    // Remove this when defining the public API.
    #[allow(dead_code, reason = "currently only used for tests")]
    fn compile(self) -> Vec<u8> {
        // Reserve space for block offsets.
        // Blocks are referred to by index so this is the only backfilling required.
        // It would be possible to maintain a much smaller vector
        // (and lazily produce the program's bytecode)
        // by putting this at the end of the program.
        let mut output = Vec::from_iter(iter::repeat_n(0, self.blocks.len() * size_of::<u32>()));
        for (block_id, block) in self.blocks.into_iter().enumerate() {
            // Fill in offset.
            // The position of the first block implicitly determines the end of the block offsets.
            let offset = output.len();
            output[(block_id * 4)..(block_id * 4 + 4)]
                .copy_from_slice(&(offset as u32).to_le_bytes());
            output.extend(block);
        }
        output
    }

    fn create_block(&mut self) -> BlockId {
        self.blocks.push(Vec::new());
        // TODO: Must be handled.
        (self.blocks.len() - 1)
            .try_into()
            .expect("block limit reached")
    }

    fn add_block<'source>(
        &mut self,
        block_id: BlockId,
        block: Block<'source>,
        mut scope: Scope<'source>,
    ) {
        for i in block.statements {
            self.add_statement(block_id, i, &mut scope);
        }
        // Collapse the scope if any additional values are left the stack.
        // This occurs when statements bind variables.
        // We have to check this here because BlockResult::Function is about to move the scope,
        // but the instruction is not emitted until after the result is pushed to the stack.
        // This is fine because block results must push one and only one value.
        let collapse_point = scope
            .parent
            .filter(|parent| parent.stack_pointer < scope.stack_pointer)
            .map(|parent| parent.stack_pointer);
        match block.result {
            espy_ears::BlockResult::Expression(i) => {
                self.add_expression(block_id, i, &mut scope);
            }
            espy_ears::BlockResult::Function(function) => {
                let mut scope = scope.promote();
                let captures = scope.stack_pointer;
                // Note that, while a function takes a single argument,
                // the inner block recieves it destructured as expected by `with`.
                for argument in function.arguments {
                    scope.stack_pointer += 1;
                    scope.insert(argument.origin);
                }
                let function_id = self.create_block();
                self.add_block(function_id, function.block, scope);
                self.blocks[block_id as usize].extend(Instruction::PushFunction {
                    captures,
                    function: function_id,
                })
            }
        }
        if let Some(collapse_point) = collapse_point {
            self.blocks[block_id as usize].extend(Instruction::Collapse(collapse_point));
        }
    }

    /// # Panic
    ///
    /// Panics if provided an invalid statement.
    /// Statements should be validated beforehand by checking the `diagnostics.errors` field.
    fn add_statement<'source>(
        &mut self,
        block_id: BlockId,
        statement: Statement<'source>,
        scope: &mut Scope<'source>,
    ) {
        self.add_expression(block_id, statement.expression.unwrap_or_default(), scope);
        match statement.action {
            Some(Action::Binding(Binding { ident_token, .. })) => {
                scope.insert(ident_token.expect("invalid statement structure").origin);
            }
            Some(Action::Break(_)) => todo!(),
            None => self.blocks[block_id as usize].extend(Instruction::Pop),
        }
    }

    fn add_expression<'source>(
        &mut self,
        block_id: BlockId,
        expression: Expression<'source>,
        scope: &mut Scope<'source>,
    ) {
        // Shortcut for re-indexing self.blocks.
        // This is necessary because add_block etc take &mut self.
        macro_rules! block {
            () => {
                self.blocks[block_id as usize]
            };
        }
        if expression.contents.is_empty() {
            block!().extend(Instruction::PushUnit);
        } else {
            for node in expression.contents {
                match node {
                    Node::Unit => block!().extend(Instruction::PushUnit),
                    Node::Number(Token { origin, .. }) => {
                        // TODO: Must be handled.
                        let integer = origin.parse().expect("invalid i64");
                        scope.stack_pointer += 1;
                        block!().extend(Instruction::PushI64(integer))
                    }
                    Node::Ident(Token { origin, .. }) => {
                        // TODO: Must be handled.
                        let value = scope.get(origin).expect("undefined symbol");
                        scope.stack_pointer += 1;
                        block!().extend(Instruction::Clone(value.index))
                    }
                    Node::Add(_) => {
                        scope.stack_pointer -= 1;
                        block!().extend(Instruction::Add)
                    }
                    Node::Sub(_) => {
                        scope.stack_pointer -= 1;
                        block!().extend(Instruction::Sub)
                    }
                    Node::Mul(_) => {
                        scope.stack_pointer -= 1;
                        block!().extend(Instruction::Mul)
                    }
                    Node::Div(_) => {
                        scope.stack_pointer -= 1;
                        block!().extend(Instruction::Div)
                    }
                    Node::Call(_) => {
                        scope.stack_pointer -= 1;
                        block!().extend(Instruction::Call)
                    }
                    Node::Block(block) => {
                        self.add_block(block_id, block, scope.child());
                        scope.stack_pointer += 1;
                    }
                    Node::Bool(true, _) => {
                        scope.stack_pointer += 1;
                        block!().extend(Instruction::PushTrue)
                    }
                    Node::Bool(false, _) => {
                        scope.stack_pointer += 1;
                        block!().extend(Instruction::PushFalse)
                    }
                    Node::If(If {
                        condition,
                        first,
                        second,
                        ..
                    }) => {
                        // For retroactively filling in the jump instructions.
                        fn fill(block: &mut [u8], at: usize) {
                            let pc = block.len() as ProgramCounter;
                            block[at..(at + size_of::<ProgramCounter>())]
                                .copy_from_slice(&pc.to_le_bytes());
                        }

                        self.add_expression(block_id, condition, scope);
                        block!().extend(Instruction::If(0));
                        let if_destination = block!().len() - size_of::<ProgramCounter>();
                        self.add_block(block_id, first, scope.child());

                        // the first block always returns a value (though it may be implicit unit)
                        // so there always needs to be an else block with some value as well,
                        // but an empty else block will merely return unit.

                        // This jump is the last instruction of the first block--
                        // it skips over else.
                        block!().extend(Instruction::Jump(0));
                        let jump_destination = block!().len() - size_of::<ProgramCounter>();

                        // Now that the first block is complete,
                        // we can fill in the conditional jump's destination.
                        fill(&mut block!(), if_destination);
                        self.add_block(block_id, second, scope.child());

                        fill(&mut block!(), jump_destination);
                    }
                    Node::For(_) => todo!(),
                    Node::Pipe(_) => todo!(),
                    Node::Positive(_) => todo!(),
                    Node::Negative(_) => todo!(),
                    Node::BitwiseAnd(_) => todo!(),
                    Node::BitwiseOr(_) => todo!(),
                    Node::BitwiseXor(_) => todo!(),
                    Node::EqualTo(_) => todo!(),
                    Node::NotEqualTo(_) => todo!(),
                    Node::Greater(_) => todo!(),
                    Node::GreaterEqual(_) => todo!(),
                    Node::Lesser(_) => todo!(),
                    Node::LesserEqual(_) => todo!(),
                    Node::LogicalAnd(_) => todo!(),
                    Node::LogicalOr(_) => todo!(),
                    Node::Name(_) => todo!(),
                    Node::Tuple(_) => todo!(),
                    Node::Struct(_) => todo!(),
                    Node::Enum(_) => todo!(),
                };
            }
        }
    }
}

impl<'source> From<Block<'source>> for Program {
    fn from(block: Block<'source>) -> Self {
        let mut this = Self::default();
        let block_id = this.create_block();
        this.add_block(block_id, block, Scope::default());
        this
    }
}

#[derive(Clone, Copy, Debug)]
pub struct Value {
    index: StackPointer,
}

#[derive(Debug, Default)]
pub struct Scope<'source> {
    parent: Option<&'source Scope<'source>>,
    stack_pointer: StackPointer,
    bindings: HashMap<&'source str, Value>,
}

impl<'source> Scope<'source> {
    fn get(&self, k: &'source str) -> Option<Value> {
        self.bindings
            .get(k)
            .copied()
            .or_else(|| self.parent.and_then(|parent| parent.get(k)))
    }

    fn insert(&mut self, k: &'source str) {
        self.bindings.insert(
            k,
            Value {
                index: self.stack_pointer - 1,
            },
        );
    }

    fn child(&'source self) -> Self {
        Self {
            parent: Some(self),
            stack_pointer: self.stack_pointer,
            bindings: HashMap::new(),
        }
    }

    fn promote(mut self) -> Self {
        let lost_size = self.parent.map_or(0, |x| x.stack_pointer);
        self.parent = None;
        self.stack_pointer -= lost_size;
        for binding in &mut self.bindings {
            binding.1.index -= lost_size;
        }
        self
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use espy_eyes::Lexer;

    macro_rules! program {
        ($($num:expr),* $(,)?) => {
            {
                let mut program = Vec::new();
                $( program.extend($num.to_le_bytes()); )*
                program
            }
        }
    }

    #[test]
    fn variables_and_arithmetic() {
        let mut lexer = Lexer::from("let x = 1 + 2; let y = x * 3; x - y").peekable();
        let block = Block::from(&mut lexer);
        let program = Program::from(block);
        let actual = program.compile();
        let expected = program![
            4 as BlockId,
            // block 0
            instruction::PUSH_I64,
            1i64,
            instruction::PUSH_I64,
            2i64,
            instruction::ADD,
            instruction::CLONE,
            0 as StackPointer,
            instruction::PUSH_I64,
            3i64,
            instruction::MUL,
            instruction::CLONE,
            0 as StackPointer,
            instruction::CLONE,
            1 as StackPointer,
            instruction::SUB,
        ];
        assert_eq!(actual, expected);
    }

    #[test]
    fn simple_blocks() {
        let mut lexer = Lexer::from("let x = 2; 1 + { let y = 3; x * y }").peekable();
        let block = Block::from(&mut lexer);
        let program = Program::from(block);
        let actual = program.compile();
        let expected = program![
            4u32,
            // block 0
            instruction::PUSH_I64,
            2i64,
            instruction::PUSH_I64,
            1i64,
            instruction::PUSH_I64,
            3i64,
            instruction::CLONE,
            0 as StackPointer,
            instruction::CLONE,
            2 as StackPointer,
            instruction::MUL,
            instruction::COLLAPSE,
            2 as StackPointer,
            instruction::ADD,
        ];
        assert_eq!(actual, expected);
    }

    #[test]
    fn function_creation() {
        let mut lexer = Lexer::from("let x = 2; with y; x * y").peekable();
        let block = Block::from(&mut lexer);
        let program = Program::from(block);
        let actual = program.compile();
        let expected = program![
            8u32,
            26u32,
            // block 0
            instruction::PUSH_I64,
            2i64,
            instruction::PUSH_FUNCTION,
            1 as StackPointer,
            1 as BlockId,
            // block 1
            instruction::CLONE,
            0 as StackPointer,
            instruction::CLONE,
            1 as StackPointer,
            instruction::MUL,
        ];
        assert_eq!(actual, expected);
    }

    #[test]
    fn function_usage() {
        let mut lexer = Lexer::from("let f = {with x; x * x}; f 2").peekable();
        let block = Block::from(&mut lexer);
        let program = Program::from(block);
        let actual = program.compile();
        let expected = program![
            8u32,
            32u32,
            // block 0
            instruction::PUSH_FUNCTION,
            0 as StackPointer,
            1 as BlockId,
            instruction::CLONE,
            0 as StackPointer, // f
            instruction::PUSH_I64,
            2i64,
            instruction::CALL,
            // block 1 (f)
            instruction::CLONE,
            0 as StackPointer, // x
            instruction::CLONE,
            0 as StackPointer, // x
            instruction::MUL,
        ];
        assert_eq!(actual, expected);
    }

    #[test]
    fn if_expression() {
        let mut lexer = Lexer::from("if true then 1 else then 2 end").peekable();
        let block = Block::from(&mut lexer);
        let program = Program::from(block);
        let actual = program.compile();
        let expected = program![
            4u32,
            // block 0
            instruction::PUSH_TRUE,
            instruction::IF,
            20 as ProgramCounter,
            instruction::PUSH_I64,
            1i64,
            instruction::JUMP,
            29 as ProgramCounter,
            instruction::PUSH_I64,
            2i64,
        ];
        assert_eq!(actual, expected);
    }
}
