use espy_ears::{Action, Binding, Block, Expression, Node, Statement};
use espy_eyes::Token;
use std::collections::HashMap;
use std::iter;

pub mod instruction {
    // Stack primatives: 0x00-0x0F
    pub const CLONE: u8 = 0x00;
    pub const WRITE: u8 = 0x01;
    pub const COLLAPSE: u8 = 0x02;
    pub const JUMP: u8 = 0x03;

    // Push ops: 0x10-0x2F
    pub const PUSH_UNIT: u8 = 0x10;
    pub const PUSH_I64: u8 = 0x11;

    // Arithmatic ops: 0x30-0x4F
    pub const ADD: u8 = 0x30;
    pub const SUB: u8 = 0x31;
    pub const MUL: u8 = 0x32;
    pub const DIV: u8 = 0x33;
}

pub type StackPointer = u32;
pub type BlockId = u32;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Instruction {
    /// Copy a value from the given position and put it on the top of the stack.
    Clone(StackPointer),
    /// Pop a value off the stack and write it to the given position.
    Write(StackPointer),
    /// Pop a value off the stack and write it to the given position.
    /// Then, pop each value succeeding this position and discard it.
    Collapse(StackPointer),
    /// Using the current stack,
    /// jump to the given block id and execute it.
    Jump(BlockId),

    PushUnit,
    PushI64(i64),

    Add,
    Sub,
    Mul,
    Div,
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
            Instruction::Collapse(to) => decompose!(instruction::COLLAPSE, to as 1..=4),
            Instruction::Jump(block_id) => decompose!(instruction::JUMP, block_id as 1..=4),

            Instruction::PushUnit => decompose!(instruction::PUSH_UNIT,),
            Instruction::PushI64(literal) => decompose!(instruction::PUSH_I64, literal as 1..=8),

            Instruction::Add => decompose!(instruction::ADD,),
            Instruction::Sub => decompose!(instruction::SUB,),
            Instruction::Mul => decompose!(instruction::MUL,),
            Instruction::Div => decompose!(instruction::DIV,),
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
    blocks: Vec<Vec<Instruction>>,
}

impl Program {
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
            output.extend(block.into_iter().flatten());
        }
        output
    }

    fn add_block<'source>(&mut self, block: Block<'source>) {
        let block_id = self.blocks.len();
        self.blocks.push(Vec::new());
        let mut scope = Scope::default();
        for i in block.statements {
            let statement = self.add_statement(i, &mut scope);
            self.blocks[block_id].extend(statement);
        }
        match block.result {
            espy_ears::BlockResult::Expression(i) => {
                let expression = self.add_expression(i, &scope);
                self.blocks[block_id].extend(expression);
            }
            espy_ears::BlockResult::Function(_) => todo!(),
        }
    }

    /// # Panic
    ///
    /// Panics if provided an invalid statement.
    /// Statements should be validated beforehand by checking the `diagnostics.errors` field.
    fn add_statement<'source>(
        &mut self,
        statement: Statement<'source>,
        scope: &mut Scope<'source>,
    ) -> Vec<Instruction> {
        match statement.action {
            Some(Action::Binding(Binding { ident_token, .. })) => {
                // Expressions always put *one* item on the stack, so protect it by advancing sp.
                scope.bindings.insert(
                    ident_token.expect("invalid statement structure").origin,
                    Value {
                        index: scope.stack_pointer,
                    },
                );
                scope.stack_pointer += 1;
            }
            Some(Action::Break(_)) => todo!(),
            None => todo!(),
        }
        self.add_expression(statement.expression.unwrap_or_default(), scope)
    }

    fn add_expression(&mut self, expression: Expression, scope: &Scope) -> Vec<Instruction> {
        if expression.contents.is_empty() {
            return vec![Instruction::PushUnit];
        }
        expression
            .contents
            .into_iter()
            .map(|node| {
                match node {
                    Node::Unit => Instruction::PushUnit,
                    Node::Number(Token { origin, .. }) => {
                        // TODO: Must be handled.
                        let integer = origin.parse().expect("invalid i64");
                        Instruction::PushI64(integer)
                    }
                    Node::Ident(Token { origin, .. }) => {
                        // TODO: Must be handled.
                        let value = scope.bindings.get(origin).expect("undefined symbol");
                        Instruction::Clone(value.index)
                    }
                    Node::Add(_) => Instruction::Add,
                    Node::Sub(_) => Instruction::Sub,
                    Node::Mul(_) => Instruction::Mul,
                    Node::Div(_) => Instruction::Div,
                    Node::Block(block) => {
                        let new_block = self.blocks.len();
                        self.add_block(block);
                        // TODO: Must be handled.
                        Instruction::Jump(new_block.try_into().expect("block limit reached"))
                    }
                    _ => todo!(),
                }
            })
            .collect()
    }
}

impl<'source> From<Block<'source>> for Program {
    fn from(block: Block<'source>) -> Self {
        let mut this = Self::default();
        this.add_block(block);
        this
    }
}

pub struct Value {
    index: StackPointer,
}

#[derive(Default)]
pub struct Scope<'source> {
    stack_pointer: StackPointer,
    bindings: HashMap<&'source str, Value>,
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
            4u32,
            // block 0
            instruction::PUSH_I64,
            1i64,
            instruction::PUSH_I64,
            2i64,
            instruction::ADD,
            instruction::CLONE,
            0u32,
            instruction::PUSH_I64,
            3i64,
            instruction::MUL,
            instruction::CLONE,
            0u32,
            instruction::CLONE,
            1u32,
            instruction::SUB,
        ];
        assert_eq!(actual, expected);
    }

    #[test]
    fn simple_blocks() {
        let mut lexer = Lexer::from("{ 1 + 2 }").peekable();
        let block = Block::from(&mut lexer);
        let program = Program::from(block);
        let actual = program.compile();
        let expected = program![
            8u32,
            13u32,
            // block 0
            instruction::JUMP,
            1 as BlockId,
            // block 1
            instruction::PUSH_I64,
            1i64,
            instruction::PUSH_I64,
            2i64,
            instruction::ADD,
        ];
        assert_eq!(actual, expected);
    }
}
