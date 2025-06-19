use espy_ears::{Action, Block, Expression, If, Node, Statement};
use espy_eyes::{Lexigram, Token};
use std::iter;

#[cfg(test)]
mod tests;

// TODO: Reorder these before release.
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
    pub const PUSH_ENUM: u8 = 0x15;
    pub const PUSH_STRING: u8 = 0x16;

    // Operations: 0x30-0x4F
    pub const ADD: u8 = 0x30;
    pub const SUB: u8 = 0x31;
    pub const MUL: u8 = 0x32;
    pub const DIV: u8 = 0x33;
    pub const CALL: u8 = 0x34;
    pub const INDEX: u8 = 0x35;
    pub const TUPLE: u8 = 0x36;
    pub const NAME: u8 = 0x37;
}

pub type ArchWidth = u32;
pub type ProgramCounter = ArchWidth;
pub type StackPointer = ArchWidth;
pub type BlockId = ArchWidth;
pub type StringId = ArchWidth;
pub type StringSet = ArchWidth;

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
    /// Pop the top value off the stack;
    /// it should be unit or a tamed tuple of functions.
    /// Use this value as the enum's methods.
    ///
    /// Pop the top `captures` values off the stack;
    /// these should be types.
    /// Use these as the types contained within each of the enum's variants.
    ///
    /// The names of each variant can be found in the string set `names`.
    ///
    /// Push the resulting enum type to the stack.
    PushEnum {
        captures: StackPointer,
        names: BlockId,
    },
    PushString(StringId),

    Add,
    Sub,
    Mul,
    Div,
    /// Pop the top value off the stack and push it to the function's stack.
    /// The next value is the function to be called.
    /// After pushing the value, jump to the function's block id.
    /// It will return a single value which is placed to the stack in its place.
    Call,
    /// Pop the first value off the stack, then pop the second and index it using the first.
    /// Push the result.
    Index,
    /// Pop two values off the stack and combine them into a tuple.
    /// If one of them is already a tuple, concatenate them (always producing a flat tuple).
    /// The topmost value should be appended to the value underneath it.
    Tuple,
    /// Pop a value off the stack and turn it into a named tuple with a single value,
    /// and a name according to the following string id.
    Name(StringId),
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
            Instruction::PushEnum { captures, names } => {
                decompose!(instruction::PUSH_ENUM, captures as 1..=4, names as 5..=8)
            }
            Instruction::PushString(s) => decompose!(instruction::PUSH_STRING, s as 1..=4),

            Instruction::Add => decompose!(instruction::ADD,),
            Instruction::Sub => decompose!(instruction::SUB,),
            Instruction::Mul => decompose!(instruction::MUL,),
            Instruction::Div => decompose!(instruction::DIV,),
            Instruction::Call => decompose!(instruction::CALL,),
            Instruction::Index => decompose!(instruction::INDEX,),
            Instruction::Tuple => decompose!(instruction::TUPLE,),
            Instruction::Name(name) => decompose!(instruction::NAME, name as 1..=4),
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
struct Program<'source> {
    blocks: Vec<Vec<u8>>,
    strings: Vec<&'source str>,
    string_sets: Vec<Vec<StringId>>,
}

impl<'source> Program<'source> {
    // Remove this when defining the public API.
    #[allow(dead_code, reason = "currently only used for tests")]
    fn compile(self) -> Vec<u8> {
        let mut output = Vec::new();
        // Reserve space for vector offsets.
        // Blocks, strings, and string sets are only referred to by index,
        // so this is the only retroactive filling required.
        // It would be possible to maintain a much smaller vector
        // (and lazily produce the program's bytecode)
        // by putting this at the end of the program.
        output.extend((self.blocks.len() as u32).to_le_bytes());
        let block_offsets = output.len();
        output.extend(iter::repeat_n(0, self.blocks.len() * size_of::<u32>()));
        // same for strings.
        output.extend((self.strings.len() as u32).to_le_bytes());
        let string_offsets = output.len();
        output.extend(iter::repeat_n(0, self.strings.len() * size_of::<u32>()));
        // and string sets (these are currently only used for enums)
        output.extend((self.string_sets.len() as u32).to_le_bytes());
        let string_set_offsets = output.len();
        output.extend(iter::repeat_n(0, self.string_sets.len() * size_of::<u32>()));

        for (block_id, block) in self.blocks.into_iter().enumerate() {
            // Fill in offset.
            let src = output.len() as u32;
            let dest = block_offsets + block_id * size_of::<u32>();
            output[dest..(dest + size_of::<u32>())].copy_from_slice(&src.to_le_bytes());
            output.extend(block);
        }

        for (string_id, string) in self.strings.into_iter().enumerate() {
            // Fill in offset.
            let src = output.len() as u32;
            let dest = string_offsets + string_id * size_of::<u32>();
            output[dest..(dest + size_of::<u32>())].copy_from_slice(&src.to_le_bytes());
            output.extend(string.bytes());
        }

        for (string_set_id, string_set) in self.string_sets.into_iter().enumerate() {
            // Fill in offset.
            let src = output.len() as u32;
            let dest = string_set_offsets + string_set_id * size_of::<u32>();
            output[dest..(dest + size_of::<u32>())].copy_from_slice(&src.to_le_bytes());
            output.extend(string_set.into_iter().flat_map(|x| x.to_le_bytes()));
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

    fn create_string(&mut self, s: &'source str) -> StringId {
        if let Some((i, _)) = self.strings.iter().enumerate().find(|(_, x)| **x == s) {
            i as StringId
        } else {
            self.strings.push(s);
            self.strings.len() as StringId - 1
        }
    }

    fn add_block(
        &mut self,
        block_id: BlockId,
        block: Block<'source>,
        mut scope: Scope<'_, 'source>,
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
    fn add_statement(
        &mut self,
        block_id: BlockId,
        statement: Statement<'source>,
        scope: &mut Scope<'_, 'source>,
    ) {
        match statement.action {
            Action::Evaluate(binding, expression) => {
                // If the binding exists but not an expression, we need to generate a unit value.
                if expression.is_some() || binding.is_some() {
                    self.add_expression(block_id, expression.unwrap_or_default(), scope);
                }
                if let Some(binding) = binding {
                    scope.insert(
                        binding
                            .ident_token
                            .expect("invalid statement structure")
                            .origin,
                    );
                } else {
                    self.blocks[block_id as usize].extend(Instruction::Pop)
                }
            }
            Action::Break(_, _expression) => todo!(),
            Action::Implementation(_) => todo!(),
        }
    }

    fn add_expression(
        &mut self,
        block_id: BlockId,
        expression: Expression<'source>,
        scope: &mut Scope<'_, 'source>,
    ) {
        // Shortcut for re-indexing self.blocks.
        // This is necessary because add_block etc take &mut self.
        macro_rules! block {
            () => {
                self.blocks[block_id as usize]
            };
        }
        if expression.contents.is_empty() {
            scope.stack_pointer += 1;
            block!().extend(Instruction::PushUnit);
            return;
        }
        for node in expression.contents {
            match node {
                Node::Unit => {
                    scope.stack_pointer += 1;
                    block!().extend(Instruction::PushUnit)
                }
                Node::Number(Token { origin, .. }) => {
                    // TODO: Must be handled.
                    let integer = origin.parse().expect("invalid i64");
                    scope.stack_pointer += 1;
                    block!().extend(Instruction::PushI64(integer))
                }
                Node::Variable(Token { origin, .. }) => {
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
                Node::Tuple(_) => {
                    scope.stack_pointer -= 1;
                    block!().extend(Instruction::Tuple)
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
                Node::Name {
                    name,
                    colon_token: _,
                } => {
                    // this is unary and does not move the stack.
                    scope.stack_pointer += 0;
                    let s = self.create_string(name.origin);
                    block!().extend(Instruction::Name(s));
                }
                Node::Field {
                    dot_token: _,
                    index,
                } => {
                    match index {
                        Token {
                            lexigram: Lexigram::Ident,
                            origin,
                        } => {
                            let s = self.create_string(origin);
                            block!().extend(Instruction::PushString(s));
                        }
                        Token {
                            lexigram: Lexigram::Number,
                            origin,
                        } => {
                            // TODO: Must be handled.
                            let integer = origin.parse().expect("invalid i64");
                            block!().extend(Instruction::PushI64(integer));
                        }
                        _ => {
                            panic!("expected an identifier or number in field index, got {index:?}")
                        }
                    }
                    // the stack pointer does not move by the end,
                    // because while Index pops twice, we performed one of the pushes ourselves.
                    scope.stack_pointer += 0;
                    block!().extend(Instruction::Index)
                }
                Node::Struct(_) => todo!(),
                Node::Enum(enumeration) => {
                    let mut child = scope.child();
                    for i in enumeration.block.statements {
                        self.add_statement(block_id, i, &mut child);
                    }
                    match enumeration.block.result {
                        // Eventually this value will be the enum's methods. Force unit for now.
                        espy_ears::BlockResult::Expression(expression) => {
                            self.add_expression(block_id, expression, &mut child);
                        }
                        // This is a semantic error,
                        // but interestingly enough it's also not possible to move the scope into `promote` here,
                        // since enums capture locals to use as their field names.
                        // TODO: must be handled
                        espy_ears::BlockResult::Function(_) => {
                            panic!("enum block may not result in a function")
                        }
                    }
                    let set = child
                        .bindings
                        .into_iter()
                        .map(|(case, _)| self.create_string(case))
                        .collect();
                    let names = self.string_sets.len() as StringSet;
                    self.string_sets.push(set);
                    // -1 to account for the block result (the enum's methods)
                    let captures = child.stack_pointer - scope.stack_pointer - 1;
                    self.blocks[block_id as usize]
                        .extend(Instruction::PushEnum { captures, names });
                    scope.stack_pointer += 1;
                }
                Node::Match(_) => todo!(),
            };
        }
    }
}

impl<'source> From<Block<'source>> for Program<'source> {
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
pub struct Scope<'parent, 'source> {
    parent: Option<&'parent Scope<'parent, 'source>>,
    stack_pointer: StackPointer,
    bindings: Vec<(&'source str, Value)>,
}

impl<'parent, 'source> Scope<'parent, 'source> {
    fn get(&self, k: &'source str) -> Option<Value> {
        self.bindings
            .iter()
            .find(|(x, _)| *x == k)
            .copied()
            .map(|(_, x)| x)
            .or_else(|| self.parent.and_then(|parent| parent.get(k)))
    }

    fn insert(&mut self, k: &'source str) {
        self.bindings.push((
            k,
            Value {
                index: self
                    .stack_pointer
                    .checked_sub(1)
                    .expect("attempted to assign variable while stack was empty"),
            },
        ));
    }

    fn child(&'parent self) -> Self {
        Self {
            parent: Some(self),
            stack_pointer: self.stack_pointer,
            bindings: Vec::new(),
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
