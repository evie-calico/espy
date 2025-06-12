use espy_ears::{Action, Binding, Block, Expression, Node, Statement};
use espy_eyes::Token;
use std::collections::HashMap;
use std::iter;

pub mod instruction {
    // Stack primatives: 0x00-0x0F
    pub const CLONE: u8 = 0x00;
    pub const WRITE: u8 = 0x01;

    // Push ops: 0x10-0x2F
    pub const PUSH_UNIT: u8 = 0x10;
    pub const PUSH_I64: u8 = 0x11;

    // Arithmatic ops: 0x30-0x4F
    pub const ADD: u8 = 0x30;
    pub const SUB: u8 = 0x31;
    pub const MUL: u8 = 0x32;
    pub const DIV: u8 = 0x33;
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Instruction {
    /// Copy a value from the given position and put it on the top of the stack.
    Clone(u32),
    /// Pop a value off the stack and write it to the given position.
    Write(u32),

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
}

impl<'source> From<Block<'source>> for Program {
    fn from(block: Block<'source>) -> Self {
        let mut scope = Scope::default();
        let mut root_block = Vec::new();
        for i in block.statements {
            root_block.extend(statement(i, &mut scope));
        }
        match block.result {
            espy_ears::BlockResult::Expression(i) => root_block.extend(expression(i, &scope)),
            espy_ears::BlockResult::Function(_) => todo!(),
        }
        Program {
            blocks: vec![root_block],
        }
    }
}

pub struct Value {
    index: u32,
}

#[derive(Default)]
pub struct Scope<'source> {
    stack_pointer: u32,
    bindings: HashMap<&'source str, Value>,
}

/// # Panic
///
/// Panics if provided an invalid statement.
/// Statements should be validated beforehand by checking the `diagnostics.errors` field.
fn statement<'source, 'scope>(
    statement: Statement<'source>,
    scope: &'scope mut Scope<'source>,
) -> impl Iterator<Item = Instruction> + 'scope {
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
    expression(statement.expression.unwrap_or_default(), scope)
}

fn expression(expression: Expression, scope: &Scope) -> impl Iterator<Item = Instruction> {
    assert!(
        expression.diagnostics.errors.is_empty(),
        "expression must be valid"
    );
    if expression.contents.is_empty() {
        Some(Instruction::PushUnit)
    } else {
        None
    }
    .into_iter()
    .chain(expression.contents.into_iter().flat_map(|node| {
        match node {
            Node::Unit => Some(Instruction::PushUnit),
            Node::Number(Token { origin, .. }) => {
                // TODO: Must be handled.
                let integer = origin.parse().expect("invalid i64");
                Some(Instruction::PushI64(integer))
            }
            Node::Ident(Token { origin, .. }) => {
                // TODO: Must be handled.
                let value = scope.bindings.get(origin).expect("undefined symbol");
                Some(Instruction::Clone(value.index))
            }
            Node::Add(_) => Some(Instruction::Add),
            Node::Sub(_) => Some(Instruction::Sub),
            Node::Mul(_) => Some(Instruction::Mul),
            Node::Div(_) => Some(Instruction::Div),
            _ => todo!(),
        }
    }))
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
}
