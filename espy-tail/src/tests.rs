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
        1u32, // block count
        16 as BlockId,
        0u32, // string count
        0u32, // string set count
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
        1u32, // block count
        16 as BlockId,
        0u32, // string count
        0u32, // string set count
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
        2u32, // block count
        20 as BlockId,
        38 as BlockId,
        0u32, // string count
        0u32, // string set count
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
        2u32, // block count
        20 as BlockId,
        44 as BlockId,
        0u32, // string count
        0u32, // string set count
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
        1u32, // block count
        16 as BlockId,
        0u32, // string count
        0u32, // string set count
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

#[test]
fn option_enum() {
    let mut lexer =
        Lexer::from("let Option = enum then let Some = (); let None = (); end;").peekable();
    let block = Block::from(&mut lexer);
    let program = Program::from(block);
    let actual = program.compile();
    let mut expected = program![
        1u32, // block count
        28 as BlockId,
        2u32, // string count
        41 as StringId,
        45 as StringId,
        1u32, // string set count
        49 as StringSet,
        // block 0
        instruction::PUSH_UNIT,
        instruction::PUSH_UNIT,
        instruction::PUSH_UNIT,
        instruction::PUSH_ENUM,
        2 as StackPointer, // two cases
        0 as StringSet,    // first string set
        instruction::PUSH_UNIT,
    ];
    expected.extend("Some".bytes());
    expected.extend("None".bytes());
    expected.extend(0u32.to_le_bytes());
    expected.extend(1u32.to_le_bytes());
    assert_eq!(actual, expected);
}

#[test]
fn tuple_indexing() {
    let mut lexer = Lexer::from("let x = 1, 2; x.1").peekable();
    let block = Block::from(&mut lexer);
    let program = Program::from(block);
    let actual = program.compile();
    let expected = program![
        1u32, // block count
        16 as BlockId,
        0u32, // string count
        0u32, // string set count
        // block 0
        instruction::PUSH_I64,
        1i64,
        instruction::PUSH_I64,
        2i64,
        instruction::TUPLE,
        instruction::CLONE,
        0 as StackPointer,
        instruction::PUSH_I64,
        1i64,
        instruction::INDEX,
    ];
    assert_eq!(actual, expected);
}

#[test]
fn named_tuple_indexing() {
    let mut lexer = Lexer::from("let x = first: 1, second: 2; x.second").peekable();
    let block = Block::from(&mut lexer);
    let program = Program::from(block);
    let actual = program.compile();
    let mut expected = program![
        1u32, // block count
        24 as BlockId,
        2u32,           // string count
        64 as StringId, // "first"
        69 as StringId, // "second"
        0u32,           // string set count
        // block 0
        instruction::PUSH_I64,
        1i64,
        instruction::NAME,
        0 as StringId, // "first"
        instruction::PUSH_I64,
        2i64,
        instruction::NAME,
        1 as StringId, // "second"
        instruction::TUPLE,
        instruction::CLONE,
        0 as StackPointer,
        instruction::PUSH_STRING,
        1 as StringId, // "second"
        instruction::INDEX,
    ];
    expected.extend("first".bytes());
    expected.extend("second".bytes());
    assert_eq!(actual, expected);
}
