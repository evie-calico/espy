use super::*;
use Instruction::*;
use espy_eyes::Lexer;

// espy assembly parser :3
macro_rules! program {
    [
        $(
            let $s:ident = $slit:literal;
        )*
        $(fn $block:ident {
            $($i:expr,)*
        })*
    ] => {
        {
            let mut program = Vec::new();
            program.extend([0; 4]);
            program.extend([0; 4]);
            let block_count = program.len();
            let mut i: BlockId = 0;
            $(
                let $block = i;
                program.extend([0; 4]);
                i += 1;
            )*
            program[0..4].copy_from_slice(&i.to_le_bytes());

            #[allow(unused)]
            let string_count = program.len();
            #[allow(unused)]
            let mut i: BlockId = 0;
            $(
                let $s = i;
                program.extend([0; 4]);
                i += 1;
            )*
            program[4..8].copy_from_slice(&i.to_le_bytes());

            let mut i = 0;
            $(
                #[allow(clippy::cast_possible_truncation)]
                let offset = program.len() as u32;
                $(program.extend($i);)*
                program[(block_count + i * size_of::<u32>())..(block_count + (i + 1) * size_of::<u32>())]
                    .copy_from_slice(&offset.to_le_bytes());
                #[allow(unused_assignments)]
                { i += 1; }
            )*
            #[allow(unused)]
            let mut i = 0;
            $(
                #[allow(clippy::cast_possible_truncation)]
                let offset = program.len() as u32;
                program.extend($slit.bytes());
                program[(string_count + i * size_of::<u32>())..(string_count + (i + 1) * size_of::<u32>())]
                    .copy_from_slice(&offset.to_le_bytes());
                #[allow(unused_assignments)]
                { i += 1; }
            )*
            program
        }
    }
}

#[test]
fn variables_and_arithmetic() {
    let mut lexer = Lexer::from("let x = 1 + 2; let y = x * 3; x - y").peekable();
    let block = Block::new(&mut lexer);
    let program = Program::try_from(block).unwrap();
    let actual = program.compile().unwrap();
    let expected = program![
        fn _main {
            PushI64(1),
            PushI64(2),
            Add,
            Clone(0),
            PushI64(3),
            Mul,
            Clone(0),
            Clone(1),
            Sub,
        }
    ];
    assert_eq!(actual, expected);
}

#[test]
fn simple_blocks() {
    let mut lexer = Lexer::from("let x = 2; 1 + { let y = 3; x * y }").peekable();
    let block = Block::new(&mut lexer);
    let program = Program::try_from(block).unwrap();
    let actual = program.compile().unwrap();
    let expected = program![
        fn _main {
            PushI64(2i64),
            PushI64(1i64),
            PushI64(3i64),
            Clone(0),
            Clone(2),
            Mul,
            Collapse(2),
            Add,
        }
    ];
    assert_eq!(actual, expected);
}

#[test]
fn function_creation() {
    let mut lexer = Lexer::from("let x = 2; with y; x * y").peekable();
    let block = Block::new(&mut lexer);
    let program = Program::try_from(block).unwrap();
    let actual = program.compile().unwrap();
    let expected = program![
        fn _main {
            PushI64(2i64),
            Clone(builtins::ANY),
            Clone(builtins::ANY),
            PushFunction {
                captures: 1,
                function: f,
            },
        }
        fn f {
            Clone(0),
            Clone(1),
            Mul,
        }
    ];
    assert_eq!(actual, expected);
}

#[test]
fn function_usage() {
    let mut lexer = Lexer::from("let f = {with x; x * x}; f 2").peekable();
    let block = Block::new(&mut lexer);
    let program = Program::try_from(block).unwrap();
    let actual = program.compile().unwrap();
    let expected = program![
        fn _main {
            Clone(builtins::ANY),
            Clone(builtins::ANY),
            PushFunction {
                captures: 0,
                function: f,
            },
            Clone(0),
            PushI64(2),
            Call,
        }
        fn f {
            Clone(0),
            Clone(0),
            Mul,
        }
    ];
    assert_eq!(actual, expected);
}

#[test]
fn if_expression() {
    let mut lexer = Lexer::from("if true then 1 else then 2 end").peekable();
    let block = Block::new(&mut lexer);
    let program = Program::try_from(block).unwrap();
    let actual = program.compile().unwrap();
    let expected = program![
        fn _main {
            PushTrue,
            If(20),
            PushI64(1i64),
            Jump(29),
            PushI64(2i64),
        }
    ];
    assert_eq!(actual, expected);
}

#[test]
fn option_enum() {
    let mut lexer = Lexer::from("let Option = enum Some: any, None: () end;").peekable();
    let block = Block::new(&mut lexer);
    let program = Program::try_from(block).unwrap();
    let actual = program.compile().unwrap();
    let expected = program![
        let some = "Some";
        let none = "None";
        fn _main {
            Clone(builtins::ANY),
            Name(some),
            PushUnit,
            Name(none),
            Tuple,
            PushEnum,
            PushUnit,
        }
    ];
    assert_eq!(actual, expected);
}

#[test]
fn tuple_indexing() {
    let mut lexer = Lexer::from("let x = 1, 2; x.1").peekable();
    let block = Block::new(&mut lexer);
    let program = Program::try_from(block).unwrap();
    let actual = program.compile().unwrap();
    let expected = program![
        fn _main {
            PushI64(1i64),
            PushI64(2i64),
            Tuple,
            Clone(0),
            PushI64(1i64),
            Index,
        }
    ];
    assert_eq!(actual, expected);
}

#[test]
fn named_tuple_indexing() {
    let mut lexer = Lexer::from("let x = first: 1, second: 2; x.second").peekable();
    let block = Block::new(&mut lexer);
    let program = Program::try_from(block).unwrap();
    let actual = program.compile().unwrap();
    let expected = program![
        let first = "first";
        let second = "second";
        fn _main {
            PushI64(1i64),
            Name(first),
            PushI64(2i64),
            Name(second),
            Tuple,
            Clone(0),
            PushString(second),
            Index,
        }
    ];
    assert_eq!(actual, expected);
}

#[test]
fn builtins() {
    let mut lexer = Lexer::from("let OptionI64 = option i64; OptionI64.Some 1").peekable();
    let block = Block::new(&mut lexer);
    let program = Program::try_from(block).unwrap();
    let actual = program.compile().unwrap();
    let expected = program! {
        let some = "Some";
        fn _main {
            Clone(builtins::OPTION),
            Clone(builtins::I64),
            Call,
            Clone(0),
            PushString(some),
            Index,
            PushI64(1),
            Call,
        }
    };
    assert_eq!(actual, expected);
}

#[test]
fn string() {
    let mut lexer = Lexer::from("\"string\"").peekable();
    let block = Block::new(&mut lexer);
    let program = Program::try_from(block).unwrap();
    let actual = program.compile().unwrap();
    let expected = program! {
        let string = "string";
        fn _main {
            PushString(string),
        }
    };
    assert_eq!(actual, expected);
}
