use super::*;
use Instruction::*;
use espy_eyes::Lexer;

// espyscript assembly parser :3
macro_rules! program {
    [
        $(
            let $s:ident = $slit:literal;
        )*
        $(
            enum $e:ident = [$($case:expr),* $(,)?];
        )*
        $(fn $block:ident {
            $($i:expr,)*
        })*
    ] => {
        {
            let mut program = Vec::new();
            let mut i: BlockId = 0;
            $(
                let $block = i;
                program.extend([0; 4]);
                i += 1;
            )*
            for b in i.to_le_bytes().into_iter().rev() {
                program.insert(0, b);
            }

            let string_count = program.len();
            #[allow(unused)]
            let mut i: BlockId = 0;
            $(
                let $s = i;
                program.extend([0; 4]);
                i += 1;
            )*
            for b in i.to_le_bytes().into_iter().rev() {
                program.insert(string_count, b);
            }

            let enum_count = program.len();
            #[allow(unused)]
            let mut i: BlockId = 0;
            $(
                let $e = i;
                program.extend([0; 4]);
                i += 1;
            )*
            for b in i.to_le_bytes().into_iter().rev() {
                program.insert(enum_count, b);
            }

            let mut i = 1;
            $(
                let offset = program.len() as u32;
                $(program.extend($i);)*
                program[(i * size_of::<u32>())..((i + 1) * size_of::<u32>())]
                    .copy_from_slice(&offset.to_le_bytes());
                #[allow(unused_assignments)]
                { i += 1; }
            )*
            #[allow(unused)]
            let mut i = 1;
            $(
                let offset = program.len() as u32;
                program.extend($slit.bytes());
                program[(string_count + i * size_of::<u32>())..(string_count + (i + 1) * size_of::<u32>())]
                    .copy_from_slice(&offset.to_le_bytes());
                #[allow(unused_assignments)]
                { i += 1; }
            )*
            #[allow(unused)]
            let mut i = 1;
            $(
                let offset = program.len() as u32;
                $(program.extend($case.to_le_bytes());)*
                program[(enum_count + i * size_of::<u32>())..(enum_count + (i + 1) * size_of::<u32>())]
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
    let block = Block::from(&mut lexer);
    let program = Program::try_from(block).unwrap();
    let actual = program.compile();
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
    let block = Block::from(&mut lexer);
    let program = Program::try_from(block).unwrap();
    let actual = program.compile();
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
    let block = Block::from(&mut lexer);
    let program = Program::try_from(block).unwrap();
    let actual = program.compile();
    let expected = program![
        fn _main {
            PushI64(2i64),
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
    let block = Block::from(&mut lexer);
    let program = Program::try_from(block).unwrap();
    let actual = program.compile();
    let expected = program![
        fn _main {
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
    let block = Block::from(&mut lexer);
    let program = Program::try_from(block).unwrap();
    let actual = program.compile();
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
    let mut lexer =
        Lexer::from("let Option = enum then let Some = (); let None = (); end;").peekable();
    let block = Block::from(&mut lexer);
    let program = Program::try_from(block).unwrap();
    let actual = program.compile();
    let expected = program![
        let none = "None";
        let some = "Some";
        enum option = [none, some];
        fn _main {
            PushUnit,
            PushUnit,
            PushUnit,
            PushEnum {
                names: option,
            },
            PushUnit,
        }
    ];
    assert_eq!(actual, expected);
}

#[test]
fn tuple_indexing() {
    let mut lexer = Lexer::from("let x = 1, 2; x.1").peekable();
    let block = Block::from(&mut lexer);
    let program = Program::try_from(block).unwrap();
    let actual = program.compile();
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
    let block = Block::from(&mut lexer);
    let program = Program::try_from(block).unwrap();
    let actual = program.compile();
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
    let mut lexer = Lexer::from("Option.Some 1; None ()").peekable();
    let block = Block::from(&mut lexer);
    let program = Program::try_from(block).unwrap();
    let actual = program.compile();
    let expected = program! {
        let some = "Some";
        fn _main {
            Clone(builtins::OPTION),
            PushString(some),
            Index,
            PushI64(1),
            Call,
            Pop,
            Clone(builtins::NONE),
            PushUnit,
            Call,
        }
    };
    assert_eq!(actual, expected);
}

#[test]
fn invalid_enum() {
    let mut lexer = Lexer::from("enum then break 1 end").peekable();
    let block = Block::from(&mut lexer);
    let actual = Program::try_from(block);
    if !matches!(actual, Err(Error::UnexpectedEnumResult)) {
        panic!(
            "actual: {actual:?}\nexpected: Err({:?})",
            Error::UnexpectedEnumResult
        );
    }
}

#[test]
fn for_loop() {
    let mut lexer = Lexer::from("for i in 5 then Some i end;").peekable();
    let block = Block::from(&mut lexer);
    let program = Program::try_from(block).unwrap();
    let actual = program.compile();
    let expected = program! {
        fn _main {
            PushI64(5),
            For(31),
            Clone(builtins::SOME),
            Clone(1),
            Call,
            Pop,
            Jump(9),
            Pop,
            PushUnit,
        }
    };
    assert_eq!(actual, expected);
}

#[test]
fn for_with_break() {
    let mut lexer = Lexer::from("for i in 5 then break end;").peekable();
    let block = Block::from(&mut lexer);
    let program = Program::try_from(block).unwrap();
    let actual = program.compile();
    let expected = program! {
        fn _main {
            PushI64(5),
            For(26),
            PushUnit,
            Jump(26),
            Pop,
            Jump(9),
            Pop,
            PushUnit,
        }
    };
    assert_eq!(actual, expected);
}
