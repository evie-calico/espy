use crate::*;

macro_rules! token {
    ($name:ident: $lexigram:ident = $origin:literal) => {
        const $name: Token = Token {
            origin: $origin,
            lexigram: Lexigram::$lexigram,
        };
    };
}

token!(COLON: Colon = ":");
token!(DOT: Dot = ".");
token!(ELSE: Else = "else");
token!(END: End = "end");
token!(ENUM: Enum = "enum");
token!(FOR: For = "for");
token!(IF: If = "if");
token!(IMPL: Impl = "impl");
token!(IN: In = "in");
token!(LET: Let = "let");
token!(MATCH: Match = "match");
token!(OPEN_PAREN: OpenParen = "(");
token!(STRUCT: Struct = "struct");
token!(THEN: Then = "then");
token!(WITH: With = "with");
token!(DOUBLE_ARROW: DoubleArrow = "=>");
token!(SEMICOLON: Semicolon = ";");
token!(SINGLE_EQUAL: SingleEqual = "=");

fn ident<'source>(origin: &'source str) -> Token<'source> {
    Token {
        origin,
        lexigram: Lexigram::Ident,
    }
}

fn number<'source>(origin: &'source str) -> Token<'source> {
    Token {
        origin,
        lexigram: Lexigram::Number,
    }
}

macro_rules! node {
    ($name:ident: $node:ident = $origin:literal as $lexigram:ident) => {
        const $name: Node = Node::$node(Some(Token {
            origin: $origin,
            lexigram: Lexigram::$lexigram,
        }));
    };
}

fn number_node<'source>(origin: &'source str) -> Node<'source> {
    Node::Number(number(origin))
}

fn variable<'source>(origin: &'source str) -> Node<'source> {
    Node::Variable(ident(origin))
}

node!(PIPE: Pipe = "|>" as Triangle);
node!(MUL: Mul = "*" as Star);
node!(BITWISE_AND: BitwiseAnd = "&" as Ampersand);
node!(BITWISE_XOR: BitwiseXor = "^" as Caret);
node!(BITWISE_OR: BitwiseOr = "|" as Pipe);
node!(EQUAL_TO: EqualTo = "==" as DoubleEqual);
node!(NOT_EQUAL_TO: NotEqualTo = "!=" as BangEqual);
node!(GREATER: Greater = ">" as Greater);
node!(GREATER_EQUAL: GreaterEqual = ">=" as GreaterEqual);
node!(LESSER: Lesser = "<" as Lesser);
node!(LESSER_EQUAL: LesserEqual = "<=" as LesserEqual);
node!(TUPLE: Tuple = "," as Comma);

fn binding<'source>(
    origin: &'source str,
    contents: impl Into<Vec<Node<'source>>>,
) -> Statement<'source> {
    Statement::Evaluation(Evaluation {
        binding: Some(Binding {
            let_token: LET,
            ident_token: Some(ident(origin)),
            equals_token: Some(SINGLE_EQUAL),
        }),
        expression: expression(contents),
        semicolon_token: Some(SEMICOLON),
        diagnostics: Diagnostics::default(),
    })
}

fn evaluation<'source>(contents: impl Into<Vec<Node<'source>>>) -> Statement<'source> {
    Statement::Evaluation(Evaluation {
        binding: None,
        expression: expression(contents),
        semicolon_token: Some(SEMICOLON),
        diagnostics: Diagnostics::default(),
    })
}

fn expression<'source>(contents: impl Into<Vec<Node<'source>>>) -> Expression<'source> {
    Expression {
        contents: contents.into(),
        diagnostics: Diagnostics::default(),
    }
}

#[test]
fn assignment() {
    let source = "let x = 1;";
    let actual = Block::from(&mut Lexer::from(source).peekable());
    let expected = Block {
        statements: vec![binding("x", [number_node("1")])],
        ..Default::default()
    };
    assert_eq!(actual, expected);
}

#[test]
fn tuples() {
    let source = "1 * 2, 3";
    let actual = Block::from(&mut Lexer::from(source).peekable());
    let expected = Block {
        result: expression([
            number_node("1"),
            number_node("2"),
            MUL,
            number_node("3"),
            TUPLE,
        ])
        .into(),
        ..Default::default()
    };
    assert_eq!(actual, expected);
}

#[test]
fn named_tuple() {
    let source = "x: 1, y: 2";
    let actual = Block::from(&mut Lexer::from(source).peekable());
    let expected = Block {
        result: expression([
            number_node("1"),
            Node::Name {
                name: ident("x"),
                colon_token: COLON,
            },
            number_node("2"),
            Node::Name {
                name: ident("y"),
                colon_token: COLON,
            },
            TUPLE,
        ])
        .into(),
        ..Default::default()
    };
    assert_eq!(actual, expected);
}

#[test]
fn block_expression() {
    let source = "let x = { let y = 2; y * 3 };";
    let actual = Block::from(&mut Lexer::from(source).peekable());
    let expected = Block {
        statements: vec![binding(
            "x",
            [Node::Block(Block {
                statements: [binding("y", [number_node("2")])].into(),
                result: expression([variable("y"), number_node("3"), MUL]).into(),
                ..Default::default()
            })],
        )],
        ..Default::default()
    };
    assert_eq!(actual, expected);
}

#[test]
fn if_expression() {
    let source = "let x = if condition then 1 else then 2 end;";
    let actual = Block::from(&mut Lexer::from(source).peekable());
    let expected = Block {
        statements: vec![binding(
            "x",
            [If {
                if_token: Some(IF),
                condition: expression([variable("condition")]),
                then_token: Some(THEN),
                first: Block {
                    result: expression([number_node("1")]).into(),
                    ..Default::default()
                },
                else_token: Some(ELSE),
                else_kind: Some(THEN),
                second: Block {
                    result: expression([number_node("2")]).into(),
                    ..Default::default()
                },
                end_token: Some(END),
                diagnostics: Diagnostics::default(),
            }
            .into()],
        )],
        ..Default::default()
    };
    assert_eq!(actual, expected);
}

#[test]
fn if_else() {
    let source = "let x = if condition then 1 else if other then 2 else then 3 end;";
    let actual = Block::from(&mut Lexer::from(source).peekable());
    let expected = Block {
        statements: vec![binding(
            "x",
            [If {
                if_token: Some(IF),
                condition: expression([variable("condition")]),
                then_token: Some(THEN),
                first: Block {
                    result: expression([number_node("1")]).into(),
                    ..Default::default()
                },
                else_token: Some(ELSE),
                else_kind: Some(IF),
                second: Block {
                    result: expression([If {
                        if_token: Some(IF),
                        condition: expression([variable("other")]),
                        then_token: Some(THEN),
                        first: Block {
                            result: expression([number_node("2")]).into(),
                            ..Default::default()
                        },
                        else_token: Some(ELSE),
                        else_kind: Some(THEN),
                        second: Block {
                            result: expression([number_node("3")]).into(),
                            ..Default::default()
                        },
                        end_token: Some(END),
                        diagnostics: Diagnostics::default(),
                    }
                    .into()])
                    .into(),
                    ..Default::default()
                },
                end_token: Some(END),
                diagnostics: Diagnostics::default(),
            }
            .into()],
        )],
        ..Default::default()
    };
    assert_eq!(actual, expected);
}

#[test]
fn incomplete_expression() {
    let source = "1 * 2,";
    let actual = Block::from(&mut Lexer::from(source).peekable());
    let expected = Block {
        result: Expression {
            contents: vec![number_node("1"), number_node("2"), MUL],
            diagnostics: Diagnostics {
                errors: vec![Error::IncompleteExpression],
            },
        }
        .into(),
        ..Default::default()
    };
    assert_eq!(actual, expected);
}

#[test]
fn malformed_binding() {
    let source = "let x 2";
    let actual = Block::from(&mut Lexer::from(source).peekable());
    let expected = Block {
        // Despite being malformed, this expression is still parsed correctly!
        statements: vec![Statement::Evaluation(Evaluation {
            binding: Some(Binding {
                let_token: LET,
                ident_token: Some(ident("x")),
                equals_token: None,
            }),
            expression: expression([number_node("2")]),
            semicolon_token: None,
            diagnostics: Diagnostics {
                errors: vec![
                    Error::MissingToken {
                        expected: &[Lexigram::SingleEqual],
                        actual: Some(number("2")),
                    },
                    Error::MissingToken {
                        expected: &[Lexigram::Semicolon],
                        actual: None,
                    },
                ],
            },
        })],
        ..Default::default()
    };
    assert_eq!(actual, expected);
}

#[test]
fn for_loop() {
    let source = "for i in iter then print i; end";
    let actual = Block::from(&mut Lexer::from(source).peekable());
    let expected = Block {
        statements: vec![Statement::For(For {
            for_token: FOR,
            binding: Some(ident("i")),
            in_token: Some(IN),
            iterator: expression([variable("iter")]),
            then_token: Some(THEN),
            block: Block {
                statements: vec![evaluation([
                    variable("print"),
                    variable("i"),
                    Node::Call(Some(ident("i"))),
                ])],
                ..Default::default()
            },
            end_token: Some(END),
            diagnostics: Diagnostics::default(),
        })],
        ..Default::default()
    };
    assert_eq!(actual, expected);
}

#[test]
fn reserved_symbol() {
    let source = "let class = 1;";
    let actual = Block::from(&mut Lexer::from(source).peekable());
    let expected = Block {
        statements: vec![Statement::Evaluation(Evaluation {
            binding: Some(Binding {
                let_token: LET,
                // Note that this is an invalid identifier,
                // but we still know the *intent* and can smooth things over for diagnostics.
                ident_token: Some(ident("class")),
                equals_token: Some(SINGLE_EQUAL),
            }),
            expression: expression([number_node("1")]),
            semicolon_token: Some(SEMICOLON),
            diagnostics: Diagnostics {
                errors: vec![Error::Lexer(lexer::Error {
                    origin: "class",
                    kind: lexer::ErrorKind::ReservedSymbol,
                })],
            },
        })],
        ..Default::default()
    };
    assert_eq!(actual, expected);
}

#[test]
fn comparison_operators() {
    let source = "1 == 1; 1 != 1; 1 > 1; 1 >= 1; 1 < 1; 1<= 1;";
    let actual = Block::from(&mut Lexer::from(source).peekable());
    let expected = Block {
        statements: vec![
            evaluation([number_node("1"), number_node("1"), EQUAL_TO]),
            evaluation([number_node("1"), number_node("1"), NOT_EQUAL_TO]),
            evaluation([number_node("1"), number_node("1"), GREATER]),
            evaluation([number_node("1"), number_node("1"), GREATER_EQUAL]),
            evaluation([number_node("1"), number_node("1"), LESSER]),
            evaluation([number_node("1"), number_node("1"), LESSER_EQUAL]),
        ],
        ..Default::default()
    };
    assert_eq!(actual, expected);
}

#[test]
fn bitwise_operators() {
    let source = "1 | 2 & 3 ^ 4";
    let actual = Block::from(&mut Lexer::from(source).peekable());
    let expected = Block {
        result: expression([
            number_node("1"),
            number_node("2"),
            number_node("3"),
            BITWISE_AND,
            number_node("4"),
            BITWISE_XOR,
            BITWISE_OR,
        ])
        .into(),
        ..Default::default()
    };
    assert_eq!(actual, expected);
}

#[test]
fn nested_parens() {
    let source = "1 | (2 & (3 ^ 4))";
    let actual = Block::from(&mut Lexer::from(source).peekable());
    let expected = Block {
        result: expression([
            number_node("1"),
            number_node("2"),
            number_node("3"),
            number_node("4"),
            BITWISE_XOR,
            BITWISE_AND,
            BITWISE_OR,
        ])
        .into(),
        ..Default::default()
    };
    assert_eq!(actual, expected);
}

#[test]
fn pipe_operator() {
    let source = "square 2 |> add 4 |> square ()";
    let actual = Block::from(&mut Lexer::from(source).peekable());
    let expected = Block {
        result: expression([
            variable("square"),
            number_node("2"),
            Node::Call(Some(number("2"))),
            variable("add"),
            PIPE,
            number_node("4"),
            Node::Call(Some(number("4"))),
            variable("square"),
            PIPE,
            Node::Unit,
            Node::Call(Some(OPEN_PAREN)),
        ])
        .into(),
        ..Default::default()
    };
    assert_eq!(actual, expected);
}

#[test]
fn function() {
    let source = "let captured = 1; with x; x * captured";
    let actual = Block::from(&mut Lexer::from(source).peekable());
    let expected = Block {
        statements: vec![binding("captured", [number_node("1")])],
        result: Function {
            with_token: Some(WITH),
            argument: Some(ident("x")),
            semicolon_token: Some(SEMICOLON),
            block: Block {
                result: expression([variable("x"), variable("captured"), MUL]).into(),
                ..Default::default()
            },
            diagnostics: Diagnostics::default(),
        }
        .into(),
        ..Default::default()
    };
    assert_eq!(actual, expected);
}

#[test]
fn structure() {
    let source = "let Coord = struct x: u32, y: u32 then let new = {with pos; pos.0, pos.1}; end;";
    let actual = Block::from(&mut Lexer::from(source).peekable());
    let expected = Block {
        statements: vec![binding(
            "Coord",
            [Node::Struct(Struct {
                struct_token: Some(STRUCT),
                inner: expression([
                    variable("u32"),
                    Node::Name {
                        name: ident("x"),
                        colon_token: COLON,
                    },
                    variable("u32"),
                    Node::Name {
                        name: ident("y"),
                        colon_token: COLON,
                    },
                    TUPLE,
                ]),
                then_token: Some(THEN),
                members: Some(BlockExpression {
                    statements: vec![binding(
                        "new",
                        [Node::Block(Block {
                            result: Function {
                                with_token: Some(WITH),
                                argument: Some(ident("pos")),
                                semicolon_token: Some(SEMICOLON),
                                block: Block {
                                    result: expression([
                                        variable("pos"),
                                        Node::Field {
                                            dot_token: DOT,
                                            index: number("0"),
                                        },
                                        variable("pos"),
                                        Node::Field {
                                            dot_token: DOT,
                                            index: number("1"),
                                        },
                                        TUPLE,
                                    ])
                                    .into(),
                                    ..Default::default()
                                },
                                diagnostics: Diagnostics::default(),
                            }
                            .into(),
                            ..Default::default()
                        })],
                    )],
                    ..Default::default()
                }),
                end_token: Some(END),
                diagnostics: Diagnostics::default(),
            })],
        )],
        ..Default::default()
    };
    assert_eq!(actual, expected);
}

#[test]
fn match_expression() {
    let source = "match 0 then let x = 1 => x * 2; 3 => 4; end";
    let actual = Block::from(&mut Lexer::from(source).peekable());
    let expected = Block {
        result: expression([Match {
            match_token: Some(MATCH),
            expression: expression([number_node("0")]),
            then_token: Some(THEN),
            cases: vec![
                MatchCase {
                    let_token: Some(LET),
                    binding: Some(ident("x")),
                    equals_token: Some(SINGLE_EQUAL),
                    case: expression([number_node("1")]).into(),
                    arrow_token: Some(DOUBLE_ARROW),
                    expression: expression([variable("x"), number_node("2"), MUL]),
                    semicolon_token: Some(SEMICOLON),
                },
                MatchCase {
                    let_token: None,
                    binding: None,
                    equals_token: None,
                    case: expression([number_node("3")]).into(),
                    arrow_token: Some(DOUBLE_ARROW),
                    expression: expression([number_node("4")]),
                    semicolon_token: Some(SEMICOLON),
                },
            ],
            end_token: Some(END),
            diagnostics: Diagnostics::default(),
        }
        .into()])
        .into(),
        ..Default::default()
    };
    assert_eq!(actual, expected);
}

#[test]
fn enum_creation() {
    let source = "let Option = enum Some: any, None: () end;";
    let actual = Block::from(&mut Lexer::from(source).peekable());
    let expected = Block {
        statements: vec![binding(
            "Option",
            [Node::Enum(Enum {
                enum_token: Some(ENUM),
                variants: expression([
                    variable("any"),
                    Node::Name {
                        name: ident("Some"),
                        colon_token: COLON,
                    },
                    Node::Unit,
                    Node::Name {
                        name: ident("None"),
                        colon_token: COLON,
                    },
                    TUPLE,
                ]),
                then_token: None,
                members: None,
                end_token: Some(END),
                diagnostics: Diagnostics::default(),
            })],
        )],
        ..Default::default()
    };
    assert_eq!(actual, expected);
}

#[test]
fn implementation() {
    let source = "impl Iterator for Array then next: {with self; next} end";
    let actual = Block::from(&mut Lexer::from(source).peekable());
    let expected = Block {
        statements: vec![Statement::Implementation(Implementation {
            impl_token: IMPL,
            trait_expression: expression([variable("Iterator")]),
            for_token: Some(FOR),
            struct_expression: expression([variable("Array")]),
            then_token: Some(THEN),
            block: Block {
                result: expression([
                    Node::Block(Block {
                        result: Function {
                            with_token: Some(WITH),
                            argument: Some(ident("self")),
                            semicolon_token: Some(SEMICOLON),
                            block: Block {
                                result: expression([variable("next")]).into(),
                                ..Default::default()
                            },
                            diagnostics: Diagnostics::default(),
                        }
                        .into(),
                        ..Default::default()
                    }),
                    Node::Name {
                        name: ident("next"),
                        colon_token: COLON,
                    },
                ])
                .into(),
                ..Default::default()
            },
            end_token: Some(END),
            diagnostics: Diagnostics::default(),
        })],
        ..Default::default()
    };
    assert_eq!(actual, expected);
}

#[test]
fn field_precedence() {
    let source = "something.field |> Iterator.next ()";
    let actual = Block::from(&mut Lexer::from(source).peekable());
    let expected = Block {
        result: expression([
            variable("something"),
            Node::Field {
                dot_token: DOT,
                index: ident("field"),
            },
            variable("Iterator"),
            Node::Field {
                dot_token: DOT,
                index: ident("next"),
            },
            PIPE,
            Node::Unit,
            Node::Call(Some(OPEN_PAREN)),
        ])
        .into(),
        ..Default::default()
    };
    assert_eq!(actual, expected);
}

#[test]
fn tuple_indexing() {
    let source = "let x = 1, 2; x.1";
    let actual = Block::from(&mut Lexer::from(source).peekable());
    let expected = Block {
        statements: vec![binding("x", [number_node("1"), number_node("2"), TUPLE])],
        result: expression([
            variable("x"),
            Node::Field {
                dot_token: DOT,
                index: number("1"),
            },
        ])
        .into(),
        ..Default::default()
    };
    assert_eq!(actual, expected);
}

#[test]
fn string() {
    let source = "\"string\"";
    let actual = Block::from(&mut Lexer::from(source).peekable());
    let expected = Block {
        result: expression([Node::String(Token {
            origin: "\"string\"",
            lexigram: Lexigram::String,
        })])
        .into(),
        ..Default::default()
    };
    assert_eq!(actual, expected);
}
