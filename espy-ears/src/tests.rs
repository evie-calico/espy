use crate::*;

macro_rules! token {
    ($name:ident: $lexigram:ident = $origin:literal) => {
        const $name: Token = Token {
            origin: $origin,
            lexigram: Lexigram::$lexigram,
        };
    };
}

token!(ELSE: Else = "else");
token!(END: End = "end");
token!(FOR: For = "for");
token!(IF: If = "if");
token!(IN: In = "in");
token!(STRUCT: Struct = "struct");
token!(THEN: Then = "then");
token!(WITH: With = "with");
token!(SEMICOLON: Semicolon = ";");

fn ident<'source>(origin: &'source str) -> Token<'source> {
    Token {
        origin,
        lexigram: Lexigram::Ident,
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
    Node::Number(Token {
        origin,
        lexigram: Lexigram::Number,
    })
}

fn ident_node<'source>(origin: &'source str) -> Node<'source> {
    Node::Ident(ident(origin))
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
node!(NAME: Name = ":" as Colon);

fn binding<'source>(origin: &'source str) -> Option<Action<'source>> {
    Some(Action::Binding(Binding {
        let_token: Some(Token {
            origin: "let",
            lexigram: Lexigram::Let,
        }),
        ident_token: Some(ident(origin)),
        colon_token: None,
        ty_token: None,
        equals_token: Some(Token {
            origin: "=",
            lexigram: Lexigram::SingleEqual,
        }),
    }))
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
        statements: vec![Statement {
            action: binding("x"),
            expression: expression([number_node("1")]).into(),
            semicolon_token: Some(SEMICOLON),
            diagnostics: Diagnostics::default(),
        }],
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
            ident_node("x"),
            number_node("1"),
            NAME,
            ident_node("y"),
            number_node("2"),
            NAME,
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
        statements: vec![Statement {
            action: binding("x"),
            expression: expression([Node::Block(Block {
                statements: [Statement {
                    action: binding("y"),
                    expression: expression([number_node("2")]).into(),
                    semicolon_token: Some(SEMICOLON),
                    diagnostics: Diagnostics::default(),
                }]
                .into(),
                result: expression([ident_node("y"), number_node("3"), MUL]).into(),
                ..Default::default()
            })])
            .into(),
            semicolon_token: Some(SEMICOLON),
            diagnostics: Diagnostics::default(),
        }],
        ..Default::default()
    };
    assert_eq!(actual, expected);
}

#[test]
fn if_expression() {
    let source = "let x = if condition then 1 else then 2 end;";
    let actual = Block::from(&mut Lexer::from(source).peekable());
    let expected = Block {
        statements: vec![Statement {
            action: binding("x"),
            expression: expression([If {
                if_token: Some(IF),
                condition: expression([ident_node("condition")]),
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
            .into()])
            .into(),
            semicolon_token: Some(SEMICOLON),
            diagnostics: Diagnostics::default(),
        }],
        ..Default::default()
    };
    assert_eq!(actual, expected);
}

#[test]
fn if_else() {
    let source = "let x = if condition then 1 else if other then 2 else then 3 end;";
    let actual = Block::from(&mut Lexer::from(source).peekable());
    let expected = Block {
        statements: vec![Statement {
            action: binding("x"),
            expression: expression([If {
                if_token: Some(IF),
                condition: expression([ident_node("condition")]),
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
                        condition: expression([ident_node("other")]),
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
            .into()])
            .into(),
            semicolon_token: Some(SEMICOLON),
            diagnostics: Diagnostics::default(),
        }],
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
                errors: vec![Diagnostic::Error(Error::IncompleteExpression)],
            },
        }
        .into(),
        ..Default::default()
    };
    assert_eq!(actual, expected);
}

#[test]
fn forgotten_semicolon() {
    let source = "let x 2";
    let actual = Block::from(&mut Lexer::from(source).peekable());
    let expected = Block {
        statements: vec![Statement {
            action: Some(
                Binding {
                    let_token: Some(Token {
                        origin: "let",
                        lexigram: Lexigram::Let,
                    }),
                    ident_token: Some(ident("x")),
                    colon_token: None,
                    ty_token: None,
                    equals_token: None,
                }
                .into(),
            ),
            expression: None,
            semicolon_token: None,
            diagnostics: Diagnostics {
                errors: vec![Diagnostic::Error(Error::MissingToken {
                    expected: &[Lexigram::SingleEqual, Lexigram::Semicolon],
                    actual: Some(Token {
                        origin: "2",
                        lexigram: Lexigram::Number,
                    }),
                })],
            },
        }],
        result: expression([number_node("2")]).into(),
        ..Default::default()
    };
    assert_eq!(actual, expected);
}

#[test]
fn for_loop() {
    let source = "for i in iter then print i end;";
    let actual = Block::from(&mut Lexer::from(source).peekable());
    let expected = Block {
        statements: vec![Statement {
            action: None,
            expression: expression([For {
                for_token: Some(FOR),
                binding: Some(ident("i")),
                in_token: Some(IN),
                iterator: expression([ident_node("iter")]),
                then_token: Some(THEN),
                first: Block {
                    result: expression([
                        ident_node("print"),
                        ident_node("i"),
                        Node::Call(Some(ident("i"))),
                    ])
                    .into(),
                    ..Default::default()
                },
                else_token: None,
                second: Block::default(),
                end_token: Some(END),
                diagnostics: Diagnostics::default(),
            }
            .into()])
            .into(),
            semicolon_token: Some(SEMICOLON),
            diagnostics: Diagnostics::default(),
        }],
        ..Default::default()
    };
    assert_eq!(actual, expected);
}

#[test]
fn for_expression() {
    let source = "let x = for i in iter then if i == needle then break Some i; end else None end;";
    let actual = Block::from(&mut Lexer::from(source).peekable());
    let expected = Block {
        statements: vec![Statement {
            action: binding("x"),
            expression: expression([For {
                for_token: Some(FOR),
                binding: Some(ident("i")),
                in_token: Some(IN),
                iterator: expression([ident_node("iter")]),
                then_token: Some(THEN),
                first: Block {
                    result: expression([If {
                        if_token: Some(IF),
                        condition: expression([ident_node("i"), ident_node("needle"), EQUAL_TO]),
                        then_token: Some(THEN),
                        first: Block {
                            statements: vec![Statement {
                                action: Some(Action::Break(Some(Token {
                                    origin: "break",
                                    lexigram: Lexigram::Break,
                                }))),
                                expression: expression([
                                    ident_node("Some"),
                                    ident_node("i"),
                                    Node::Call(Some(ident("i"))),
                                ])
                                .into(),
                                semicolon_token: Some(SEMICOLON),
                                diagnostics: Diagnostics::default(),
                            }],
                            ..Default::default()
                        },
                        else_token: None,
                        else_kind: None,
                        second: Block::default(),
                        end_token: Some(END),
                        diagnostics: Diagnostics::default(),
                    }
                    .into()])
                    .into(),
                    ..Default::default()
                },
                else_token: Some(ELSE),
                second: Block {
                    result: expression([ident_node("None")]).into(),
                    ..Default::default()
                },
                end_token: Some(END),
                diagnostics: Diagnostics::default(),
            }
            .into()])
            .into(),
            semicolon_token: Some(SEMICOLON),
            diagnostics: Diagnostics::default(),
        }],
        ..Default::default()
    };
    assert_eq!(actual, expected);
}

#[test]
fn reserved_symbol() {
    let source = "let class = 1;";
    let actual = Block::from(&mut Lexer::from(source).peekable());
    let expected = Block {
        statements: vec![Statement {
            // Note that this is an invalid identifier,
            // but we still know the *intent* and can smooth things over for diagnostics.
            action: binding("class"),
            expression: expression([number_node("1")]).into(),
            semicolon_token: Some(SEMICOLON),
            diagnostics: Diagnostics {
                errors: vec![Diagnostic::Error(Error::Lexer(lexer::Error {
                    origin: "class",
                    kind: lexer::ErrorKind::ReservedSymbol,
                }))],
            },
        }],
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
            Statement {
                expression: expression([number_node("1"), number_node("1"), EQUAL_TO]).into(),
                semicolon_token: Some(SEMICOLON),
                ..Default::default()
            },
            Statement {
                expression: expression([number_node("1"), number_node("1"), NOT_EQUAL_TO]).into(),
                semicolon_token: Some(SEMICOLON),
                ..Default::default()
            },
            Statement {
                expression: expression([number_node("1"), number_node("1"), GREATER]).into(),
                semicolon_token: Some(SEMICOLON),
                ..Default::default()
            },
            Statement {
                expression: expression([number_node("1"), number_node("1"), GREATER_EQUAL]).into(),
                semicolon_token: Some(SEMICOLON),
                ..Default::default()
            },
            Statement {
                expression: expression([number_node("1"), number_node("1"), LESSER]).into(),
                semicolon_token: Some(SEMICOLON),
                ..Default::default()
            },
            Statement {
                expression: expression([number_node("1"), number_node("1"), LESSER_EQUAL]).into(),
                semicolon_token: Some(SEMICOLON),
                ..Default::default()
            },
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
    let source = "1 |> 2 |> f x";
    let actual = Block::from(&mut Lexer::from(source).peekable());
    let expected = Block {
        result: expression([
            number_node("1"),
            number_node("2"),
            ident_node("f"),
            PIPE,
            PIPE,
            ident_node("x"),
            Node::Call(Some(ident("x"))),
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
        statements: vec![Statement {
            action: binding("captured"),
            expression: expression([number_node("1")]).into(),
            semicolon_token: Some(SEMICOLON),
            diagnostics: Diagnostics::default(),
        }],
        result: Function {
            with_token: Some(WITH),
            arguments: vec![ident("x")],
            semicolon_token: Some(SEMICOLON),
            block: Block {
                result: expression([ident_node("x"), ident_node("captured"), MUL]).into(),
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
    let source = "let Coord = struct x: u32, y: u32 then let new = {with x, y; x, y}; end;";
    let actual = Block::from(&mut Lexer::from(source).peekable());
    let expected = Block {
        statements: vec![Statement {
            action: binding("Coord"),
            expression: expression([Node::Struct(Struct {
                struct_token: Some(STRUCT),
                inner: expression([
                    ident_node("x"),
                    ident_node("u32"),
                    NAME,
                    ident_node("y"),
                    ident_node("u32"),
                    NAME,
                    TUPLE,
                ]),
                then_token: Some(THEN),
                block: Block {
                    statements: vec![Statement {
                        action: binding("new"),
                        expression: expression([Node::Block(Block {
                            result: Function {
                                with_token: Some(WITH),
                                arguments: vec![ident("x"), ident("y")],
                                semicolon_token: Some(SEMICOLON),
                                block: Block {
                                    result: expression([ident_node("x"), ident_node("y"), TUPLE])
                                        .into(),
                                    ..Default::default()
                                },
                                diagnostics: Diagnostics::default(),
                            }
                            .into(),
                            ..Default::default()
                        })])
                        .into(),
                        semicolon_token: Some(SEMICOLON),
                        diagnostics: Diagnostics::default(),
                    }],
                    ..Default::default()
                },
                end_token: Some(END),
                diagnostics: Diagnostics::default(),
            })])
            .into(),
            semicolon_token: Some(SEMICOLON),
            diagnostics: Diagnostics::default(),
        }],
        ..Default::default()
    };
    assert_eq!(actual, expected);
}
