use crate::*;

fn number<'source>(origin: &'source str) -> Node<'source> {
    Node::Number(
        origin,
        Some(Token {
            origin,
            lexigram: Lexigram::Number,
        }),
    )
}

fn ident<'source>(origin: &'source str) -> Node<'source> {
    Node::Ident(
        origin,
        Some(Token {
            origin,
            lexigram: Lexigram::Ident,
        }),
    )
}

macro_rules! token {
    ($name:ident: $lexigram:ident = $origin:literal) => {
        const $name: Token = Token {
            origin: $origin,
            lexigram: Lexigram::$lexigram,
        };
    };
}

macro_rules! node {
    ($name:ident: $node:ident = $origin:literal as $lexigram:ident) => {
        const $name: Node = Node::$node(Some(Token {
            origin: $origin,
            lexigram: Lexigram::$lexigram,
        }));
    };
}

token!(SEMICOLON: Semicolon = ";");
node!(PIPE: Pipe = "|>" as Triangle);
node!(MUL: Mul = "*" as Star);
node!(BITWISE_AND: BitwiseAnd = "&" as Ampersand);
node!(BITWISE_XOR: BitwiseXor = "^" as Caret);
node!(BITWISE_OR: BitwiseOr = "|" as Pipe);
node!(EQUAL_TO: EqualTo = "==" as EqualTo);
node!(NOT_EQUAL_TO: NotEqualTo = "!=" as NotEqualTo);
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
        ident_token: Some(Token {
            origin,
            lexigram: Lexigram::Ident,
        }),
        colon_token: None,
        ty_token: None,
        equals_token: Some(Token {
            origin: "=",
            lexigram: Lexigram::Equals,
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
    let actual = Block::from(Ast::from(&mut Lexer::from(source).peekable()));
    let expected = Block {
        statements: vec![Statement {
            action: binding("x"),
            expression: expression([number("1")]).into(),
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
    let actual = Block::from(Ast::from(&mut Lexer::from(source).peekable()));
    let expected = Block {
        result: expression([number("1"), number("2"), MUL, number("3"), TUPLE]),
        ..Default::default()
    };
    assert_eq!(actual, expected);
}

#[test]
fn named_tuple() {
    let source = "x: 1, y: 2";
    let actual = Block::from(Ast::from(&mut Lexer::from(source).peekable()));
    let expected = Block {
        result: expression([
            ident("x"),
            number("1"),
            NAME,
            ident("y"),
            number("2"),
            NAME,
            TUPLE,
        ]),
        ..Default::default()
    };
    assert_eq!(actual, expected);
}

#[test]
fn block_expression() {
    let source = "let x = { let y = 2; y * 3 };";
    let actual = Block::from(Ast::from(&mut Lexer::from(source).peekable()));
    let expected = Block {
        statements: vec![Statement {
            action: binding("x"),
            expression: expression([Node::Block(Block {
                statements: [Statement {
                    action: binding("y"),
                    expression: expression([number("2")]).into(),
                    semicolon_token: Some(SEMICOLON),
                    diagnostics: Diagnostics::default(),
                }]
                .into(),
                result: expression([ident("y"), number("3"), MUL]),
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
    let actual = Block::from(Ast::from(&mut Lexer::from(source).peekable()));
    let expected = Block {
        statements: vec![Statement {
            action: binding("x"),
            expression: expression([Node::If {
                condition: expression([ident("condition")]),
                first: Block {
                    result: expression([number("1")]),
                    ..Default::default()
                },
                second: Block {
                    result: expression([number("2")]),
                    ..Default::default()
                },
                diagnostics: Diagnostics::default(),
            }])
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
    let actual = Block::from(Ast::from(&mut Lexer::from(source).peekable()));
    let expected = Block {
        statements: vec![Statement {
            action: binding("x"),
            expression: expression([Node::If {
                condition: expression([ident("condition")]),
                first: Block {
                    result: expression([number("1")]),
                    ..Default::default()
                },
                second: Block {
                    result: expression([Node::If {
                        condition: expression([ident("other")]),
                        first: Block {
                            result: expression([number("2")]),
                            ..Default::default()
                        },
                        second: Block {
                            result: expression([number("3")]),
                            ..Default::default()
                        },
                        diagnostics: Diagnostics::default(),
                    }]),
                    ..Default::default()
                },
                diagnostics: Diagnostics::default(),
            }])
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
    let actual = Block::from(Ast::from(&mut Lexer::from(source).peekable()));
    let expected = Block {
        result: Expression {
            contents: vec![number("1"), number("2"), MUL],
            diagnostics: Diagnostics {
                contents: vec![Diagnostic::Error(Error::IncompleteExpression)],
            },
        },
        ..Default::default()
    };
    assert_eq!(actual, expected);
}

#[test]
fn forgotten_semicolon() {
    let source = "let x 2";
    let actual = Block::from(Ast::from(&mut Lexer::from(source).peekable()));
    let expected = Block {
        statements: vec![Statement {
            action: Some(
                Binding {
                    let_token: Some(Token {
                        origin: "let",
                        lexigram: Lexigram::Let,
                    }),
                    ident_token: Some(Token {
                        origin: "x",
                        lexigram: Lexigram::Ident,
                    }),
                    colon_token: None,
                    ty_token: None,
                    equals_token: None,
                }
                .into(),
            ),
            expression: None,
            semicolon_token: None,
            diagnostics: Diagnostics {
                contents: vec![Diagnostic::Error(Error::MissingToken {
                    expected: &[Lexigram::Equals, Lexigram::Semicolon],
                    actual: Some(Token {
                        origin: "2",
                        lexigram: Lexigram::Number,
                    }),
                })],
            },
        }],
        result: expression([number("2")]),
        ..Default::default()
    };
    assert_eq!(actual, expected);
}

#[test]
fn for_loop() {
    let source = "for i in iter then print i end;";
    let actual = Block::from(Ast::from(&mut Lexer::from(source).peekable()));
    let expected = Block {
        statements: vec![Statement {
            action: None,
            expression: expression([Node::For {
                binding: Some("i"),
                iterator: expression([ident("iter")]),
                first: Block {
                    result: expression([
                        ident("print"),
                        ident("i"),
                        Node::Call(Some(Token {
                            origin: "i",
                            lexigram: Lexigram::Ident,
                        })),
                    ]),
                    ..Default::default()
                },
                second: Block::default(),
                diagnostics: Diagnostics::default(),
            }])
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
    let actual = Block::from(Ast::from(&mut Lexer::from(source).peekable()));
    let expected = Block {
        statements: vec![Statement {
            action: binding("x"),
            expression: expression([Node::For {
                binding: Some("i"),
                iterator: expression([ident("iter")]),
                first: Block {
                    result: expression([Node::If {
                        condition: expression([ident("i"), ident("needle"), EQUAL_TO]),
                        first: Block {
                            statements: vec![Statement {
                                action: Some(Action::Break(Some(Token {
                                    origin: "break",
                                    lexigram: Lexigram::Break,
                                }))),
                                expression: expression([
                                    ident("Some"),
                                    ident("i"),
                                    Node::Call(Some(Token {
                                        origin: "i",
                                        lexigram: Lexigram::Ident,
                                    })),
                                ])
                                .into(),
                                semicolon_token: Some(SEMICOLON),
                                diagnostics: Diagnostics::default(),
                            }],
                            ..Default::default()
                        },
                        second: Block::default(),
                        diagnostics: Diagnostics::default(),
                    }]),
                    ..Default::default()
                },
                second: Block {
                    result: expression([ident("None")]),
                    ..Default::default()
                },
                diagnostics: Diagnostics::default(),
            }])
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
    let actual = Block::from(Ast::from(&mut Lexer::from(source).peekable()));
    let expected = Block {
        statements: vec![Statement {
            // Note that this is an invalid identifier,
            // but we still know the *intent* and can smooth things over for diagnostics.
            action: binding("class"),
            expression: expression([number("1")]).into(),
            semicolon_token: Some(SEMICOLON),
            diagnostics: Diagnostics {
                contents: vec![Diagnostic::Error(Error::Lexer(lexer::Error {
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
    let actual = Block::from(Ast::from(&mut Lexer::from(source).peekable()));
    let expected = Block {
        statements: vec![
            Statement {
                expression: expression([number("1"), number("1"), EQUAL_TO]).into(),
                semicolon_token: Some(SEMICOLON),
                ..Default::default()
            },
            Statement {
                expression: expression([number("1"), number("1"), NOT_EQUAL_TO]).into(),
                semicolon_token: Some(SEMICOLON),
                ..Default::default()
            },
            Statement {
                expression: expression([number("1"), number("1"), GREATER]).into(),
                semicolon_token: Some(SEMICOLON),
                ..Default::default()
            },
            Statement {
                expression: expression([number("1"), number("1"), GREATER_EQUAL]).into(),
                semicolon_token: Some(SEMICOLON),
                ..Default::default()
            },
            Statement {
                expression: expression([number("1"), number("1"), LESSER]).into(),
                semicolon_token: Some(SEMICOLON),
                ..Default::default()
            },
            Statement {
                expression: expression([number("1"), number("1"), LESSER_EQUAL]).into(),
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
    let actual = Block::from(Ast::from(&mut Lexer::from(source).peekable()));
    let expected = Block {
        result: expression([
            number("1"),
            number("2"),
            number("3"),
            BITWISE_AND,
            number("4"),
            BITWISE_XOR,
            BITWISE_OR,
        ]),
        ..Default::default()
    };
    assert_eq!(actual, expected);
}

#[test]
fn nested_parens() {
    let source = "1 | (2 & (3 ^ 4))";
    let actual = Block::from(Ast::from(&mut Lexer::from(source).peekable()));
    let expected = Block {
        result: expression([
            number("1"),
            number("2"),
            number("3"),
            number("4"),
            BITWISE_XOR,
            BITWISE_AND,
            BITWISE_OR,
        ]),
        ..Default::default()
    };
    assert_eq!(actual, expected);
}

#[test]
fn pipe_operator() {
    let source = "1 |> 2 |> f x";
    let actual = Block::from(Ast::from(&mut Lexer::from(source).peekable()));
    let expected = Block {
        result: expression([
            number("1"),
            number("2"),
            ident("f"),
            PIPE,
            PIPE,
            ident("x"),
            Node::Call(Some(Token {
                origin: "x",
                lexigram: Lexigram::Ident,
            })),
        ]),
        ..Default::default()
    };
    assert_eq!(actual, expected);
}
