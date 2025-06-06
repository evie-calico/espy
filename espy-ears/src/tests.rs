use crate::*;

#[test]
fn assignment() {
    let source = "let x = 1;";
    let actual = Block::from(Ast::from(&mut Lexer::from(source).peekable()));
    let expected = Block {
        statements: vec![Statement {
            action: Some(
                Binding {
                    ident: "x",
                    ty: None,
                }
                .into(),
            ),
            expression: Some(Expression {
                contents: vec![Node::Number("1")],
                ..Default::default()
            }),
            ..Default::default()
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
        result: Expression {
            contents: vec![
                Node::Number("1"),
                Node::Number("2"),
                Node::Mul,
                Node::Number("3"),
                Node::Tuple,
            ],
            ..Default::default()
        },
        ..Default::default()
    };
    assert_eq!(actual, expected);
}

#[test]
fn named_tuple() {
    let source = "x: 1, y: 2";
    let actual = Block::from(Ast::from(&mut Lexer::from(source).peekable()));
    let expected = Block {
        result: Expression {
            contents: vec![
                Node::Ident("x"),
                Node::Number("1"),
                Node::Name,
                Node::Ident("y"),
                Node::Number("2"),
                Node::Name,
                Node::Tuple,
            ],
            ..Default::default()
        },
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
            action: Some(
                Binding {
                    ident: "x",
                    ty: None,
                }
                .into(),
            ),
            expression: Some(Expression {
                contents: vec![Node::Block(Block {
                    statements: [Statement {
                        action: Some(
                            Binding {
                                ident: "y",
                                ty: None,
                            }
                            .into(),
                        ),
                        expression: Some(Expression {
                            contents: vec![Node::Number("2")],
                            ..Default::default()
                        }),
                        ..Default::default()
                    }]
                    .into(),
                    result: Expression {
                        contents: vec![Node::Ident("y"), Node::Number("3"), Node::Mul],
                        ..Default::default()
                    },
                    ..Default::default()
                })],
                ..Default::default()
            }),
            ..Default::default()
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
            action: Some(
                Binding {
                    ident: "x",
                    ty: None,
                }
                .into(),
            ),
            expression: Some(Expression {
                contents: vec![Node::If {
                    condition: Expression {
                        contents: vec![Node::Ident("condition")],
                        ..Default::default()
                    },
                    first: Block {
                        result: Expression {
                            contents: vec![Node::Number("1")],
                            ..Default::default()
                        },
                        ..Default::default()
                    },
                    second: Block {
                        result: Expression {
                            contents: vec![Node::Number("2")],
                            ..Default::default()
                        },
                        ..Default::default()
                    },
                    diagnostics: Diagnostics::default(),
                }],
                ..Default::default()
            }),
            ..Default::default()
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
            action: Some(
                Binding {
                    ident: "x",
                    ty: None,
                }
                .into(),
            ),
            expression: Some(Expression {
                contents: vec![Node::If {
                    condition: Expression {
                        contents: vec![Node::Ident("condition")],
                        ..Default::default()
                    },
                    first: Block {
                        result: Expression {
                            contents: vec![Node::Number("1")],
                            ..Default::default()
                        },
                        ..Default::default()
                    },
                    second: Block {
                        result: Expression {
                            contents: vec![Node::If {
                                condition: Expression {
                                    contents: vec![Node::Ident("other")],
                                    ..Default::default()
                                },
                                first: Block {
                                    result: Expression {
                                        contents: vec![Node::Number("2")],
                                        ..Default::default()
                                    },
                                    ..Default::default()
                                },
                                second: Block {
                                    result: Expression {
                                        contents: vec![Node::Number("3")],
                                        ..Default::default()
                                    },
                                    ..Default::default()
                                },
                                diagnostics: Diagnostics::default(),
                            }],
                            ..Default::default()
                        },
                        ..Default::default()
                    },
                    diagnostics: Diagnostics::default(),
                }],
                ..Default::default()
            }),
            ..Default::default()
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
            contents: vec![Node::Number("1"), Node::Number("2"), Node::Mul],
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
                    ident: "x",
                    ty: None,
                }
                .into(),
            ),
            expression: None,
            diagnostics: Diagnostics {
                contents: vec![Diagnostic::Error(Error::MissingToken {
                    expected: &[Lexigram::Equals, Lexigram::Semicolon],
                    actual: Some(Token {
                        start: 6,
                        end: 7,
                        lexigram: Lexigram::Number("2"),
                    }),
                })],
            },
        }],
        result: Expression {
            contents: vec![Node::Number("2")],
            ..Default::default()
        },
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
            expression: Some(Expression {
                contents: vec![Node::For {
                    binding: Some("i"),
                    iterator: Expression {
                        contents: vec![Node::Ident("iter")],
                        ..Default::default()
                    },
                    first: Block {
                        result: Expression {
                            contents: vec![Node::Ident("print"), Node::Ident("i")],
                            ..Default::default()
                        },
                        ..Default::default()
                    },
                    second: Block::default(),
                    diagnostics: Diagnostics::default(),
                }],
                ..Default::default()
            }),
            ..Default::default()
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
            action: Some(
                Binding {
                    ident: "x",
                    ty: None,
                }
                .into(),
            ),
            expression: Some(Expression {
                contents: vec![Node::For {
                    binding: Some("i"),
                    iterator: Expression {
                        contents: vec![Node::Ident("iter")],
                        ..Default::default()
                    },
                    first: Block {
                        result: Expression {
                            contents: vec![Node::If {
                                condition: Expression {
                                    contents: vec![
                                        Node::Ident("i"),
                                        Node::Ident("needle"),
                                        Node::EqualTo,
                                    ],
                                    ..Default::default()
                                },
                                first: Block {
                                    statements: vec![Statement {
                                        action: Some(Action::Break),
                                        expression: Some(Expression {
                                            contents: vec![Node::Ident("Some"), Node::Ident("i")],
                                            ..Default::default()
                                        }),
                                        ..Default::default()
                                    }],
                                    ..Default::default()
                                },
                                second: Block::default(),
                                diagnostics: Diagnostics::default(),
                            }],
                            ..Default::default()
                        },
                        ..Default::default()
                    },
                    second: Block {
                        result: Expression {
                            contents: vec![Node::Ident("None")],
                            ..Default::default()
                        },
                        ..Default::default()
                    },
                    diagnostics: Diagnostics::default(),
                }],
                ..Default::default()
            }),
            ..Default::default()
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
            action: None,
            expression: None,
            diagnostics: Diagnostics {
                contents: vec![
                    Diagnostic::Error(Error::Lexer(lexer::Error::ReservedSymbol("class"))),
                    Diagnostic::Error(Error::MissingToken {
                        expected: &[Lexigram::Ident("")],
                        actual: None,
                    }),
                ],
            },
        }],
        ..Default::default()
    };
    assert_eq!(actual, expected);
}

#[test]
fn comparison() {
    let source = "1 == 1; 1 != 1; 1 > 1; 1 >= 1; 1 < 1; 1<= 1;";
    let actual = Block::from(Ast::from(&mut Lexer::from(source).peekable()));
    let expected = Block {
        statements: vec![
            Statement {
                expression: Some(Expression {
                    contents: vec![Node::Number("1"), Node::Number("1"), Node::EqualTo],
                    ..Default::default()
                }),
                ..Default::default()
            },
            Statement {
                expression: Some(Expression {
                    contents: vec![Node::Number("1"), Node::Number("1"), Node::NotEqualTo],
                    ..Default::default()
                }),
                ..Default::default()
            },
            Statement {
                expression: Some(Expression {
                    contents: vec![Node::Number("1"), Node::Number("1"), Node::Greater],
                    ..Default::default()
                }),
                ..Default::default()
            },
            Statement {
                expression: Some(Expression {
                    contents: vec![Node::Number("1"), Node::Number("1"), Node::GreaterEqual],
                    ..Default::default()
                }),
                ..Default::default()
            },
            Statement {
                expression: Some(Expression {
                    contents: vec![Node::Number("1"), Node::Number("1"), Node::Lesser],
                    ..Default::default()
                }),
                ..Default::default()
            },
            Statement {
                expression: Some(Expression {
                    contents: vec![Node::Number("1"), Node::Number("1"), Node::LesserEqual],
                    ..Default::default()
                }),
                ..Default::default()
            },
        ],
        ..Default::default()
    };
    assert_eq!(actual, expected);
}
