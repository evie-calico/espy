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
                contents: vec![Node::Number(
                    "1",
                    Some(Token {
                        lexigram: Lexigram::Number("1"),
                        start: 8,
                        end: 9,
                    }),
                )],
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
                Node::Number(
                    "1",
                    Some(Token {
                        lexigram: Lexigram::Number("1"),
                        start: 0,
                        end: 1,
                    }),
                ),
                Node::Number(
                    "2",
                    Some(Token {
                        lexigram: Lexigram::Number("2"),
                        start: 4,
                        end: 5,
                    }),
                ),
                Node::Mul(Some(Token {
                    lexigram: Lexigram::Star,
                    start: 2,
                    end: 3,
                })),
                Node::Number(
                    "3",
                    Some(Token {
                        lexigram: Lexigram::Number("3"),
                        start: 7,
                        end: 8,
                    }),
                ),
                Node::Tuple(Some(Token {
                    lexigram: Lexigram::Comma,
                    start: 5,
                    end: 6,
                })),
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
                Node::Ident(
                    "x",
                    Some(Token {
                        lexigram: Lexigram::Ident("x"),
                        start: 0,
                        end: 1,
                    }),
                ),
                Node::Number(
                    "1",
                    Some(Token {
                        lexigram: Lexigram::Number("1"),
                        start: 3,
                        end: 4,
                    }),
                ),
                Node::Name(Some(Token {
                    lexigram: Lexigram::Colon,
                    start: 1,
                    end: 2,
                })),
                Node::Ident(
                    "y",
                    Some(Token {
                        lexigram: Lexigram::Ident("y"),
                        start: 6,
                        end: 7,
                    }),
                ),
                Node::Number(
                    "2",
                    Some(Token {
                        lexigram: Lexigram::Number("2"),
                        start: 9,
                        end: 10,
                    }),
                ),
                Node::Name(Some(Token {
                    lexigram: Lexigram::Colon,
                    start: 7,
                    end: 8,
                })),
                Node::Tuple(Some(Token {
                    lexigram: Lexigram::Comma,
                    start: 4,
                    end: 5,
                })),
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
                            contents: vec![Node::Number(
                                "2",
                                Some(Token {
                                    lexigram: Lexigram::Number("2"),
                                    start: 18,
                                    end: 19,
                                }),
                            )],
                            ..Default::default()
                        }),
                        ..Default::default()
                    }]
                    .into(),
                    result: Expression {
                        contents: vec![
                            Node::Ident(
                                "y",
                                Some(Token {
                                    lexigram: Lexigram::Ident("y"),
                                    start: 21,
                                    end: 22,
                                }),
                            ),
                            Node::Number(
                                "3",
                                Some(Token {
                                    lexigram: Lexigram::Number("3"),
                                    start: 25,
                                    end: 26,
                                }),
                            ),
                            Node::Mul(Some(Token {
                                lexigram: Lexigram::Star,
                                start: 23,
                                end: 24,
                            })),
                        ],
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
                        contents: vec![Node::Ident(
                            "condition",
                            Some(Token {
                                lexigram: Lexigram::Ident("condition"),
                                start: 11,
                                end: 20,
                            }),
                        )],
                        ..Default::default()
                    },
                    first: Block {
                        result: Expression {
                            contents: vec![Node::Number(
                                "1",
                                Some(Token {
                                    lexigram: Lexigram::Number("1"),
                                    start: 26,
                                    end: 27,
                                }),
                            )],
                            ..Default::default()
                        },
                        ..Default::default()
                    },
                    second: Block {
                        result: Expression {
                            contents: vec![Node::Number(
                                "2",
                                Some(Token {
                                    lexigram: Lexigram::Number("2"),
                                    start: 38,
                                    end: 39,
                                }),
                            )],
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
                        contents: vec![Node::Ident(
                            "condition",
                            Some(Token {
                                lexigram: Lexigram::Ident("condition"),
                                start: 11,
                                end: 20,
                            }),
                        )],
                        ..Default::default()
                    },
                    first: Block {
                        result: Expression {
                            contents: vec![Node::Number(
                                "1",
                                Some(Token {
                                    lexigram: Lexigram::Number("1"),
                                    start: 26,
                                    end: 27,
                                }),
                            )],
                            ..Default::default()
                        },
                        ..Default::default()
                    },
                    second: Block {
                        result: Expression {
                            contents: vec![Node::If {
                                condition: Expression {
                                    contents: vec![Node::Ident(
                                        "other",
                                        Some(Token {
                                            lexigram: Lexigram::Ident("other"),
                                            start: 36,
                                            end: 41,
                                        }),
                                    )],
                                    ..Default::default()
                                },
                                first: Block {
                                    result: Expression {
                                        contents: vec![Node::Number(
                                            "2",
                                            Some(Token {
                                                lexigram: Lexigram::Number("2"),
                                                start: 47,
                                                end: 48,
                                            }),
                                        )],
                                        ..Default::default()
                                    },
                                    ..Default::default()
                                },
                                second: Block {
                                    result: Expression {
                                        contents: vec![Node::Number(
                                            "3",
                                            Some(Token {
                                                lexigram: Lexigram::Number("3"),
                                                start: 59,
                                                end: 60,
                                            }),
                                        )],
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
            contents: vec![
                Node::Number(
                    "1",
                    Some(Token {
                        lexigram: Lexigram::Number("1"),
                        start: 0,
                        end: 1,
                    }),
                ),
                Node::Number(
                    "2",
                    Some(Token {
                        lexigram: Lexigram::Number("2"),
                        start: 4,
                        end: 5,
                    }),
                ),
                Node::Mul(Some(Token {
                    lexigram: Lexigram::Star,
                    start: 2,
                    end: 3,
                })),
            ],
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
            contents: vec![Node::Number(
                "2",
                Some(Token {
                    lexigram: Lexigram::Number("2"),
                    start: 6,
                    end: 7,
                }),
            )],
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
                        contents: vec![Node::Ident(
                            "iter",
                            Some(Token {
                                lexigram: Lexigram::Ident("iter"),
                                start: 9,
                                end: 13,
                            }),
                        )],
                        ..Default::default()
                    },
                    first: Block {
                        result: Expression {
                            contents: vec![
                                Node::Ident(
                                    "print",
                                    Some(Token {
                                        lexigram: Lexigram::Ident("print"),
                                        start: 19,
                                        end: 24,
                                    }),
                                ),
                                Node::Ident(
                                    "i",
                                    Some(Token {
                                        lexigram: Lexigram::Ident("i"),
                                        start: 25,
                                        end: 26,
                                    }),
                                ),
                                Node::Call(Some(Token {
                                    lexigram: Lexigram::Ident("i"),
                                    start: 25,
                                    end: 26,
                                })),
                            ],
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
                        contents: vec![Node::Ident(
                            "iter",
                            Some(Token {
                                lexigram: Lexigram::Ident("iter"),
                                start: 17,
                                end: 21,
                            }),
                        )],
                        ..Default::default()
                    },
                    first: Block {
                        result: Expression {
                            contents: vec![Node::If {
                                condition: Expression {
                                    contents: vec![
                                        Node::Ident(
                                            "i",
                                            Some(Token {
                                                lexigram: Lexigram::Ident("i"),
                                                start: 30,
                                                end: 31,
                                            }),
                                        ),
                                        Node::Ident(
                                            "needle",
                                            Some(Token {
                                                lexigram: Lexigram::Ident("needle"),
                                                start: 35,
                                                end: 41,
                                            }),
                                        ),
                                        Node::EqualTo(Some(Token {
                                            lexigram: Lexigram::EqualTo,
                                            start: 32,
                                            end: 34,
                                        })),
                                    ],
                                    ..Default::default()
                                },
                                first: Block {
                                    statements: vec![Statement {
                                        action: Some(Action::Break),
                                        expression: Some(Expression {
                                            contents: vec![
                                                Node::Ident(
                                                    "Some",
                                                    Some(Token {
                                                        lexigram: Lexigram::Ident("Some"),
                                                        start: 53,
                                                        end: 57,
                                                    }),
                                                ),
                                                Node::Ident(
                                                    "i",
                                                    Some(Token {
                                                        lexigram: Lexigram::Ident("i"),
                                                        start: 58,
                                                        end: 59,
                                                    }),
                                                ),
                                                Node::Call(Some(Token {
                                                    lexigram: Lexigram::Ident("i"),
                                                    start: 58,
                                                    end: 59,
                                                })),
                                            ],
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
                            contents: vec![Node::Ident(
                                "None",
                                Some(Token {
                                    lexigram: Lexigram::Ident("None"),
                                    start: 70,
                                    end: 74,
                                }),
                            )],
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
            // Note that this is an invalid identifier,
            // but we still know the *intent* and can smooth things over for diagnostics.
            action: Some(
                Binding {
                    ident: "class",
                    ty: None,
                }
                .into(),
            ),
            expression: Some(Expression {
                contents: vec![Node::Number(
                    "1",
                    Some(Token {
                        lexigram: Lexigram::Number("1"),
                        start: 12,
                        end: 13,
                    }),
                )],
                ..Default::default()
            }),
            diagnostics: Diagnostics {
                contents: vec![Diagnostic::Error(Error::Lexer(lexer::Error {
                    kind: lexer::ErrorKind::ReservedSymbol("class"),
                    start: 4,
                    end: 9,
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
                expression: Some(Expression {
                    contents: vec![
                        Node::Number(
                            "1",
                            Some(Token {
                                lexigram: Lexigram::Number("1"),
                                start: 0,
                                end: 1,
                            }),
                        ),
                        Node::Number(
                            "1",
                            Some(Token {
                                lexigram: Lexigram::Number("1"),
                                start: 5,
                                end: 6,
                            }),
                        ),
                        Node::EqualTo(Some(Token {
                            lexigram: Lexigram::EqualTo,
                            start: 2,
                            end: 4,
                        })),
                    ],
                    ..Default::default()
                }),
                ..Default::default()
            },
            Statement {
                expression: Some(Expression {
                    contents: vec![
                        Node::Number(
                            "1",
                            Some(Token {
                                lexigram: Lexigram::Number("1"),
                                start: 8,
                                end: 9,
                            }),
                        ),
                        Node::Number(
                            "1",
                            Some(Token {
                                lexigram: Lexigram::Number("1"),
                                start: 13,
                                end: 14,
                            }),
                        ),
                        Node::NotEqualTo(Some(Token {
                            lexigram: Lexigram::NotEqualTo,
                            start: 10,
                            end: 12,
                        })),
                    ],
                    ..Default::default()
                }),
                ..Default::default()
            },
            Statement {
                expression: Some(Expression {
                    contents: vec![
                        Node::Number(
                            "1",
                            Some(Token {
                                lexigram: Lexigram::Number("1"),
                                start: 16,
                                end: 17,
                            }),
                        ),
                        Node::Number(
                            "1",
                            Some(Token {
                                lexigram: Lexigram::Number("1"),
                                start: 20,
                                end: 21,
                            }),
                        ),
                        Node::Greater(Some(Token {
                            lexigram: Lexigram::Greater,
                            start: 18,
                            end: 19,
                        })),
                    ],
                    ..Default::default()
                }),
                ..Default::default()
            },
            Statement {
                expression: Some(Expression {
                    contents: vec![
                        Node::Number(
                            "1",
                            Some(Token {
                                lexigram: Lexigram::Number("1"),
                                start: 23,
                                end: 24,
                            }),
                        ),
                        Node::Number(
                            "1",
                            Some(Token {
                                lexigram: Lexigram::Number("1"),
                                start: 28,
                                end: 29,
                            }),
                        ),
                        Node::GreaterEqual(Some(Token {
                            lexigram: Lexigram::GreaterEqual,
                            start: 25,
                            end: 27,
                        })),
                    ],
                    ..Default::default()
                }),
                ..Default::default()
            },
            Statement {
                expression: Some(Expression {
                    contents: vec![
                        Node::Number(
                            "1",
                            Some(Token {
                                lexigram: Lexigram::Number("1"),
                                start: 31,
                                end: 32,
                            }),
                        ),
                        Node::Number(
                            "1",
                            Some(Token {
                                lexigram: Lexigram::Number("1"),
                                start: 35,
                                end: 36,
                            }),
                        ),
                        Node::Lesser(Some(Token {
                            lexigram: Lexigram::Lesser,
                            start: 33,
                            end: 34,
                        })),
                    ],
                    ..Default::default()
                }),
                ..Default::default()
            },
            Statement {
                expression: Some(Expression {
                    contents: vec![
                        Node::Number(
                            "1",
                            Some(Token {
                                lexigram: Lexigram::Number("1"),
                                start: 38,
                                end: 39,
                            }),
                        ),
                        Node::Number(
                            "1",
                            Some(Token {
                                lexigram: Lexigram::Number("1"),
                                start: 42,
                                end: 43,
                            }),
                        ),
                        Node::LesserEqual(Some(Token {
                            lexigram: Lexigram::LesserEqual,
                            start: 39,
                            end: 41,
                        })),
                    ],
                    ..Default::default()
                }),
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
        result: Expression {
            contents: vec![
                Node::Number(
                    "1",
                    Some(Token {
                        lexigram: Lexigram::Number("1"),
                        start: 0,
                        end: 1,
                    }),
                ),
                Node::Number(
                    "2",
                    Some(Token {
                        lexigram: Lexigram::Number("2"),
                        start: 4,
                        end: 5,
                    }),
                ),
                Node::Number(
                    "3",
                    Some(Token {
                        lexigram: Lexigram::Number("3"),
                        start: 8,
                        end: 9,
                    }),
                ),
                Node::BitwiseAnd(Some(Token {
                    lexigram: Lexigram::Ampersand,
                    start: 6,
                    end: 7,
                })),
                Node::Number(
                    "4",
                    Some(Token {
                        lexigram: Lexigram::Number("4"),
                        start: 12,
                        end: 13,
                    }),
                ),
                Node::BitwiseXor(Some(Token {
                    lexigram: Lexigram::Caret,
                    start: 10,
                    end: 11,
                })),
                Node::BitwiseOr(Some(Token {
                    lexigram: Lexigram::Pipe,
                    start: 2,
                    end: 3,
                })),
            ],
            ..Default::default()
        },
        ..Default::default()
    };
    assert_eq!(actual, expected);
}

#[test]
fn nested_parens() {
    let source = "1 | (2 & (3 ^ 4))";
    let actual = Block::from(Ast::from(&mut Lexer::from(source).peekable()));
    let expected = Block {
        result: Expression {
            contents: vec![
                Node::Number(
                    "1",
                    Some(Token {
                        lexigram: Lexigram::Number("1"),
                        start: 0,
                        end: 1,
                    }),
                ),
                Node::Number(
                    "2",
                    Some(Token {
                        lexigram: Lexigram::Number("2"),
                        start: 5,
                        end: 6,
                    }),
                ),
                Node::Number(
                    "3",
                    Some(Token {
                        lexigram: Lexigram::Number("3"),
                        start: 10,
                        end: 11,
                    }),
                ),
                Node::Number(
                    "4",
                    Some(Token {
                        lexigram: Lexigram::Number("4"),
                        start: 14,
                        end: 15,
                    }),
                ),
                Node::BitwiseXor(Some(Token {
                    lexigram: Lexigram::Caret,
                    start: 12,
                    end: 13,
                })),
                Node::BitwiseAnd(Some(Token {
                    lexigram: Lexigram::Ampersand,
                    start: 7,
                    end: 8,
                })),
                Node::BitwiseOr(Some(Token {
                    lexigram: Lexigram::Pipe,
                    start: 2,
                    end: 3,
                })),
            ],
            ..Default::default()
        },
        ..Default::default()
    };
    assert_eq!(actual, expected);
}

#[test]
fn pipe() {
    let source = "1 |> 2 |> f x";
    let actual = Block::from(Ast::from(&mut Lexer::from(source).peekable()));
    let expected = Block {
        result: Expression {
            contents: vec![
                Node::Number(
                    "1",
                    Some(Token {
                        lexigram: Lexigram::Number("1"),
                        start: 0,
                        end: 1,
                    }),
                ),
                Node::Number(
                    "2",
                    Some(Token {
                        lexigram: Lexigram::Number("2"),
                        start: 5,
                        end: 6,
                    }),
                ),
                Node::Ident(
                    "f",
                    Some(Token {
                        lexigram: Lexigram::Ident("f"),
                        start: 10,
                        end: 11,
                    }),
                ),
                Node::Pipe(Some(Token {
                    lexigram: Lexigram::Triangle,
                    start: 7,
                    end: 9,
                })),
                Node::Pipe(Some(Token {
                    lexigram: Lexigram::Triangle,
                    start: 2,
                    end: 4,
                })),
                Node::Ident(
                    "x",
                    Some(Token {
                        lexigram: Lexigram::Ident("x"),
                        start: 12,
                        end: 13,
                    }),
                ),
                Node::Call(Some(Token {
                    lexigram: Lexigram::Ident("x"),
                    start: 12,
                    end: 13,
                })),
            ],
            ..Default::default()
        },
        ..Default::default()
    };
    assert_eq!(actual, expected);
}
