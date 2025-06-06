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
                contents: vec![ExpressionNode::Number("1")],
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
                ExpressionNode::Number("1"),
                ExpressionNode::Number("2"),
                ExpressionNode::Mul,
                ExpressionNode::Number("3"),
                ExpressionNode::Tuple,
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
                ExpressionNode::Ident("x"),
                ExpressionNode::Number("1"),
                ExpressionNode::Name,
                ExpressionNode::Ident("y"),
                ExpressionNode::Number("2"),
                ExpressionNode::Name,
                ExpressionNode::Tuple,
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
                contents: vec![ExpressionNode::Block(Block {
                    statements: [Statement {
                        action: Some(
                            Binding {
                                ident: "y",
                                ty: None,
                            }
                            .into(),
                        ),
                        expression: Some(Expression {
                            contents: vec![ExpressionNode::Number("2")],
                            ..Default::default()
                        }),
                        ..Default::default()
                    }]
                    .into(),
                    result: Expression {
                        contents: vec![
                            ExpressionNode::Ident("y"),
                            ExpressionNode::Number("3"),
                            ExpressionNode::Mul,
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
                contents: vec![ExpressionNode::If {
                    condition: Expression {
                        contents: vec![ExpressionNode::Ident("condition")],
                        ..Default::default()
                    },
                    first: Block {
                        result: Expression {
                            contents: vec![ExpressionNode::Number("1")],
                            ..Default::default()
                        },
                        ..Default::default()
                    },
                    second: Block {
                        result: Expression {
                            contents: vec![ExpressionNode::Number("2")],
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
                contents: vec![ExpressionNode::If {
                    condition: Expression {
                        contents: vec![ExpressionNode::Ident("condition")],
                        ..Default::default()
                    },
                    first: Block {
                        result: Expression {
                            contents: vec![ExpressionNode::Number("1")],
                            ..Default::default()
                        },
                        ..Default::default()
                    },
                    second: Block {
                        result: Expression {
                            contents: vec![ExpressionNode::If {
                                condition: Expression {
                                    contents: vec![ExpressionNode::Ident("other")],
                                    ..Default::default()
                                },
                                first: Block {
                                    result: Expression {
                                        contents: vec![ExpressionNode::Number("2")],
                                        ..Default::default()
                                    },
                                    ..Default::default()
                                },
                                second: Block {
                                    result: Expression {
                                        contents: vec![ExpressionNode::Number("3")],
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
                ExpressionNode::Number("1"),
                ExpressionNode::Number("2"),
                ExpressionNode::Mul,
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
            contents: vec![ExpressionNode::Number("2")],
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
                contents: vec![ExpressionNode::For {
                    binding: Some("i"),
                    iterator: Expression {
                        contents: vec![ExpressionNode::Ident("iter")],
                        ..Default::default()
                    },
                    first: Block {
                        result: Expression {
                            contents: vec![
                                ExpressionNode::Ident("print"),
                                ExpressionNode::Ident("i"),
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
