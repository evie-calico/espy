use crate::*;

fn number<'source>(s: &'source str, start: usize, end: usize) -> Node<'source> {
    Node::Number(
        s,
        Some(Token {
            lexigram: Lexigram::Number(s),
            start,
            end,
        }),
    )
}

fn ident<'source>(s: &'source str, start: usize, end: usize) -> Node<'source> {
    Node::Ident(
        s,
        Some(Token {
            lexigram: Lexigram::Ident(s),
            start,
            end,
        }),
    )
}

macro_rules! binop {
    ($name:ident = $node:ident: $lexigram:ident) => {
        fn $name(start: usize, end: usize) -> Node<'static> {
            Node::$node(Some(Token {
                lexigram: Lexigram::$lexigram,
                start,
                end,
            }))
        }
    };
}

binop!(pipe = Pipe: Triangle);
binop!(mul = Mul: Star);
binop!(bitwise_and = BitwiseAnd: Ampersand);
binop!(bitwise_xor = BitwiseXor: Caret);
binop!(bitwise_or = BitwiseOr: Pipe);
binop!(equal_to = EqualTo: EqualTo);
binop!(not_equal_to = NotEqualTo: NotEqualTo);
binop!(greater = Greater: Greater);
binop!(greater_equal = GreaterEqual: GreaterEqual);
binop!(lesser = Lesser: Lesser);
binop!(lesser_equal = LesserEqual: LesserEqual);
binop!(tuple = Tuple: Comma);
binop!(name = Name: Equals);

fn binding<'source>(ident: &'source str) -> Option<Action<'source>> {
    Some(Action::Binding(Binding { ident, ty: None }))
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
            expression: expression([number("1", 8, 9)]).into(),
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
        result: expression([
            number("1", 0, 1),
            number("2", 4, 5),
            mul(2, 3),
            number("3", 7, 8),
            tuple(5, 6),
        ]),
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
            ident("x", 0, 1),
            number("1", 3, 4),
            name(1, 2),
            ident("y", 6, 7),
            number("2", 9, 10),
            name(7, 8),
            tuple(4, 5),
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
                    expression: expression([number("2", 18, 19)]).into(),
                    ..Default::default()
                }]
                .into(),
                result: expression([ident("y", 21, 22), number("3", 25, 26), mul(23, 24)]),
                ..Default::default()
            })])
            .into(),
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
            action: binding("x"),
            expression: expression([Node::If {
                condition: expression([ident("condition", 11, 20)]),
                first: Block {
                    result: expression([number("1", 26, 27)]),
                    ..Default::default()
                },
                second: Block {
                    result: expression([number("2", 38, 39)]),
                    ..Default::default()
                },
                diagnostics: Diagnostics::default(),
            }])
            .into(),
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
            action: binding("x"),
            expression: expression([Node::If {
                condition: expression([ident("condition", 11, 20)]),
                first: Block {
                    result: expression([number("1", 26, 27)]),
                    ..Default::default()
                },
                second: Block {
                    result: expression([Node::If {
                        condition: expression([ident("other", 36, 41)]),
                        first: Block {
                            result: expression([number("2", 47, 48)]),
                            ..Default::default()
                        },
                        second: Block {
                            result: expression([number("3", 59, 60)]),
                            ..Default::default()
                        },
                        diagnostics: Diagnostics::default(),
                    }]),
                    ..Default::default()
                },
                diagnostics: Diagnostics::default(),
            }])
            .into(),
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
            contents: vec![number("1", 0, 1), number("2", 4, 5), mul(2, 3)],
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
            action: binding("x"),
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
        result: expression([number("2", 6, 7)]),
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
                iterator: expression([ident("iter", 9, 13)]),
                first: Block {
                    result: expression([
                        ident("print", 19, 24),
                        ident("i", 25, 26),
                        Node::Call(Some(Token {
                            lexigram: Lexigram::Ident("i"),
                            start: 25,
                            end: 26,
                        })),
                    ]),
                    ..Default::default()
                },
                second: Block::default(),
                diagnostics: Diagnostics::default(),
            }])
            .into(),
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
            action: binding("x"),
            expression: expression([Node::For {
                binding: Some("i"),
                iterator: expression([ident("iter", 17, 21)]),
                first: Block {
                    result: expression([Node::If {
                        condition: expression([
                            ident("i", 30, 31),
                            ident("needle", 35, 41),
                            equal_to(32, 34),
                        ]),
                        first: Block {
                            statements: vec![Statement {
                                action: Some(Action::Break),
                                expression: expression([
                                    ident("Some", 53, 57),
                                    ident("i", 58, 59),
                                    Node::Call(Some(Token {
                                        lexigram: Lexigram::Ident("i"),
                                        start: 58,
                                        end: 59,
                                    })),
                                ])
                                .into(),
                                ..Default::default()
                            }],
                            ..Default::default()
                        },
                        second: Block::default(),
                        diagnostics: Diagnostics::default(),
                    }]),
                    ..Default::default()
                },
                second: Block {
                    result: expression([ident("None", 70, 74)]),
                    ..Default::default()
                },
                diagnostics: Diagnostics::default(),
            }])
            .into(),
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
            action: binding("class"),
            expression: expression([number("1", 12, 13)]).into(),
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
                expression: expression([number("1", 0, 1), number("1", 5, 6), equal_to(2, 4)])
                    .into(),
                ..Default::default()
            },
            Statement {
                expression: expression([
                    number("1", 8, 9),
                    number("1", 13, 14),
                    not_equal_to(10, 12),
                ])
                .into(),
                ..Default::default()
            },
            Statement {
                expression: expression([number("1", 16, 17), number("1", 20, 21), greater(18, 19)])
                    .into(),
                ..Default::default()
            },
            Statement {
                expression: expression([
                    number("1", 23, 24),
                    number("1", 28, 29),
                    greater_equal(25, 27),
                ])
                .into(),
                ..Default::default()
            },
            Statement {
                expression: expression([number("1", 31, 32), number("1", 35, 36), lesser(33, 34)])
                    .into(),
                ..Default::default()
            },
            Statement {
                expression: expression([
                    number("1", 38, 39),
                    number("1", 42, 43),
                    lesser_equal(39, 41),
                ])
                .into(),
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
            number("1", 0, 1),
            number("2", 4, 5),
            number("3", 8, 9),
            bitwise_and(6, 7),
            number("4", 12, 13),
            bitwise_xor(10, 11),
            bitwise_or(2, 3),
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
            number("1", 0, 1),
            number("2", 5, 6),
            number("3", 10, 11),
            number("4", 14, 15),
            bitwise_xor(12, 13),
            bitwise_and(7, 8),
            bitwise_or(2, 3),
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
            number("1", 0, 1),
            number("2", 5, 6),
            ident("f", 10, 11),
            pipe(7, 9),
            pipe(2, 4),
            ident("x", 12, 13),
            Node::Call(Some(Token {
                lexigram: Lexigram::Ident("x"),
                start: 12,
                end: 13,
            })),
        ]),
        ..Default::default()
    };
    assert_eq!(actual, expected);
}
