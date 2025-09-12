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
token!(COMMA: Comma = ",");
token!(DOT: Dot = ".");
token!(ELSE: Else = "else");
token!(END: End = "end");
token!(ENUM: Enum = "enum");
token!(IF: If = "if");
token!(LET: Let = "let");
token!(MATCH: Match = "match");
token!(OPEN_PAREN: OpenParen = "(");
token!(CLOSE_PAREN: CloseParen= ")");
token!(OPEN_BRACE: OpenBrace = "{");
token!(CLOSE_BRACE: CloseBrace = "}");
token!(THEN: Then = "then");
token!(WITH: With = "with");
token!(DOUBLE_ARROW: DoubleArrow = "=>");
token!(SEMICOLON: Semicolon = ";");
token!(SINGLE_EQUAL: SingleEqual = "=");

fn ident(origin: &str) -> Token<'_> {
    Token {
        origin,
        lexigram: Lexigram::Ident,
    }
}

fn number(origin: &str) -> Token<'_> {
    Token {
        origin,
        lexigram: Lexigram::Number,
    }
}

macro_rules! node {
    ($name:ident: $node:ident = $origin:literal as $lexigram:ident) => {
        const $name: Node = Node::$node(Token {
            origin: $origin,
            lexigram: Lexigram::$lexigram,
        });
    };
}

fn number_node(origin: &str) -> Node<'_> {
    Node::Number(number(origin))
}

fn variable(origin: &str) -> Node<'_> {
    Node::Variable(ident(origin))
}

node!(PIPE: Pipe = "|>" as Triangle);
node!(MUL: Mul = "*" as Star);
node!(BITWISE_AND: BitwiseAnd = "&" as Ampersand);
node!(BITWISE_XOR: BitwiseXor = "^" as Caret);
node!(BITWISE_OR: BitwiseOr = "|" as Pipe);
node!(TUPLE: Tuple = "," as Comma);

fn binding<'source>(
    origin: &'source str,
    expression: Box<Expression<'source>>,
) -> Statement<'source> {
    Statement::Evaluation(Evaluation {
        binding: Some(LetBinding {
            let_token: LET,
            binding: Some(Binding {
                method: BindingMethod::Single(ident(origin)),
                diagnostics: Diagnostics::default(),
            }),
            equals_token: Some(SINGLE_EQUAL),
        }),
        expression: Some(expression),
        semicolon_token: Some(SEMICOLON),
        diagnostics: Diagnostics::default(),
    })
}

fn expression<'source>(
    first_token: impl Into<Option<Token<'source>>>,
    last_token: impl Into<Option<Token<'source>>>,
    contents: impl ExactSizeIterator<Item = Node<'source>>,
) -> Box<Expression<'source>> {
    Expression::build(
        first_token.into(),
        last_token.into(),
        Diagnostics::default(),
        contents,
    )
}

fn statements<'source>(
    statements: impl ExactSizeIterator<Item = Statement<'source>>,
) -> Box<Block<'source>> {
    Block::build(
        BlockResult::Expression(None),
        Diagnostics::default(),
        statements,
    )
}

fn result(expression: Box<Expression<'_>>) -> Box<Block<'_>> {
    Block::build(expression.into(), Diagnostics::default(), [])
}

#[test]
fn named_tuple() {
    let source = "x: 1, y: 2";
    let actual = Block::new(&mut Lexer::from(source).peekable());
    let expected = result(expression(
        ident("x"),
        number("2"),
        [
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
        ]
        .into_iter(),
    ));
    assert_eq!(actual, expected);
}

#[test]
fn block_expression() {
    let source = "let x = { let y = 2; y * 3 };";
    let actual = Block::new(&mut Lexer::from(source).peekable());
    let expected = statements(
        [binding(
            "x",
            expression(
                OPEN_BRACE,
                CLOSE_BRACE,
                [Node::Block(Block::build(
                    expression(
                        ident("y"),
                        number("3"),
                        [variable("y"), number_node("3"), MUL].into_iter(),
                    )
                    .into(),
                    Diagnostics::default(),
                    [binding(
                        "y",
                        expression(number("2"), number("2"), [number_node("2")].into_iter()),
                    )],
                ))]
                .into_iter(),
            ),
        )]
        .into_iter(),
    );
    assert_eq!(actual, expected);
}

#[test]
fn if_expression() {
    let source = "let x = if condition then 1 else then 2 end;";
    let actual = Block::new(&mut Lexer::from(source).peekable());
    let expected = statements(
        [binding(
            "x",
            expression(
                IF,
                END,
                [If {
                    if_token: IF,
                    condition: Some(expression(
                        ident("condition"),
                        ident("condition"),
                        [variable("condition")].into_iter(),
                    )),
                    then_token: Some(THEN),
                    first: result(expression(
                        number("1"),
                        number("1"),
                        [number_node("1")].into_iter(),
                    )),
                    else_token: Some(ELSE),
                    else_kind: Some(THEN),
                    second: result(expression(
                        number("2"),
                        number("2"),
                        [number_node("2")].into_iter(),
                    )),
                    end_token: Some(END),
                    diagnostics: Diagnostics::default(),
                }
                .into()]
                .into_iter(),
            ),
        )]
        .into_iter(),
    );
    assert_eq!(actual, expected);
}

#[test]
fn if_else() {
    let source = "let x = if condition then 1 else if other then 2 else then 3 end;";
    let actual = Block::new(&mut Lexer::from(source).peekable());
    let expected = statements(
        [binding(
            "x",
            expression(
                IF,
                END,
                [If {
                    if_token: IF,
                    condition: Some(expression(
                        ident("condition"),
                        ident("condition"),
                        [variable("condition")].into_iter(),
                    )),
                    then_token: Some(THEN),
                    first: result(expression(
                        number("1"),
                        number("1"),
                        [number_node("1")].into_iter(),
                    )),
                    else_token: Some(ELSE),
                    else_kind: Some(IF),
                    second: result(expression(
                        None,
                        None,
                        [If {
                            if_token: IF,
                            condition: Some(expression(
                                ident("other"),
                                ident("other"),
                                [variable("other")].into_iter(),
                            )),
                            then_token: Some(THEN),
                            first: result(expression(
                                number("2"),
                                number("2"),
                                [number_node("2")].into_iter(),
                            )),
                            else_token: Some(ELSE),
                            else_kind: Some(THEN),
                            second: result(expression(
                                number("3"),
                                number("3"),
                                [number_node("3")].into_iter(),
                            )),
                            end_token: Some(END),
                            diagnostics: Diagnostics::default(),
                        }
                        .into()]
                        .into_iter(),
                    )),
                    end_token: Some(END),
                    diagnostics: Diagnostics::default(),
                }
                .into()]
                .into_iter(),
            ),
        )]
        .into_iter(),
    );
    assert_eq!(actual, expected);
}

#[test]
fn incomplete_expression() {
    let source = "1 * 2,";
    let actual = Block::new(&mut Lexer::from(source).peekable());
    let expected = result(Expression::build(
        Some(number("1")),
        Some(COMMA),
        Diagnostics {
            errors: vec![Error::IncompleteExpression],
        },
        [number_node("1"), number_node("2"), MUL],
    ));
    assert_eq!(actual, expected);
}

#[test]
fn malformed_binding() {
    let source = "let x 2";
    let actual = Block::new(&mut Lexer::from(source).peekable());
    let expected = statements(
        [Statement::Evaluation(Evaluation {
            binding: Some(LetBinding {
                let_token: LET,
                binding: Some(Binding {
                    method: BindingMethod::Single(ident("x")),
                    diagnostics: Diagnostics::default(),
                }),
                equals_token: None,
            }),
            // Despite being malformed, this expression is still parsed correctly!
            expression: Some(expression(
                number("2"),
                number("2"),
                [number_node("2")].into_iter(),
            )),
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
        })]
        .into_iter(),
    );
    assert_eq!(actual, expected);
}

#[test]
fn bitwise_operators() {
    let source = "1 | 2 & 3 ^ 4";
    let actual = Block::new(&mut Lexer::from(source).peekable());
    let expected = result(expression(
        number("1"),
        number("4"),
        [
            number_node("1"),
            number_node("2"),
            number_node("3"),
            BITWISE_AND,
            number_node("4"),
            BITWISE_XOR,
            BITWISE_OR,
        ]
        .into_iter(),
    ));
    assert_eq!(actual, expected);
}

#[test]
fn nested_parens() {
    let source = "1 | (2 & (3 ^ 4))";
    let actual = Block::new(&mut Lexer::from(source).peekable());
    let expected = result(expression(
        number("1"),
        CLOSE_PAREN,
        [
            number_node("1"),
            number_node("2"),
            number_node("3"),
            number_node("4"),
            BITWISE_XOR,
            BITWISE_AND,
            BITWISE_OR,
        ]
        .into_iter(),
    ));
    assert_eq!(actual, expected);
}

#[test]
fn pipe_operator() {
    let source = "square 2 |> add 4 |> square ()";
    let actual = Block::new(&mut Lexer::from(source).peekable());
    let expected = result(expression(
        ident("square"),
        CLOSE_PAREN,
        [
            variable("square"),
            number_node("2"),
            Node::Call(number("2")),
            variable("add"),
            PIPE,
            number_node("4"),
            Node::Call(number("4")),
            variable("square"),
            PIPE,
            Node::Unit(OPEN_PAREN, CLOSE_PAREN),
            Node::Call(OPEN_PAREN),
        ]
        .into_iter(),
    ));
    assert_eq!(actual, expected);
}

#[test]
fn function() {
    let source = "let captured = 1; with x; x * captured";
    let actual = Block::new(&mut Lexer::from(source).peekable());
    let expected = Block::build(
        Function {
            with_token: WITH,
            argument: Some(Binding {
                method: BindingMethod::Single(ident("x")),
                diagnostics: Diagnostics::default(),
            }),
            colon_token: None,
            input: None,
            single_arrow_token: None,
            output: None,
            semicolon_token: Some(SEMICOLON),
            body: FunctionBody::Block(result(expression(
                ident("x"),
                ident("captured"),
                [variable("x"), variable("captured"), MUL].into_iter(),
            ))),
            diagnostics: Diagnostics::default(),
        }
        .into(),
        Diagnostics::default(),
        [binding(
            "captured",
            expression(number("1"), number("1"), [number_node("1")].into_iter()),
        )],
    );
    assert_eq!(actual, expected);
}

#[test]
fn match_expression() {
    let source = "match 0 then let x = 1 => x * 2; 3 => 4; end";
    let actual = Block::new(&mut Lexer::from(source).peekable());
    let expected = result(expression(
        MATCH,
        END,
        [Match::build(
            MATCH,
            Some(expression(
                number("0"),
                number("0"),
                [number_node("0")].into_iter(),
            )),
            Some(THEN),
            Some(END),
            Diagnostics::default(),
            [
                MatchCase {
                    let_token: Some(LET),
                    binding: Some(Binding {
                        method: BindingMethod::Single(ident("x")),
                        diagnostics: Diagnostics::default(),
                    }),
                    equals_token: Some(SINGLE_EQUAL),
                    case: expression(number("1"), number("1"), [number_node("1")].into_iter())
                        .into(),
                    arrow_token: Some(DOUBLE_ARROW),
                    expression: Some(expression(
                        ident("x"),
                        number("2"),
                        [variable("x"), number_node("2"), MUL].into_iter(),
                    )),
                    semicolon_token: Some(SEMICOLON),
                },
                MatchCase {
                    let_token: None,
                    binding: None,
                    equals_token: None,
                    case: expression(number("3"), number("3"), [number_node("3")].into_iter())
                        .into(),
                    arrow_token: Some(DOUBLE_ARROW),
                    expression: Some(expression(
                        number("4"),
                        number("4"),
                        [number_node("4")].into_iter(),
                    )),
                    semicolon_token: Some(SEMICOLON),
                },
            ],
        )
        .into()]
        .into_iter(),
    ));
    assert_eq!(actual, expected);
}

#[test]
fn enum_creation() {
    let source = "let Option = enum Some: any, None: () end;";
    let actual = Block::new(&mut Lexer::from(source).peekable());
    let expected = statements(
        [binding(
            "Option",
            expression(
                ENUM,
                END,
                [Node::Enum(Box::new(Enum {
                    enum_token: ENUM,
                    variants: Some(expression(
                        ident("Some"),
                        CLOSE_PAREN,
                        [
                            variable("any"),
                            Node::Name {
                                name: ident("Some"),
                                colon_token: COLON,
                            },
                            Node::Unit(OPEN_PAREN, CLOSE_PAREN),
                            Node::Name {
                                name: ident("None"),
                                colon_token: COLON,
                            },
                            TUPLE,
                        ]
                        .into_iter(),
                    )),
                    end_token: Some(END),
                    diagnostics: Diagnostics::default(),
                }))]
                .into_iter(),
            ),
        )]
        .into_iter(),
    );
    assert_eq!(actual, expected);
}

#[test]
fn field_precedence() {
    let source = "something.field |> Iterator.next ()";
    let actual = Block::new(&mut Lexer::from(source).peekable());
    let expected = result(expression(
        ident("something"),
        CLOSE_PAREN,
        [
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
            Node::Unit(OPEN_PAREN, CLOSE_PAREN),
            Node::Call(OPEN_PAREN),
        ]
        .into_iter(),
    ));
    assert_eq!(actual, expected);
}

#[test]
fn tuple_indexing() {
    let source = "let x = 1, 2; x.1";
    let actual = Block::new(&mut Lexer::from(source).peekable());
    let expected = Block::build(
        expression(
            ident("x"),
            number("1"),
            [
                variable("x"),
                Node::Field {
                    dot_token: DOT,
                    index: number("1"),
                },
            ]
            .into_iter(),
        )
        .into(),
        Diagnostics::default(),
        [binding(
            "x",
            expression(
                number("1"),
                number("2"),
                [number_node("1"), number_node("2"), TUPLE].into_iter(),
            ),
        )],
    );
    assert_eq!(actual, expected);
}

#[test]
fn string() {
    let source = "\"string\"";
    let actual = Block::new(&mut Lexer::from(source).peekable());
    let expected = result(expression(
        Token {
            origin: "\"string\"",
            lexigram: Lexigram::String,
        },
        Token {
            origin: "\"string\"",
            lexigram: Lexigram::String,
        },
        [Node::String(Token {
            origin: "\"string\"",
            lexigram: Lexigram::String,
        })]
        .into_iter(),
    ));
    assert_eq!(actual, expected);
}
