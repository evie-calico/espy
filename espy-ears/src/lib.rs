use espy_eyes::{self as lexer, Lexer, Lexigram, Token};
use std::iter::Peekable;

#[cfg(test)]
mod tests;

#[derive(Debug, Eq, PartialEq)]
pub enum Error<'source> {
    Lexer(lexer::Error<'source>),
    MissingToken {
        expected: &'static [Lexigram],
        actual: Option<Token<'source>>,
    },
    UnexpectedCloseParen(Option<Token<'source>>),
    IncompleteExpression,
}

#[derive(Debug, Eq, PartialEq)]
pub enum Diagnostic<'source> {
    Error(Error<'source>),
}

#[derive(Debug, Default, Eq, PartialEq)]
pub struct Diagnostics<'source> {
    pub errors: Vec<Diagnostic<'source>>,
}

impl<'source> Diagnostics<'source> {
    fn expect(
        &mut self,
        t: Option<lexer::Result<'source>>,
        expected: &'static [Lexigram],
    ) -> Option<Token<'source>> {
        let actual = self.wrap(t);
        if actual.is_some_and(|actual| expected.contains(&actual.lexigram)) {
            actual
        } else {
            self.errors
                .push(Diagnostic::Error(Error::MissingToken { expected, actual }));
            None
        }
    }

    fn next_if(
        &mut self,
        lexer: &mut Peekable<Lexer<'source>>,
        expected: &'static [Lexigram],
    ) -> Option<Token<'source>> {
        self.expect(lexer.peek().copied(), expected).inspect(|_| {
            lexer.next();
        })
    }

    fn wrap(&mut self, t: Option<lexer::Result<'source>>) -> Option<Token<'source>> {
        match t? {
            Ok(t) => Some(t),
            Err(e) => {
                let t = if let lexer::Error {
                    origin,
                    kind: lexer::ErrorKind::ReservedSymbol,
                } = e
                {
                    Some(Token {
                        origin,
                        lexigram: Lexigram::Ident,
                    })
                } else {
                    None
                };
                self.errors.push(Diagnostic::Error(Error::Lexer(e)));
                t
            }
        }
    }
}

#[derive(Debug, Eq, PartialEq)]
pub enum Node<'source> {
    Unit,
    Number(Token<'source>),
    Variable(Token<'source>),
    Bool(bool, Option<Token<'source>>),
    Block(Block<'source>),
    If(If<'source>),
    Match(Match<'source>),
    Struct(Struct<'source>),
    Enum(Enum<'source>),

    Pipe(Option<Token<'source>>),
    Call(Option<Token<'source>>),
    Positive(Option<Token<'source>>),
    Negative(Option<Token<'source>>),
    Mul(Option<Token<'source>>),
    Div(Option<Token<'source>>),
    Add(Option<Token<'source>>),
    Sub(Option<Token<'source>>),
    BitwiseAnd(Option<Token<'source>>),
    BitwiseOr(Option<Token<'source>>),
    BitwiseXor(Option<Token<'source>>),
    EqualTo(Option<Token<'source>>),
    NotEqualTo(Option<Token<'source>>),
    Greater(Option<Token<'source>>),
    GreaterEqual(Option<Token<'source>>),
    Lesser(Option<Token<'source>>),
    LesserEqual(Option<Token<'source>>),
    LogicalAnd(Option<Token<'source>>),
    LogicalOr(Option<Token<'source>>),
    Name {
        name: Token<'source>,
        colon_token: Token<'source>,
    },
    Field {
        dot_token: Token<'source>,
        index: Token<'source>,
    },
    Tuple(Option<Token<'source>>),
}

/// This type must not contain any incomplete expressions.
#[derive(Debug, Default, Eq, PartialEq)]
pub struct Expression<'source> {
    pub contents: Vec<Node<'source>>,
    pub diagnostics: Diagnostics<'source>,
}

/// Parse an expression until an unexpected token is upcoming (via peek).
impl<'source> From<&mut Peekable<Lexer<'source>>> for Expression<'source> {
    fn from(lexer: &mut Peekable<Lexer<'source>>) -> Self {
        #[derive(Clone, Copy, Debug, Eq, PartialEq)]
        enum Operation<'source> {
            Call(Option<Token<'source>>),
            Pipe(Option<Token<'source>>),
            Positive(Option<Token<'source>>),
            Negative(Option<Token<'source>>),
            Mul(Option<Token<'source>>),
            Div(Option<Token<'source>>),
            Add(Option<Token<'source>>),
            Sub(Option<Token<'source>>),
            BitwiseAnd(Option<Token<'source>>),
            BitwiseXor(Option<Token<'source>>),
            BitwiseOr(Option<Token<'source>>),
            EqualTo(Option<Token<'source>>),
            NotEqualTo(Option<Token<'source>>),
            Greater(Option<Token<'source>>),
            GreaterEqual(Option<Token<'source>>),
            Lesser(Option<Token<'source>>),
            LesserEqual(Option<Token<'source>>),
            LogicalAnd(Option<Token<'source>>),
            LogicalOr(Option<Token<'source>>),
            Name {
                name: Token<'source>,
                colon_token: Token<'source>,
            },
            Field {
                dot_token: Token<'source>,
                index: Token<'source>,
            },
            Tuple(Option<Token<'source>>),
            SubExpression(Option<Token<'source>>),
        }

        impl<'source> Operation<'source> {
            fn precedence(self) -> usize {
                match self {
                    Operation::Field { .. } => 14,
                    Operation::Pipe(_) => 13,
                    Operation::Call(_) => 12,
                    Operation::Positive(_) | Operation::Negative(_) => 11,
                    Operation::Mul(_) | Operation::Div(_) => 10,
                    Operation::Add(_) | Operation::Sub(_) => 9,
                    Operation::BitwiseAnd(_) => 8,
                    Operation::BitwiseXor(_) => 7,
                    Operation::BitwiseOr(_) => 6,
                    Operation::EqualTo(_)
                    | Operation::NotEqualTo(_)
                    | Operation::Greater(_)
                    | Operation::GreaterEqual(_)
                    | Operation::Lesser(_)
                    | Operation::LesserEqual(_) => 5,
                    Operation::LogicalAnd(_) => 4,
                    Operation::LogicalOr(_) => 3,
                    Operation::Name { .. } => 2,
                    Operation::Tuple(_) => 1,
                    Operation::SubExpression(_) => 0,
                }
            }

            fn left_associative(self) -> bool {
                match self {
                    Operation::Field { .. }
                    | Operation::Call(_)
                    | Operation::Positive(_)
                    | Operation::Negative(_)
                    | Operation::Mul(_)
                    | Operation::Div(_)
                    | Operation::Name { .. }
                    | Operation::Add(_)
                    | Operation::Sub(_)
                    | Operation::BitwiseAnd(_)
                    | Operation::BitwiseXor(_)
                    | Operation::BitwiseOr(_)
                    | Operation::EqualTo(_)
                    | Operation::NotEqualTo(_)
                    | Operation::Greater(_)
                    | Operation::GreaterEqual(_)
                    | Operation::Lesser(_)
                    | Operation::LesserEqual(_)
                    | Operation::LogicalAnd(_)
                    | Operation::LogicalOr(_)
                    | Operation::Tuple(_)
                    | Operation::SubExpression(_) => true,
                    Operation::Pipe(_) => false,
                }
            }
        }

        impl<'source> From<Operation<'source>> for Node<'source> {
            fn from(op: Operation<'source>) -> Self {
                match op {
                    Operation::Field { dot_token, index } => Node::Field { dot_token, index },
                    Operation::Pipe(t) => Node::Pipe(t),
                    Operation::Call(t) => Node::Call(t),
                    Operation::Positive(t) => Node::Positive(t),
                    Operation::Negative(t) => Node::Negative(t),
                    Operation::Mul(t) => Node::Mul(t),
                    Operation::Div(t) => Node::Div(t),
                    Operation::Add(t) => Node::Add(t),
                    Operation::Sub(t) => Node::Sub(t),
                    Operation::BitwiseAnd(t) => Node::BitwiseAnd(t),
                    Operation::BitwiseXor(t) => Node::BitwiseXor(t),
                    Operation::BitwiseOr(t) => Node::BitwiseOr(t),
                    Operation::EqualTo(t) => Node::EqualTo(t),
                    Operation::NotEqualTo(t) => Node::NotEqualTo(t),
                    Operation::Greater(t) => Node::Greater(t),
                    Operation::GreaterEqual(t) => Node::GreaterEqual(t),
                    Operation::Lesser(t) => Node::Lesser(t),
                    Operation::LesserEqual(t) => Node::LesserEqual(t),
                    Operation::LogicalAnd(t) => Node::LogicalAnd(t),
                    Operation::LogicalOr(t) => Node::LogicalOr(t),
                    Operation::Name { name, colon_token } => Node::Name { name, colon_token },
                    Operation::Tuple(t) => Node::Tuple(t),
                    Operation::SubExpression(_) => {
                        panic!("sub expressions may not enter the output stack")
                    }
                }
            }
        }

        let mut diagnostics = Diagnostics::default();
        let mut contents = Vec::new();
        let mut stack = Vec::new();
        let mut last_token = None;
        // Check if the last token implies the unary position.
        // This is probably not the best way to do things.
        let unary_position = |last_token| {
            matches!(
                last_token,
                None | Some(Token {
                    lexigram: Lexigram::Plus
                        | Lexigram::Minus
                        | Lexigram::Star
                        | Lexigram::Slash
                        | Lexigram::Ampersand
                        | Lexigram::Caret
                        | Lexigram::Pipe
                        | Lexigram::DoubleEqual
                        | Lexigram::BangEqual
                        | Lexigram::Greater
                        | Lexigram::GreaterEqual
                        | Lexigram::Lesser
                        | Lexigram::LesserEqual
                        | Lexigram::And
                        | Lexigram::Or
                        | Lexigram::Comma
                        | Lexigram::Colon
                        | Lexigram::Triangle
                        | Lexigram::OpenParen,
                    ..
                })
            )
        };
        let flush = |output: &mut Vec<Node<'source>>, stack: &mut Vec<Operation<'source>>| {
            while let Some(op) = stack.pop_if(|x| !matches!(x, Operation::SubExpression(_))) {
                output.push(op.into());
            }
        };
        let push_with_precedence =
            |output: &mut Vec<Node<'source>>,
             stack: &mut Vec<Operation<'source>>,
             operator: Operation<'source>| {
                while let Some(op) = stack.pop_if(|x| {
                    if operator.left_associative() {
                        x.precedence() >= operator.precedence()
                    } else {
                        x.precedence() > operator.precedence()
                    }
                }) {
                    // SubExpression has the lowest precedence, so this cannot panic.
                    output.push(op.into());
                }
                stack.push(operator);
            };
        loop {
            let unary_position = unary_position(last_token);
            let t = diagnostics.wrap(lexer.peek().copied());
            macro_rules! lexi {
                ($lexi:ident) => {
                    Some(Token {
                        lexigram: Lexigram::$lexi,
                        ..
                    })
                };
            }
            macro_rules! op {
                ($op:ident) => {
                    push_with_precedence(&mut contents, &mut stack, Operation::$op(t))
                };
            }
            match t {
                // Terminals
                //
                // A terminal value outside of unary position implies a function call,
                // so flush the operator stack in this case.
                Some(
                    number @ Token {
                        lexigram: Lexigram::Number,
                        ..
                    },
                ) => {
                    if !unary_position {
                        push_with_precedence(&mut contents, &mut stack, Operation::Call(t));
                    }
                    contents.push(Node::Number(number));
                }
                Some(
                    ident @ Token {
                        lexigram: Lexigram::Ident,
                        ..
                    },
                ) => {
                    if !unary_position {
                        push_with_precedence(&mut contents, &mut stack, Operation::Call(t));
                    }
                    last_token = lexer.next().transpose().ok().flatten();
                    if let Some(Ok(
                        colon_token @ Token {
                            lexigram: Lexigram::Colon,
                            ..
                        },
                    )) = lexer.peek().copied()
                    {
                        last_token = lexer.next().transpose().ok().flatten();
                        push_with_precedence(
                            &mut contents,
                            &mut stack,
                            Operation::Name {
                                name: ident,
                                colon_token,
                            },
                        );
                    } else {
                        contents.push(Node::Variable(ident));
                    }
                    continue;
                }
                Some(
                    dot_token @ Token {
                        lexigram: Lexigram::Dot,
                        ..
                    },
                ) if !unary_position => {
                    last_token = lexer.next().transpose().ok().flatten();
                    if let Some(index) =
                        diagnostics.next_if(lexer, &[Lexigram::Ident, Lexigram::Number])
                    {
                        last_token = Some(index);
                        push_with_precedence(
                            &mut contents,
                            &mut stack,
                            Operation::Field { dot_token, index },
                        );
                    }
                    continue;
                }
                Some(Token {
                    lexigram: Lexigram::True,
                    ..
                }) => {
                    if !unary_position {
                        push_with_precedence(&mut contents, &mut stack, Operation::Call(t));
                    }
                    contents.push(Node::Bool(true, t));
                }
                Some(Token {
                    lexigram: Lexigram::False,
                    ..
                }) => {
                    if !unary_position {
                        push_with_precedence(&mut contents, &mut stack, Operation::Call(t));
                    }
                    contents.push(Node::Bool(false, t));
                }
                lexi!(OpenParen) => {
                    if !unary_position {
                        push_with_precedence(&mut contents, &mut stack, Operation::Call(t));
                    }
                    stack.push(Operation::SubExpression(t));
                }
                lexi!(OpenBrace) => {
                    if !unary_position {
                        push_with_precedence(&mut contents, &mut stack, Operation::Call(t));
                    }
                    lexer.next();
                    contents.push(Node::Block(Block::from(&mut *lexer)));
                    diagnostics.expect(lexer.peek().copied(), &[Lexigram::CloseBrace]);
                }

                // # Operators
                lexi!(Plus) if unary_position => op!(Positive),
                lexi!(Plus) if !unary_position => op!(Add),
                lexi!(Minus) if unary_position => op!(Negative),
                lexi!(Minus) if !unary_position => op!(Sub),
                lexi!(Star) if !unary_position => op!(Mul),
                lexi!(Slash) if !unary_position => op!(Div),
                lexi!(Ampersand) if !unary_position => op!(BitwiseAnd),
                lexi!(Caret) if !unary_position => op!(BitwiseXor),
                lexi!(Pipe) if !unary_position => op!(BitwiseOr),
                lexi!(DoubleEqual) if !unary_position => op!(EqualTo),
                lexi!(BangEqual) if !unary_position => op!(NotEqualTo),
                lexi!(Greater) if !unary_position => op!(Greater),
                lexi!(GreaterEqual) if !unary_position => op!(GreaterEqual),
                lexi!(Lesser) if !unary_position => op!(Lesser),
                lexi!(LesserEqual) if !unary_position => op!(LesserEqual),
                lexi!(And) if !unary_position => op!(LogicalAnd),
                lexi!(Or) if !unary_position => op!(LogicalOr),
                lexi!(Triangle) if !unary_position => op!(Pipe),
                lexi!(Comma) if !unary_position => op!(Tuple),
                lexi!(CloseParen) if unary_position => {
                    if matches!(
                        last_token,
                        Some(Token {
                            lexigram: Lexigram::OpenParen,
                            ..
                        })
                    ) {
                        contents.push(Node::Unit);
                    } else {
                        diagnostics
                            .errors
                            .push(Diagnostic::Error(Error::IncompleteExpression));
                    }
                    if !matches!(stack.pop(), Some(Operation::SubExpression(_))) {
                        diagnostics
                            .errors
                            .push(Diagnostic::Error(Error::UnexpectedCloseParen(t)))
                    }
                }
                lexi!(CloseParen) if !unary_position => {
                    while let Some(op) = stack.pop_if(|x| !matches!(x, Operation::SubExpression(_)))
                    {
                        contents.push(op.into());
                    }
                    if !matches!(stack.pop(), Some(Operation::SubExpression(_))) {
                        diagnostics
                            .errors
                            .push(Diagnostic::Error(Error::UnexpectedCloseParen(t)))
                    }
                }
                lexi!(If) => {
                    contents.push(If::from(&mut *lexer).into());
                }
                lexi!(Match) => {
                    contents.push(Match::from(&mut *lexer).into());
                }
                lexi!(Struct) => {
                    contents.push(Struct::from(&mut *lexer).into());
                }
                lexi!(Enum) => {
                    contents.push(Enum::from(&mut *lexer).into());
                }
                _ if !unary_position => {
                    flush(&mut contents, &mut stack);
                    if !stack.is_empty() {
                        diagnostics.expect(None, &[Lexigram::CloseParen]);
                    }
                    return Expression {
                        contents,
                        diagnostics,
                    };
                }
                _ => {
                    if unary_position {
                        if !contents.is_empty() || !stack.is_empty() {
                            diagnostics
                                .errors
                                .push(Diagnostic::Error(Error::IncompleteExpression));
                        }
                    } else {
                        loop {
                            flush(&mut contents, &mut stack);
                            if stack.is_empty() {
                                break;
                            }
                            diagnostics.expect(None, &[Lexigram::CloseParen]);
                        }
                    }
                    return Expression {
                        contents,
                        diagnostics,
                    };
                }
            }
            // This is sometimes skipped with a continue!
            last_token = lexer.next().transpose().unwrap_or(None);
        }
    }
}

#[derive(Debug, Eq, PartialEq)]
pub struct If<'source> {
    pub if_token: Option<Token<'source>>,
    pub condition: Expression<'source>,
    pub then_token: Option<Token<'source>>,
    pub first: Block<'source>,
    pub else_token: Option<Token<'source>>,
    pub else_kind: Option<Token<'source>>,
    pub second: Block<'source>,
    pub end_token: Option<Token<'source>>,
    pub diagnostics: Diagnostics<'source>,
}

impl<'source> From<If<'source>> for Node<'source> {
    fn from(if_block: If<'source>) -> Self {
        Self::If(if_block)
    }
}

impl<'source> From<&mut Peekable<Lexer<'source>>> for If<'source> {
    fn from(lexer: &mut Peekable<Lexer<'source>>) -> Self {
        let if_token = lexer.next().transpose().ok().flatten();
        let mut diagnostics = Diagnostics::default();
        let condition = Expression::from(&mut *lexer);
        let then_token = diagnostics.next_if(lexer, &[Lexigram::Then]);
        let first = Block::from(&mut *lexer);
        let (second, else_token, else_kind) = if let else_token @ Some(Token {
            lexigram: Lexigram::Else,
            ..
        }) = diagnostics.wrap(lexer.peek().copied())
        {
            lexer.next();
            let (second, else_kind) = match diagnostics.wrap(lexer.peek().copied()) {
                else_kind @ Some(Token {
                    lexigram: Lexigram::Then,
                    ..
                }) => {
                    lexer.next();
                    (Block::from(&mut *lexer), else_kind)
                }
                else_kind @ Some(Token {
                    lexigram: Lexigram::If,
                    ..
                }) => (
                    Block {
                        statements: Vec::new(),
                        result: Expression {
                            contents: vec![Self::from(&mut *lexer).into()],
                            diagnostics: Diagnostics::default(),
                        }
                        .into(),
                        diagnostics: Diagnostics::default(),
                    },
                    else_kind,
                ),
                _ => {
                    diagnostics.expect(lexer.peek().copied(), &[Lexigram::Then, Lexigram::If]);
                    (Block::default(), None)
                }
            };
            (second, else_token, else_kind)
        } else {
            (Block::default(), None, None)
        };
        let end_token = diagnostics.expect(lexer.peek().copied(), &[Lexigram::End]);
        Self {
            if_token,
            condition,
            then_token,
            first,
            else_token,
            else_kind,
            second,
            end_token,
            diagnostics,
        }
    }
}

#[derive(Debug, Eq, PartialEq)]
pub struct MatchCase<'source> {
    let_token: Option<Token<'source>>,
    binding: Option<Token<'source>>,
    equals_token: Option<Token<'source>>,
    case: Option<Expression<'source>>,
    arrow_token: Option<Token<'source>>,
    expression: Expression<'source>,
    semicolon_token: Option<Token<'source>>,
}

#[derive(Debug, Eq, PartialEq)]
pub struct Match<'source> {
    match_token: Option<Token<'source>>,
    expression: Expression<'source>,
    then_token: Option<Token<'source>>,
    cases: Vec<MatchCase<'source>>,
    end_token: Option<Token<'source>>,
    diagnostics: Diagnostics<'source>,
}

impl<'source> From<Match<'source>> for Node<'source> {
    fn from(struct_block: Match<'source>) -> Self {
        Self::Match(struct_block)
    }
}

impl<'source> From<&mut Peekable<Lexer<'source>>> for Match<'source> {
    fn from(lexer: &mut Peekable<Lexer<'source>>) -> Self {
        let match_token = lexer.next().transpose().ok().flatten();
        let mut diagnostics = Diagnostics::default();

        let expression = Expression::from(&mut *lexer);
        let then_token = diagnostics.next_if(lexer, &[Lexigram::Then]);
        let mut cases = Vec::new();

        loop {
            let (let_token, binding, equals_token, case) = if let let_token @ Some(Token {
                lexigram: Lexigram::Let,
                ..
            }) =
                diagnostics.wrap(lexer.peek().copied())
            {
                lexer.next();
                let binding = diagnostics.next_if(lexer, &[Lexigram::Ident, Lexigram::Discard]);
                let (equal_token, case) = if let equal_token @ Some(Token {
                    lexigram: Lexigram::SingleEqual,
                    ..
                }) = diagnostics.wrap(lexer.peek().copied())
                {
                    lexer.next();
                    (equal_token, Some(Expression::from(&mut *lexer)))
                } else {
                    (None, None)
                };
                (let_token, binding, equal_token, case)
            } else {
                let case = Expression::from(&mut *lexer);
                (None, None, None, Some(case))
            };
            let arrow_token = diagnostics.next_if(lexer, &[Lexigram::DoubleArrow]);
            let expression = Expression::from(&mut *lexer);
            let semicolon_token = diagnostics.next_if(lexer, &[Lexigram::Semicolon]);
            cases.push(MatchCase {
                let_token,
                binding,
                equals_token,
                case,
                arrow_token,
                expression,
                semicolon_token,
            });
            if semicolon_token.is_none()
                || diagnostics
                    .wrap(lexer.peek().copied())
                    .is_some_and(|t| t.lexigram == Lexigram::End)
            {
                break;
            }
        }
        let end_token = diagnostics.expect(lexer.peek().copied(), &[Lexigram::End]);
        Self {
            match_token,
            expression,
            then_token,
            cases,
            end_token,
            diagnostics,
        }
    }
}

#[derive(Debug, Eq, PartialEq)]
pub struct Struct<'source> {
    struct_token: Option<Token<'source>>,
    inner: Expression<'source>,
    then_token: Option<Token<'source>>,
    block: Block<'source>,
    end_token: Option<Token<'source>>,
    diagnostics: Diagnostics<'source>,
}

impl<'source> From<Struct<'source>> for Node<'source> {
    fn from(struct_block: Struct<'source>) -> Self {
        Self::Struct(struct_block)
    }
}

impl<'source> From<&mut Peekable<Lexer<'source>>> for Struct<'source> {
    fn from(lexer: &mut Peekable<Lexer<'source>>) -> Self {
        let struct_token = lexer.next().transpose().ok().flatten();
        let mut diagnostics = Diagnostics::default();

        let inner = Expression::from(&mut *lexer);
        let then_token = diagnostics.next_if(lexer, &[Lexigram::Then]);
        let block = Block::from(&mut *lexer);
        let end_token = diagnostics.expect(lexer.peek().copied(), &[Lexigram::End]);
        Self {
            struct_token,
            inner,
            then_token,
            block,
            end_token,
            diagnostics,
        }
    }
}

#[derive(Debug, Eq, PartialEq)]
pub struct Enum<'source> {
    pub enum_token: Option<Token<'source>>,
    pub then_token: Option<Token<'source>>,
    pub block: Block<'source>,
    pub end_token: Option<Token<'source>>,
    pub diagnostics: Diagnostics<'source>,
}

impl<'source> From<Enum<'source>> for Node<'source> {
    fn from(struct_block: Enum<'source>) -> Self {
        Self::Enum(struct_block)
    }
}

impl<'source> From<&mut Peekable<Lexer<'source>>> for Enum<'source> {
    fn from(lexer: &mut Peekable<Lexer<'source>>) -> Self {
        let enum_token = lexer.next().transpose().ok().flatten();
        let mut diagnostics = Diagnostics::default();
        let then_token = diagnostics.next_if(lexer, &[Lexigram::Then]);
        let block = Block::from(&mut *lexer);
        let end_token = diagnostics.expect(lexer.peek().copied(), &[Lexigram::End]);
        Self {
            enum_token,
            then_token,
            block,
            end_token,
            diagnostics,
        }
    }
}

#[derive(Debug, Eq, PartialEq)]
pub struct Binding<'source> {
    pub let_token: Token<'source>,
    pub ident_token: Option<Token<'source>>,
    pub colon_token: Option<Token<'source>>,
    pub ty_token: Option<Token<'source>>,
    pub equals_token: Option<Token<'source>>,
}

#[derive(Debug, Eq, PartialEq)]
pub struct For<'source> {
    pub for_token: Option<Token<'source>>,
    pub binding: Option<Token<'source>>,
    pub in_token: Option<Token<'source>>,
    pub iterator: Expression<'source>,
    pub then_token: Option<Token<'source>>,
    pub block: Block<'source>,
    pub end_token: Option<Token<'source>>,
    pub diagnostics: Diagnostics<'source>,
}

impl<'source> From<&mut Peekable<Lexer<'source>>> for For<'source> {
    fn from(lexer: &mut Peekable<Lexer<'source>>) -> Self {
        let for_token = lexer.next().transpose().ok().flatten();
        let mut diagnostics = Diagnostics::default();

        let binding = diagnostics.next_if(lexer, &[Lexigram::Ident, Lexigram::Discard]);
        let in_token = diagnostics.next_if(lexer, &[Lexigram::In]);
        let iterator = Expression::from(&mut *lexer);
        let then_token = diagnostics.next_if(lexer, &[Lexigram::Then]);
        let first = Block::from(&mut *lexer);
        let end_token = diagnostics.next_if(lexer, &[Lexigram::End]);

        Self {
            for_token,
            binding,
            in_token,
            iterator,
            then_token,
            block: first,
            end_token,
            diagnostics,
        }
    }
}

#[derive(Debug, Eq, PartialEq)]
pub struct Implementation<'source> {
    pub impl_token: Token<'source>,
    pub trait_expression: Expression<'source>,
    pub for_token: Option<Token<'source>>,
    pub struct_expression: Expression<'source>,
    pub then_token: Option<Token<'source>>,
    pub block: Block<'source>,
    pub end_token: Option<Token<'source>>,
    pub diagnostics: Diagnostics<'source>,
}

impl<'source> From<&mut Peekable<Lexer<'source>>> for Implementation<'source> {
    fn from(lexer: &mut Peekable<Lexer<'source>>) -> Self {
        let mut diagnostics = Diagnostics::default();
        let impl_token = lexer
            .next()
            .transpose()
            .ok()
            .flatten()
            .expect("caller must have peeked an impl token");
        let trait_expression = Expression::from(&mut *lexer);
        let for_token = diagnostics.next_if(lexer, &[Lexigram::For]);
        let struct_expression = Expression::from(&mut *lexer);
        let then_token = diagnostics.next_if(lexer, &[Lexigram::Then]);
        let block = Block::from(&mut *lexer);
        let end_token = diagnostics.next_if(lexer, &[Lexigram::End]);
        Self {
            impl_token,
            trait_expression,
            for_token,
            struct_expression,
            then_token,
            block,
            end_token,
            diagnostics,
        }
    }
}

#[derive(Debug, Eq, PartialEq)]
pub enum Action<'source> {
    Evaluate(Option<Binding<'source>>, Option<Expression<'source>>),
    For(For<'source>),
    Implementation(Implementation<'source>),
}

#[derive(Debug, Eq, PartialEq)]
pub struct Statement<'source> {
    pub action: Action<'source>,
    pub semicolon_token: Option<Token<'source>>,
    pub diagnostics: Diagnostics<'source>,
}

#[derive(Debug, Eq, PartialEq)]
pub struct Function<'source> {
    pub with_token: Option<Token<'source>>,
    pub arguments: Vec<Token<'source>>,
    pub semicolon_token: Option<Token<'source>>,
    pub block: Block<'source>,
    pub diagnostics: Diagnostics<'source>,
}

#[derive(Debug, Eq, PartialEq)]
pub enum BlockResult<'source> {
    Expression(Expression<'source>),
    Break {
        break_token: Token<'source>,
        expression: Expression<'source>,
    },
    // This box resolves the recursive relationship between block -> result -> function -> block.
    // You could also put it on `Function`'s `block`, but putting it here shrinks the enum.
    Function(Box<Function<'source>>),
}

impl BlockResult<'_> {
    pub fn is_empty(&self) -> bool {
        match self {
            BlockResult::Expression(expression) => expression.contents.is_empty(),
            BlockResult::Break { .. } | BlockResult::Function(_) => false,
        }
    }
}

impl Default for BlockResult<'_> {
    fn default() -> Self {
        Self::Expression(Expression::default())
    }
}

impl<'source> From<Expression<'source>> for BlockResult<'source> {
    fn from(expression: Expression<'source>) -> Self {
        Self::Expression(expression)
    }
}

impl<'source> From<Function<'source>> for BlockResult<'source> {
    fn from(function: Function<'source>) -> Self {
        Self::Function(Box::new(function))
    }
}

#[derive(Debug, Default, Eq, PartialEq)]
pub struct Block<'source> {
    pub statements: Vec<Statement<'source>>,
    pub result: BlockResult<'source>,
    pub diagnostics: Diagnostics<'source>,
}

impl Block<'_> {
    pub fn is_empty(&self) -> bool {
        self.statements.is_empty() && self.result.is_empty()
    }
}

impl<'source> From<&mut Peekable<Lexer<'source>>> for Block<'source> {
    fn from(lexer: &mut Peekable<Lexer<'source>>) -> Self {
        let mut diagnostics = Diagnostics::default();
        let mut statements = Vec::new();
        loop {
            let statement = match diagnostics.wrap(lexer.peek().copied()) {
                Some(
                    let_token @ Token {
                        lexigram: Lexigram::Let,
                        ..
                    },
                ) => {
                    lexer.next();
                    let mut st_diagnostics = Diagnostics::default();
                    let token = st_diagnostics.wrap(lexer.next());
                    if let Some(Token {
                        lexigram: Lexigram::Ident,
                        ..
                    }) = token
                    {
                        let ident_token = token;
                        match st_diagnostics.wrap(lexer.peek().copied()) {
                            equals_token @ Some(Token {
                                lexigram: Lexigram::SingleEqual,
                                ..
                            }) => {
                                lexer.next();
                                let expression = Expression::from(&mut *lexer);
                                let semicolon_token =
                                    st_diagnostics.next_if(lexer, &[Lexigram::Semicolon]);
                                Statement {
                                    action: Action::Evaluate(
                                        Some(Binding {
                                            let_token,
                                            ident_token,
                                            colon_token: None,
                                            ty_token: None,
                                            equals_token,
                                        }),
                                        Some(expression),
                                    ),
                                    semicolon_token,
                                    diagnostics: st_diagnostics,
                                }
                            }
                            semicolon_token @ Some(Token {
                                lexigram: Lexigram::Semicolon,
                                ..
                            }) => {
                                lexer.next();
                                Statement {
                                    action: Action::Evaluate(
                                        Some(Binding {
                                            let_token,
                                            ident_token,
                                            colon_token: None,
                                            ty_token: None,
                                            equals_token: None,
                                        }),
                                        None,
                                    ),
                                    semicolon_token,
                                    diagnostics: st_diagnostics,
                                }
                            }
                            _ => {
                                st_diagnostics.expect(
                                    lexer.peek().copied(),
                                    &[Lexigram::SingleEqual, Lexigram::Semicolon],
                                );
                                Statement {
                                    action: Action::Evaluate(
                                        Some(Binding {
                                            let_token,
                                            ident_token,
                                            colon_token: None,
                                            ty_token: None,
                                            equals_token: None,
                                        }),
                                        None,
                                    ),
                                    semicolon_token: None,
                                    diagnostics: st_diagnostics,
                                }
                            }
                        }
                    } else {
                        // The ident field of Binding is optional,
                        // so we could potentially keep parsing if the equal sign or semicolon is present.
                        st_diagnostics
                            .errors
                            .push(Diagnostic::Error(Error::MissingToken {
                                expected: &[Lexigram::Ident],
                                actual: token,
                            }));
                        Statement {
                            action: Action::Evaluate(
                                Some(Binding {
                                    let_token,
                                    ident_token: None,
                                    colon_token: None,
                                    ty_token: None,
                                    equals_token: None,
                                }),
                                None,
                            ),
                            diagnostics: st_diagnostics,
                            semicolon_token: None,
                        }
                    }
                }
                Some(Token {
                    lexigram: Lexigram::For,
                    ..
                }) => {
                    // Will consume the for token
                    let for_loop = For::from(&mut *lexer);
                    let mut diagnostics = Diagnostics::default();
                    let semicolon_token = diagnostics.next_if(lexer, &[Lexigram::Semicolon]);
                    Statement {
                        action: Action::For(for_loop),
                        semicolon_token,
                        diagnostics,
                    }
                }
                Some(Token {
                    lexigram: Lexigram::Impl,
                    ..
                }) => {
                    // Will consume the impl token
                    let implementation = Implementation::from(&mut *lexer);
                    let mut diagnostics = Diagnostics::default();
                    let semicolon_token = diagnostics.next_if(lexer, &[Lexigram::Semicolon]);
                    Statement {
                        action: Action::Implementation(implementation),
                        semicolon_token,
                        diagnostics,
                    }
                }
                Some(
                    break_token @ Token {
                        lexigram: Lexigram::Break,
                        ..
                    },
                ) => {
                    lexer.next();
                    let expression = Expression::from(&mut *lexer);
                    return Self {
                        statements,
                        result: BlockResult::Break {
                            break_token,
                            expression,
                        },
                        diagnostics,
                    };
                }
                with_token @ Some(Token {
                    lexigram: Lexigram::With,
                    ..
                }) => {
                    lexer.next();
                    let mut st_diagnostics = Diagnostics::default();
                    let mut arguments = Vec::new();
                    let semicolon_token = loop {
                        if let Some(arg) =
                            st_diagnostics.expect(lexer.peek().copied(), &[Lexigram::Ident])
                        {
                            lexer.next();
                            arguments.push(arg);
                            match st_diagnostics.wrap(lexer.peek().copied()) {
                                Some(Token {
                                    lexigram: Lexigram::Comma,
                                    ..
                                }) => {
                                    lexer.next();
                                }
                                semicolon_token @ Some(Token {
                                    lexigram: Lexigram::Semicolon,
                                    ..
                                }) => {
                                    lexer.next();
                                    break semicolon_token;
                                }
                                _ => {
                                    break st_diagnostics.expect(
                                        lexer.peek().copied(),
                                        &[Lexigram::Comma, Lexigram::Semicolon],
                                    );
                                }
                            }
                        }
                    };

                    let block = Block::from(&mut *lexer);

                    return Self {
                        statements,
                        result: Function {
                            with_token,
                            arguments,
                            semicolon_token,
                            block,
                            diagnostics: st_diagnostics,
                        }
                        .into(),
                        diagnostics,
                    };
                }
                _ => {
                    let mut st_diagnostics = Diagnostics::default();
                    let expression = Expression::from(&mut *lexer);
                    let semicolon_token = st_diagnostics.wrap(lexer.peek().copied());
                    if let Some(Token {
                        lexigram: Lexigram::Semicolon,
                        ..
                    }) = semicolon_token
                    {
                        lexer.next();
                        Statement {
                            action: Action::Evaluate(None, Some(expression)),
                            diagnostics: st_diagnostics,
                            semicolon_token,
                        }
                    } else {
                        return Self {
                            statements,
                            result: expression.into(),
                            diagnostics,
                        };
                    }
                }
            };
            statements.push(statement);
        }
    }
}
