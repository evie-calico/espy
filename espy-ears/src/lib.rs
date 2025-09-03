use dst_factory::make_dst_factory;
use espy_eyes::{self as lexer, Lexer, Lexigram, Token};
use std::iter::Peekable;

#[cfg(test)]
mod tests;

#[derive(Debug, Eq, PartialEq)]
pub enum Error<'source> {
    Lexer(lexer::Error<'source>),
    MissingToken {
        /// Must contain at least one element.
        expected: &'static [Lexigram],
        actual: Option<Token<'source>>,
    },
    ExpectedExpression,
    UnexpectedCloseParen(Token<'source>),
    IncompleteExpression,
}

#[derive(Debug, Eq, PartialEq)]
pub enum Diagnostic<'source> {
    Error(Error<'source>),
}

#[derive(Debug, Default, Eq, PartialEq)]
pub struct Diagnostics<'source> {
    pub errors: Vec<Error<'source>>,
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
            self.errors.push(Error::MissingToken { expected, actual });
            None
        }
    }

    fn expect_expression(
        &mut self,
        lexer: &mut Peekable<Lexer<'source>>,
    ) -> Option<Box<Expression<'source>>> {
        let expression = Expression::new(lexer);
        if expression.is_none() {
            self.errors.push(Error::ExpectedExpression);
        }
        expression
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
                self.errors.push(Error::Lexer(e));
                t
            }
        }
    }
}

#[derive(Debug, Eq, PartialEq)]
pub enum Node<'source> {
    Unit(Token<'source>, Token<'source>),
    Bool(bool, Token<'source>),
    Number(Token<'source>),
    String(Token<'source>),
    Variable(Token<'source>),
    Block(Box<Block<'source>>),
    If(Box<If<'source>>),
    Match(Box<Match<'source>>),
    Struct(Box<Struct<'source>>),
    Enum(Box<Enum<'source>>),

    Pipe(Token<'source>),
    Call(Token<'source>),
    Positive(Token<'source>),
    Negative(Token<'source>),
    Deref(Token<'source>),
    Mul(Token<'source>),
    Div(Token<'source>),
    Add(Token<'source>),
    Sub(Token<'source>),
    BitwiseAnd(Token<'source>),
    BitwiseOr(Token<'source>),
    BitwiseXor(Token<'source>),
    EqualTo(Token<'source>),
    NotEqualTo(Token<'source>),
    Greater(Token<'source>),
    GreaterEqual(Token<'source>),
    Lesser(Token<'source>),
    LesserEqual(Token<'source>),
    LogicalAnd(Token<'source>),
    LogicalOr(Token<'source>),
    Name {
        name: Token<'source>,
        colon_token: Token<'source>,
    },
    Field {
        dot_token: Token<'source>,
        index: Token<'source>,
    },
    Tuple(Token<'source>),
}

/// This type must not contain any incomplete expressions.
#[derive(Debug, Eq, PartialEq)]
#[make_dst_factory(pub)]
pub struct Expression<'source> {
    pub first_token: Option<Token<'source>>,
    pub last_token: Option<Token<'source>>,
    pub diagnostics: Diagnostics<'source>,
    pub contents: [Node<'source>],
}

impl<'source> Expression<'source> {
    /// Parse an expression until an unexpected token is upcoming (via peek).
    pub fn new(lexer: &mut Peekable<Lexer<'source>>) -> Option<Box<Self>> {
        #[derive(Clone, Copy, Debug, Eq, PartialEq)]
        enum Operation<'source> {
            Call(Token<'source>),
            Pipe(Token<'source>),
            Positive(Token<'source>),
            Negative(Token<'source>),
            Deref(Token<'source>),
            Mul(Token<'source>),
            Div(Token<'source>),
            Add(Token<'source>),
            Sub(Token<'source>),
            BitwiseAnd(Token<'source>),
            BitwiseXor(Token<'source>),
            BitwiseOr(Token<'source>),
            EqualTo(Token<'source>),
            NotEqualTo(Token<'source>),
            Greater(Token<'source>),
            GreaterEqual(Token<'source>),
            Lesser(Token<'source>),
            LesserEqual(Token<'source>),
            LogicalAnd(Token<'source>),
            LogicalOr(Token<'source>),
            Name {
                name: Token<'source>,
                colon_token: Token<'source>,
            },
            Field {
                dot_token: Token<'source>,
                index: Token<'source>,
            },
            Tuple(Token<'source>),
            SubExpression(Token<'source>),
        }

        impl<'source> Operation<'source> {
            fn precedence(self) -> usize {
                match self {
                    Operation::Field { .. } => 13,
                    Operation::Positive(_) | Operation::Negative(_) | Operation::Deref(_) => 12,
                    Operation::Mul(_) | Operation::Div(_) => 11,
                    Operation::Add(_) | Operation::Sub(_) => 10,
                    Operation::BitwiseAnd(_) => 9,
                    Operation::BitwiseXor(_) => 8,
                    Operation::BitwiseOr(_) => 7,
                    Operation::EqualTo(_)
                    | Operation::NotEqualTo(_)
                    | Operation::Greater(_)
                    | Operation::GreaterEqual(_)
                    | Operation::Lesser(_)
                    | Operation::LesserEqual(_) => 6,
                    Operation::LogicalAnd(_) => 5,
                    Operation::LogicalOr(_) => 4,
                    Operation::Name { .. } => 3,
                    Operation::Tuple(_) => 2,
                    Operation::Pipe(_) | Operation::Call(_) => 1,
                    Operation::SubExpression(_) => 0,
                }
            }

            fn left_associative(self) -> bool {
                match self {
                    Operation::Field { .. }
                    | Operation::Positive(_)
                    | Operation::Negative(_)
                    | Operation::Deref(_)
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
                    | Operation::SubExpression(_)
                    | Operation::Call(_)
                    | Operation::Pipe(_) => true,
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
                    Operation::Deref(t) => Node::Deref(t),
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

        let first_token = lexer.peek().copied().transpose().ok().flatten();
        let mut last_token = None;
        let mut diagnostics = Diagnostics::default();
        let mut contents = Vec::new();
        let mut stack = Vec::new();
        // Check if the last token implies the unary position.
        // This is probably not the best way to do this.
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
                ($($name:ident)? @ $lexi:ident) => {
                    Some($($name @)? Token {
                        lexigram: Lexigram::$lexi,
                        ..
                    })
                };
            }
            macro_rules! op {
                ($op:ident($name:ident)) => {
                    push_with_precedence(&mut contents, &mut stack, Operation::$op($name))
                };
            }
            match t {
                // Terminals
                //
                // A terminal value outside of unary position implies a function call,
                // so flush the operator stack in this case.
                lexi!(number @ Number) => {
                    if !unary_position {
                        op!(Call(number));
                    }
                    contents.push(Node::Number(number));
                }
                lexi!(string @ String) => {
                    if !unary_position {
                        op!(Call(string));
                    }
                    contents.push(Node::String(string));
                }
                lexi!(discard @ Discard) => {
                    last_token = lexer.next().transpose().ok().flatten();
                    let colon_token = diagnostics.next_if(lexer, &[Lexigram::Colon]);
                    if let Some(colon_token) = colon_token {
                        last_token = Some(colon_token);
                        push_with_precedence(
                            &mut contents,
                            &mut stack,
                            Operation::Name {
                                name: discard,
                                colon_token,
                            },
                        );
                    }
                    continue;
                }
                lexi!(ident @ Ident) => {
                    if !unary_position {
                        op!(Call(ident));
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
                lexi!(dot_token @ Dot) if !unary_position => {
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
                lexi!(t @ True) => {
                    if !unary_position {
                        push_with_precedence(&mut contents, &mut stack, Operation::Call(t));
                    }
                    contents.push(Node::Bool(true, t));
                }
                lexi!(t @ False) => {
                    if !unary_position {
                        push_with_precedence(&mut contents, &mut stack, Operation::Call(t));
                    }
                    contents.push(Node::Bool(false, t));
                }
                lexi!(t @ OpenParen) => {
                    if !unary_position {
                        push_with_precedence(&mut contents, &mut stack, Operation::Call(t));
                    }
                    stack.push(Operation::SubExpression(t));
                }
                lexi!(t @ OpenBrace) => {
                    if !unary_position {
                        push_with_precedence(&mut contents, &mut stack, Operation::Call(t));
                    }
                    lexer.next();
                    contents.push(Node::Block(Block::new(&mut *lexer)));
                    diagnostics.expect(lexer.peek().copied(), &[Lexigram::CloseBrace]);
                }

                // # Operators
                lexi!(t @ Plus) if unary_position => op!(Positive(t)),
                lexi!(t @ Plus) if !unary_position => op!(Add(t)),
                lexi!(t @ Minus) if unary_position => op!(Negative(t)),
                lexi!(t @ Minus) if !unary_position => op!(Sub(t)),
                // TODO: I'd rather use zig's `.*` for deref, but this causes unary position because the * looks like multiplication. What if ".*" was a token?
                lexi!(t @ Star) if unary_position => op!(Deref(t)),
                lexi!(t @ Star) if !unary_position => op!(Mul(t)),
                lexi!(t @ Slash) if !unary_position => op!(Div(t)),
                lexi!(t @ Ampersand) if !unary_position => op!(BitwiseAnd(t)),
                lexi!(t @ Caret) if !unary_position => op!(BitwiseXor(t)),
                lexi!(t @ Pipe) if !unary_position => op!(BitwiseOr(t)),
                lexi!(t @ DoubleEqual) if !unary_position => op!(EqualTo(t)),
                lexi!(t @ BangEqual) if !unary_position => op!(NotEqualTo(t)),
                lexi!(t @ Greater) if !unary_position => op!(Greater(t)),
                lexi!(t @ GreaterEqual) if !unary_position => op!(GreaterEqual(t)),
                lexi!(t @ Lesser) if !unary_position => op!(Lesser(t)),
                lexi!(t @ LesserEqual) if !unary_position => op!(LesserEqual(t)),
                lexi!(t @ And) if !unary_position => op!(LogicalAnd(t)),
                lexi!(t @ Or) if !unary_position => op!(LogicalOr(t)),
                lexi!(t @ Triangle) if !unary_position => op!(Pipe(t)),
                lexi!(t @ Comma) if !unary_position => op!(Tuple(t)),
                lexi!(  @ If) => contents.push(If::from(&mut *lexer).into()),
                lexi!(  @ Match) => contents.push(Match::new(&mut *lexer).into()),
                lexi!(  @ Struct) => contents.push(Struct::from(&mut *lexer).into()),
                lexi!(  @ Enum) => contents.push(Enum::from(&mut *lexer).into()),
                lexi!(t @ CloseParen) if unary_position => {
                    if let Some(
                        last_token @ Token {
                            lexigram: Lexigram::OpenParen,
                            ..
                        },
                    ) = last_token
                    {
                        contents.push(Node::Unit(last_token, t));
                    } else {
                        diagnostics.errors.push(Error::IncompleteExpression);
                    }
                    if !matches!(stack.pop(), Some(Operation::SubExpression(_))) {
                        diagnostics.errors.push(Error::UnexpectedCloseParen(t))
                    }
                }
                lexi!(t @ CloseParen) if !unary_position => {
                    while let Some(op) = stack.pop_if(|x| !matches!(x, Operation::SubExpression(_)))
                    {
                        contents.push(op.into());
                    }
                    if !matches!(stack.pop(), Some(Operation::SubExpression(_))) {
                        diagnostics.errors.push(Error::UnexpectedCloseParen(t))
                    }
                }
                _ => {
                    if unary_position {
                        if !contents.is_empty() || !stack.is_empty() {
                            diagnostics.errors.push(Error::IncompleteExpression);
                        }
                    } else {
                        flush(&mut contents, &mut stack);
                        if !stack.is_empty() {
                            diagnostics.errors.push(Error::IncompleteExpression);
                        }
                    }
                    if contents.is_empty() && diagnostics.errors.is_empty() {
                        return None;
                    } else {
                        return Some(Expression::build(
                            first_token,
                            last_token,
                            diagnostics,
                            contents,
                        ));
                    }
                }
            }
            // This is sometimes skipped with a continue!
            last_token = lexer.next().transpose().unwrap_or(None);
        }
    }
}

#[derive(Debug, Eq, PartialEq)]
pub struct If<'source> {
    pub if_token: Token<'source>,
    pub condition: Option<Box<Expression<'source>>>,
    pub then_token: Option<Token<'source>>,
    pub first: Box<Block<'source>>,
    pub else_token: Option<Token<'source>>,
    pub else_kind: Option<Token<'source>>,
    pub second: Box<Block<'source>>,
    pub end_token: Option<Token<'source>>,
    pub diagnostics: Diagnostics<'source>,
}

impl<'source> From<If<'source>> for Node<'source> {
    fn from(if_block: If<'source>) -> Self {
        Self::If(Box::new(if_block))
    }
}

impl<'source> From<&mut Peekable<Lexer<'source>>> for If<'source> {
    fn from(lexer: &mut Peekable<Lexer<'source>>) -> Self {
        let if_token = lexer
            .next()
            .transpose()
            .ok()
            .flatten()
            .expect("caller must have peeked a token");
        let mut diagnostics = Diagnostics::default();
        let condition = diagnostics.expect_expression(lexer);
        let then_token = diagnostics.next_if(lexer, &[Lexigram::Then]);
        let first = Block::new(&mut *lexer);
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
                    (Block::new(&mut *lexer), else_kind)
                }
                else_kind @ Some(Token {
                    lexigram: Lexigram::If,
                    ..
                }) => (
                    Block::build(
                        Expression::build(
                            None,
                            None,
                            Diagnostics::default(),
                            [Self::from(&mut *lexer).into()],
                        )
                        .into(),
                        Diagnostics::default(),
                        [],
                    ),
                    else_kind,
                ),
                _ => {
                    diagnostics.expect(lexer.peek().copied(), &[Lexigram::Then, Lexigram::If]);
                    (Default::default(), None)
                }
            };
            (second, else_token, else_kind)
        } else {
            (Default::default(), None, None)
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
    pub let_token: Option<Token<'source>>,
    pub binding: Option<Token<'source>>,
    pub equals_token: Option<Token<'source>>,
    pub case: Option<Box<Expression<'source>>>,
    pub arrow_token: Option<Token<'source>>,
    pub expression: Option<Box<Expression<'source>>>,
    pub semicolon_token: Option<Token<'source>>,
}

#[derive(Debug, Eq, PartialEq)]
#[make_dst_factory(pub)]
pub struct Match<'source> {
    pub match_token: Token<'source>,
    pub expression: Option<Box<Expression<'source>>>,
    pub then_token: Option<Token<'source>>,
    pub end_token: Option<Token<'source>>,
    pub diagnostics: Diagnostics<'source>,
    pub cases: [MatchCase<'source>],
}

impl<'source> From<Box<Match<'source>>> for Node<'source> {
    fn from(struct_block: Box<Match<'source>>) -> Self {
        Self::Match(struct_block)
    }
}

impl<'source> Match<'source> {
    fn new(lexer: &mut Peekable<Lexer<'source>>) -> Box<Self> {
        let match_token = lexer
            .next()
            .transpose()
            .ok()
            .flatten()
            .expect("caller must have peeked a token");
        let mut diagnostics = Diagnostics::default();

        let expression = diagnostics.expect_expression(lexer);
        let then_token = diagnostics.next_if(lexer, &[Lexigram::Then]);
        let mut cases = Vec::new();

        // ew
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
                    let case = diagnostics.expect_expression(lexer);
                    (equal_token, case)
                } else {
                    (None, None)
                };
                (let_token, binding, equal_token, case)
            } else {
                let case = diagnostics.expect_expression(lexer);
                (None, None, None, case)
            };
            let arrow_token = diagnostics.next_if(lexer, &[Lexigram::DoubleArrow]);
            let expression = diagnostics.expect_expression(lexer);
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
        Match::build(
            match_token,
            expression,
            then_token,
            end_token,
            diagnostics,
            cases,
        )
    }
}

#[derive(Debug, Eq, PartialEq)]
pub struct Struct<'source> {
    pub struct_token: Token<'source>,
    pub inner: Option<Box<Expression<'source>>>,
    pub then_token: Option<Token<'source>>,
    pub members: Option<Box<Expression<'source>>>,
    pub end_token: Option<Token<'source>>,
    pub diagnostics: Diagnostics<'source>,
}

impl<'source> From<Struct<'source>> for Node<'source> {
    fn from(struct_block: Struct<'source>) -> Self {
        Self::Struct(Box::new(struct_block))
    }
}

impl<'source> From<&mut Peekable<Lexer<'source>>> for Struct<'source> {
    fn from(lexer: &mut Peekable<Lexer<'source>>) -> Self {
        let struct_token = lexer
            .next()
            .transpose()
            .ok()
            .flatten()
            .expect("caller must have peeked a token");
        let mut diagnostics = Diagnostics::default();
        let inner = diagnostics.expect_expression(lexer);
        let (then_token, members) = match lexer.peek().copied() {
            Some(Ok(
                then_token @ Token {
                    lexigram: Lexigram::Then,
                    ..
                },
            )) => {
                lexer.next();
                let members = Expression::new(&mut *lexer);
                (Some(then_token), members)
            }
            Some(Ok(Token {
                lexigram: Lexigram::End,
                ..
            })) => (None, None),
            t => {
                diagnostics.expect(t, &[Lexigram::Then, Lexigram::End]);
                (None, None)
            }
        };
        let end_token = diagnostics.expect(lexer.peek().copied(), &[Lexigram::End]);
        Self {
            struct_token,
            inner,
            then_token,
            members,
            end_token,
            diagnostics,
        }
    }
}

#[derive(Debug, Eq, PartialEq)]
pub struct Enum<'source> {
    pub enum_token: Token<'source>,
    pub variants: Option<Box<Expression<'source>>>,
    pub end_token: Option<Token<'source>>,
    pub diagnostics: Diagnostics<'source>,
}

impl<'source> From<Enum<'source>> for Node<'source> {
    fn from(struct_block: Enum<'source>) -> Self {
        Self::Enum(Box::new(struct_block))
    }
}

impl<'source> From<&mut Peekable<Lexer<'source>>> for Enum<'source> {
    fn from(lexer: &mut Peekable<Lexer<'source>>) -> Self {
        let enum_token = lexer
            .next()
            .transpose()
            .ok()
            .flatten()
            .expect("caller must have peeked a token");
        let mut diagnostics = Diagnostics::default();
        let variants = diagnostics.expect_expression(lexer);
        let end_token = diagnostics.expect(lexer.peek().copied(), &[Lexigram::End]);
        Self {
            enum_token,
            variants,
            end_token,
            diagnostics,
        }
    }
}

#[derive(Debug, Eq, PartialEq)]
pub enum Statement<'source> {
    Evaluation(Evaluation<'source>),
    Set(Set<'source>),
    For(For<'source>),
}

#[derive(Debug, Eq, PartialEq)]
pub struct Evaluation<'source> {
    pub binding: Option<LetBinding<'source>>,
    pub expression: Option<Box<Expression<'source>>>,
    pub semicolon_token: Option<Token<'source>>,
    pub diagnostics: Diagnostics<'source>,
}

impl<'source> Evaluation<'source> {
    fn binding(lexer: &mut Peekable<Lexer<'source>>) -> Self {
        let mut diagnostics = Diagnostics::default();
        let let_token = lexer
            .next()
            .transpose()
            .ok()
            .flatten()
            .expect("caller must have peeked a token");
        let binding = Binding::new(lexer)
            .map_err(|e| diagnostics.errors.push(e))
            .ok();
        let equals_token = diagnostics.next_if(lexer, &[Lexigram::SingleEqual]);
        let expression = diagnostics.expect_expression(lexer);
        let semicolon_token = diagnostics.next_if(lexer, &[Lexigram::Semicolon]);

        Evaluation {
            binding: Some(LetBinding {
                let_token,
                binding,
                equals_token,
            }),
            expression,
            semicolon_token,
            diagnostics,
        }
    }

    fn try_expression(
        lexer: &mut Peekable<Lexer<'source>>,
    ) -> Result<Self, Option<Box<Expression<'source>>>> {
        let expression = Expression::new(&mut *lexer);
        if let Some(Ok(
            semicolon_token @ Token {
                lexigram: Lexigram::Semicolon,
                ..
            },
        )) = lexer.peek().copied()
        {
            lexer.next();
            Ok(Evaluation {
                binding: None,
                expression,
                semicolon_token: Some(semicolon_token),
                diagnostics: Diagnostics::default(),
            })
        } else {
            Err(expression)
        }
    }
}

#[derive(Debug, Eq, PartialEq)]
pub struct LetBinding<'source> {
    pub let_token: Token<'source>,
    pub binding: Option<Binding<'source>>,
    pub equals_token: Option<Token<'source>>,
}

#[derive(Debug, Eq, PartialEq)]
pub struct Set<'source> {
    pub set_token: Token<'source>,
    pub target: Option<Box<Expression<'source>>>,
    pub equals_token: Option<Token<'source>>,
    pub expression: Option<Box<Expression<'source>>>,
    pub semicolon_token: Option<Token<'source>>,
    pub diagnostics: Diagnostics<'source>,
}

impl<'source> Set<'source> {
    fn new(lexer: &mut Peekable<Lexer<'source>>) -> Self {
        let mut diagnostics = Diagnostics::default();
        let set_token = lexer
            .next()
            .transpose()
            .ok()
            .flatten()
            .expect("caller must have peeked a token");
        let target = diagnostics.expect_expression(lexer);
        let equals_token = diagnostics.next_if(lexer, &[Lexigram::SingleEqual]);
        let expression = diagnostics.expect_expression(lexer);
        let semicolon_token = diagnostics.next_if(lexer, &[Lexigram::Semicolon]);

        Set {
            set_token,
            target,
            equals_token,
            expression,
            semicolon_token,
            diagnostics,
        }
    }
}

#[derive(Debug, Eq, PartialEq)]
pub struct NumericBinding<'source> {
    pub binding: Binding<'source>,
    pub comma_token: Option<Token<'source>>,
}

#[derive(Debug, Eq, PartialEq)]
pub struct NamedBinding<'source> {
    pub field: Token<'source>,
    pub binding: Option<NamedSubBinding<'source>>,
    pub comma_token: Option<Token<'source>>,
}

#[derive(Debug, Eq, PartialEq)]
pub struct NamedSubBinding<'source> {
    pub colon_token: Token<'source>,
    pub binding: Binding<'source>,
}

#[derive(Debug, Eq, PartialEq)]
pub enum BindingMethod<'source> {
    Single(Token<'source>),
    Numeric {
        open_paren: Token<'source>,
        bindings: Box<[NumericBinding<'source>]>,
        close_paren: Option<Token<'source>>,
    },
    Named {
        open_brace: Token<'source>,
        bindings: Box<[NamedBinding<'source>]>,
        close_brace: Option<Token<'source>>,
    },
}

#[derive(Debug, Eq, PartialEq)]
pub struct Binding<'source> {
    pub method: BindingMethod<'source>,
    pub diagnostics: Diagnostics<'source>,
}

impl<'source> Binding<'source> {
    fn new(lexer: &mut Peekable<Lexer<'source>>) -> Result<Self, Error<'source>> {
        match lexer.peek().copied().transpose().map_err(Error::Lexer)? {
            Some(
                t @ Token {
                    lexigram: Lexigram::Ident | Lexigram::Discard,
                    ..
                },
            ) => {
                lexer.next();
                Ok(Binding {
                    method: BindingMethod::Single(t),
                    diagnostics: Diagnostics::default(),
                })
            }
            Some(
                open_paren @ Token {
                    lexigram: Lexigram::OpenParen,
                    ..
                },
            ) => {
                let mut diagnostics = Diagnostics::default();
                let mut bindings = Vec::new();
                lexer.next();
                loop {
                    let t = diagnostics.wrap(lexer.peek().copied());
                    if let Some(Token {
                        lexigram: Lexigram::CloseParen,
                        ..
                    }) = t
                    {
                        break;
                    }
                    match Binding::new(lexer) {
                        Ok(binding) => {
                            let comma_token = diagnostics
                                .wrap(lexer.peek().copied())
                                .filter(|t| t.lexigram == Lexigram::Comma);
                            bindings.push(NumericBinding {
                                binding,
                                comma_token,
                            });
                            if comma_token.is_some() {
                                lexer.next();
                            } else {
                                break;
                            }
                        }
                        Err(_) => {
                            diagnostics.errors.push(Error::MissingToken {
                                expected: &[
                                    Lexigram::Ident,
                                    Lexigram::Discard,
                                    Lexigram::OpenParen,
                                    Lexigram::OpenBrace,
                                    Lexigram::CloseParen,
                                ],
                                actual: t,
                            });
                            break;
                        }
                    }
                }
                let close_paren = diagnostics.next_if(lexer, &[Lexigram::CloseParen]);
                Ok(Binding {
                    method: BindingMethod::Numeric {
                        open_paren,
                        bindings: bindings.into_boxed_slice(),
                        close_paren,
                    },
                    diagnostics,
                })
            }
            Some(
                open_brace @ Token {
                    lexigram: Lexigram::OpenBrace,
                    ..
                },
            ) => {
                let mut diagnostics = Diagnostics::default();
                let mut bindings = Vec::new();
                lexer.next();
                loop {
                    match diagnostics.wrap(lexer.peek().copied()) {
                        Some(Token {
                            lexigram: Lexigram::CloseBrace,
                            ..
                        }) => break,
                        Some(
                            field @ Token {
                                lexigram: Lexigram::Ident,
                                ..
                            },
                        ) => {
                            lexer.next();
                            match diagnostics.wrap(lexer.peek().copied()) {
                                comma_token @ Some(Token {
                                    lexigram: Lexigram::Comma,
                                    ..
                                }) => {
                                    lexer.next();
                                    bindings.push(NamedBinding {
                                        field,
                                        binding: None,
                                        comma_token,
                                    });
                                }
                                Some(
                                    colon_token @ Token {
                                        lexigram: Lexigram::Colon,
                                        ..
                                    },
                                ) => {
                                    lexer.next();
                                    match Binding::new(lexer) {
                                        Ok(binding) => {
                                            let comma_token = diagnostics
                                                .wrap(lexer.peek().copied())
                                                .filter(|t| t.lexigram == Lexigram::Comma);
                                            bindings.push(NamedBinding {
                                                field,
                                                binding: Some(NamedSubBinding {
                                                    colon_token,
                                                    binding,
                                                }),
                                                comma_token,
                                            });
                                            if comma_token.is_some() {
                                                lexer.next();
                                            } else {
                                                break;
                                            }
                                        }
                                        Err(e) => {
                                            diagnostics.errors.push(e);
                                            break;
                                        }
                                    }
                                }
                                _ => {
                                    bindings.push(NamedBinding {
                                        field,
                                        binding: None,
                                        comma_token: None,
                                    });
                                }
                            }
                        }
                        actual => {
                            diagnostics.errors.push(Error::MissingToken {
                                expected: &[Lexigram::Ident, Lexigram::CloseBrace],
                                actual,
                            });
                            break;
                        }
                    }
                }
                let close_brace = diagnostics.next_if(lexer, &[Lexigram::CloseBrace]);
                Ok(Binding {
                    method: BindingMethod::Named {
                        open_brace,
                        bindings: bindings.into_boxed_slice(),
                        close_brace,
                    },
                    diagnostics,
                })
            }
            actual => Err(Error::MissingToken {
                expected: &[
                    Lexigram::Ident,
                    Lexigram::Discard,
                    Lexigram::OpenParen,
                    Lexigram::OpenBrace,
                ],
                actual,
            }),
        }
    }
}

#[derive(Debug, Eq, PartialEq)]
pub struct For<'source> {
    pub for_token: Token<'source>,
    pub binding: Option<Token<'source>>,
    pub in_token: Option<Token<'source>>,
    pub iterator: Option<Box<Expression<'source>>>,
    pub then_token: Option<Token<'source>>,
    pub block: Box<Block<'source>>,
    pub end_token: Option<Token<'source>>,
    pub diagnostics: Diagnostics<'source>,
}

impl<'source> From<&mut Peekable<Lexer<'source>>> for For<'source> {
    fn from(lexer: &mut Peekable<Lexer<'source>>) -> Self {
        let mut diagnostics = Diagnostics::default();

        let for_token = lexer
            .next()
            .transpose()
            .ok()
            .flatten()
            .expect("caller must have peeked a token");
        let binding = diagnostics.next_if(lexer, &[Lexigram::Ident, Lexigram::Discard]);
        let in_token = diagnostics.next_if(lexer, &[Lexigram::In]);
        let iterator = diagnostics.expect_expression(lexer);
        let then_token = diagnostics.next_if(lexer, &[Lexigram::Then]);
        let first = Block::new(&mut *lexer);
        let end_token = diagnostics.next_if(lexer, &[Lexigram::End]);

        For {
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
    pub trait_expression: Option<Box<Expression<'source>>>,
    pub equals_token: Option<Token<'source>>,
    pub methods: Option<Box<Expression<'source>>>,
    pub semicolon_token: Option<Token<'source>>,
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
            .expect("caller must have peeked a token");
        let trait_expression = diagnostics.expect_expression(lexer);
        let equals_token = diagnostics.next_if(lexer, &[Lexigram::SingleEqual]);
        let methods = Expression::new(&mut *lexer);
        let semicolon_token = diagnostics.next_if(lexer, &[Lexigram::Semicolon]);

        Self {
            impl_token,
            trait_expression,
            equals_token,
            methods,
            semicolon_token,
            diagnostics,
        }
    }
}

#[derive(Debug, Eq, PartialEq)]
pub struct Function<'source> {
    pub with_token: Token<'source>,
    pub argument: Option<Binding<'source>>,
    pub colon_token: Option<Token<'source>>,
    pub input: Option<Box<Expression<'source>>>,
    pub single_arrow_token: Option<Token<'source>>,
    pub output: Option<Box<Expression<'source>>>,
    pub semicolon_token: Option<Token<'source>>,
    pub block: Box<Block<'source>>,
    pub diagnostics: Diagnostics<'source>,
}

#[derive(Debug, Eq, PartialEq)]
#[expect(
    clippy::large_enum_variant,
    reason = "this is already inside of a (very large) boxed block"
)]
pub enum BlockResult<'source> {
    Expression(Option<Box<Expression<'source>>>),
    Break {
        break_token: Token<'source>,
        expression: Option<Box<Expression<'source>>>,
    },
    Function(Function<'source>),
}

impl BlockResult<'_> {
    pub fn is_empty(&self) -> bool {
        match self {
            BlockResult::Expression(expression) => expression.is_none(),
            BlockResult::Break { .. } | BlockResult::Function(_) => false,
        }
    }
}

impl Default for BlockResult<'_> {
    fn default() -> Self {
        Self::Expression(None)
    }
}

impl<'source> From<Box<Expression<'source>>> for BlockResult<'source> {
    fn from(expression: Box<Expression<'source>>) -> Self {
        Self::Expression(Some(expression))
    }
}

impl<'source> From<Function<'source>> for BlockResult<'source> {
    fn from(function: Function<'source>) -> Self {
        Self::Function(function)
    }
}

#[derive(Debug, Eq, PartialEq)]
#[make_dst_factory(pub)]
pub struct Block<'source> {
    pub result: BlockResult<'source>,
    pub diagnostics: Diagnostics<'source>,
    pub statements: [Statement<'source>],
}

impl Default for Box<Block<'_>> {
    fn default() -> Self {
        Block::build(BlockResult::Expression(None), Diagnostics::default(), [])
    }
}

impl<'source> Block<'source> {
    pub fn new(lexer: &mut Peekable<Lexer<'source>>) -> Box<Self> {
        let mut diagnostics = Diagnostics::default();
        let mut statements = Vec::new();
        loop {
            let statement = match diagnostics.wrap(lexer.peek().copied()) {
                Some(Token {
                    lexigram: Lexigram::Let,
                    ..
                }) => Statement::Evaluation(Evaluation::binding(lexer)),
                Some(Token {
                    lexigram: Lexigram::Set,
                    ..
                }) => Statement::Set(Set::new(lexer)),
                Some(Token {
                    lexigram: Lexigram::For,
                    ..
                }) => Statement::For(For::from(&mut *lexer)),
                Some(
                    break_token @ Token {
                        lexigram: Lexigram::Break,
                        ..
                    },
                ) => {
                    lexer.next();
                    let expression = Expression::new(&mut *lexer);
                    return Self::build(
                        BlockResult::Break {
                            break_token,
                            expression,
                        },
                        diagnostics,
                        statements,
                    );
                }
                Some(
                    with_token @ Token {
                        lexigram: Lexigram::With,
                        ..
                    },
                ) => {
                    lexer.next();
                    let mut st_diagnostics = Diagnostics::default();
                    let argument = Binding::new(lexer)
                        .map_err(|e| st_diagnostics.errors.push(e))
                        .ok();
                    let (colon_token, input) = if let Some(
                        t @ Token {
                            lexigram: Lexigram::Colon,
                            ..
                        },
                    ) = st_diagnostics.wrap(lexer.peek().copied())
                    {
                        lexer.next();
                        (Some(t), diagnostics.expect_expression(lexer))
                    } else {
                        (None, None)
                    };
                    let (single_arrow_token, output) = if let Some(
                        t @ Token {
                            lexigram: Lexigram::SingleArrow,
                            ..
                        },
                    ) =
                        st_diagnostics.wrap(lexer.peek().copied())
                    {
                        lexer.next();
                        (Some(t), diagnostics.expect_expression(lexer))
                    } else {
                        (None, None)
                    };
                    let semicolon_token =
                        st_diagnostics.next_if(&mut *lexer, &[Lexigram::Semicolon]);
                    let block = Block::new(&mut *lexer);

                    return Self::build(
                        Function {
                            with_token,
                            argument,
                            colon_token,
                            input,
                            single_arrow_token,
                            output,
                            semicolon_token,
                            block,
                            diagnostics: st_diagnostics,
                        }
                        .into(),
                        diagnostics,
                        statements,
                    );
                }
                _ => match Evaluation::try_expression(&mut *lexer) {
                    Ok(evaluation) => Statement::Evaluation(evaluation),
                    Err(expression) => {
                        return Self::build(
                            BlockResult::Expression(expression),
                            diagnostics,
                            statements,
                        );
                    }
                },
            };
            statements.push(statement);
        }
    }
}
