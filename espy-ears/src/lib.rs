use espy_eyes::{self as lexer, Lexer, Lexigram, Token};
use std::iter::Peekable;

#[cfg(test)]
mod tests;

#[derive(Debug, Eq, PartialEq)]
pub enum Error<'source> {
    Lexer(lexer::Error<'source>),
    MissingToken {
        expected: &'static [Lexigram<'static>],
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
    contents: Vec<Diagnostic<'source>>,
}

impl<'source> Diagnostics<'source> {
    fn expect(
        &mut self,
        t: Option<lexer::Result<'source>>,
        expected: &'static [Lexigram<'static>],
    ) -> Option<Token<'source>> {
        let actual = self.wrap(t);
        if actual.is_some_and(|actual| expected.contains(&actual.lexigram)) {
            actual
        } else {
            self.contents
                .push(Diagnostic::Error(Error::MissingToken { expected, actual }));
            None
        }
    }

    fn wrap(&mut self, t: Option<lexer::Result<'source>>) -> Option<Token<'source>> {
        match t? {
            Ok(t) => Some(t),
            Err(e) => {
                let t = if let lexer::ErrorKind::ReservedSymbol(ident) = e.kind {
                    Some(Token {
                        lexigram: Lexigram::Ident(ident),
                        start: e.start,
                        end: e.end,
                    })
                } else {
                    None
                };
                self.contents.push(Diagnostic::Error(Error::Lexer(e)));
                t
            }
        }
    }
}

#[derive(Debug, Eq, PartialEq)]
pub enum Node<'source> {
    Number(&'source str),
    Ident(&'source str),
    Block(Block<'source>),
    If {
        condition: Expression<'source>,
        first: Block<'source>,
        second: Block<'source>,
        diagnostics: Diagnostics<'source>,
    },
    For {
        binding: Option<&'source str>,
        iterator: Expression<'source>,
        first: Block<'source>,
        second: Block<'source>,
        diagnostics: Diagnostics<'source>,
    },

    Pipe,
    Call,
    Positive,
    Negative,
    Mul,
    Div,
    Add,
    Sub,
    BitwiseAnd,
    BitwiseOr,
    BitwiseXor,
    EqualTo,
    NotEqualTo,
    Greater,
    GreaterEqual,
    Lesser,
    LesserEqual,
    LogicalAnd,
    LogicalOr,
    Name,
    Tuple,
}

/// This type must not contain any incomplete expressions.
#[derive(Debug, Default, Eq, PartialEq)]
pub struct Expression<'source> {
    // TODO: This field should be exposed through an iterator or method, not pub.
    contents: Vec<Node<'source>>,
    diagnostics: Diagnostics<'source>,
}

/// Parse an expression until an unexpected token is upcoming (via peek).
impl<'source> From<&mut Peekable<Lexer<'source>>> for Expression<'source> {
    fn from(lexer: &mut Peekable<Lexer<'source>>) -> Self {
        #[derive(Clone, Copy, Debug, Eq, PartialEq)]
        enum Operation {
            Call,
            Pipe,
            Positive,
            Negative,
            Mul,
            Div,
            Add,
            Sub,
            BitwiseAnd,
            BitwiseXor,
            BitwiseOr,
            EqualTo,
            NotEqualTo,
            Greater,
            GreaterEqual,
            Lesser,
            LesserEqual,
            LogicalAnd,
            LogicalOr,
            Name,
            Tuple,
            SubExpression,
        }

        impl Operation {
            fn precedence(self) -> usize {
                match self {
                    Operation::Pipe => 13,
                    Operation::Call => 12,
                    Operation::Positive | Operation::Negative => 11,
                    Operation::Mul | Operation::Div => 10,
                    Operation::Add | Operation::Sub => 9,
                    Operation::BitwiseAnd => 8,
                    Operation::BitwiseXor => 7,
                    Operation::BitwiseOr => 6,
                    Operation::EqualTo
                    | Operation::NotEqualTo
                    | Operation::Greater
                    | Operation::GreaterEqual
                    | Operation::Lesser
                    | Operation::LesserEqual => 5,
                    Operation::LogicalAnd => 4,
                    Operation::LogicalOr => 3,
                    Operation::Name => 2,
                    Operation::Tuple => 1,
                    Operation::SubExpression => 0,
                }
            }

            fn left_associative(self) -> bool {
                match self {
                    Operation::Call
                    | Operation::Positive
                    | Operation::Negative
                    | Operation::Mul
                    | Operation::Div
                    | Operation::Add
                    | Operation::Sub
                    | Operation::BitwiseAnd
                    | Operation::BitwiseXor
                    | Operation::BitwiseOr
                    | Operation::EqualTo
                    | Operation::NotEqualTo
                    | Operation::Greater
                    | Operation::GreaterEqual
                    | Operation::Lesser
                    | Operation::LesserEqual
                    | Operation::LogicalAnd
                    | Operation::LogicalOr
                    | Operation::Tuple
                    | Operation::SubExpression => true,
                    Operation::Name | Operation::Pipe => false,
                }
            }
        }

        impl From<Operation> for Node<'_> {
            fn from(op: Operation) -> Self {
                match op {
                    Operation::Pipe => Node::Pipe,
                    Operation::Call => Node::Call,
                    Operation::Positive => Node::Positive,
                    Operation::Negative => Node::Negative,
                    Operation::Mul => Node::Mul,
                    Operation::Div => Node::Div,
                    Operation::Add => Node::Add,
                    Operation::Sub => Node::Sub,
                    Operation::BitwiseAnd => Node::BitwiseAnd,
                    Operation::BitwiseXor => Node::BitwiseXor,
                    Operation::BitwiseOr => Node::BitwiseOr,
                    Operation::EqualTo => Node::EqualTo,
                    Operation::NotEqualTo => Node::NotEqualTo,
                    Operation::Greater => Node::Greater,
                    Operation::GreaterEqual => Node::GreaterEqual,
                    Operation::Lesser => Node::Lesser,
                    Operation::LesserEqual => Node::LesserEqual,
                    Operation::LogicalAnd => Node::LogicalAnd,
                    Operation::LogicalOr => Node::LogicalOr,
                    Operation::Name => Node::Name,
                    Operation::Tuple => Node::Tuple,
                    Operation::SubExpression => {
                        panic!("sub expressions may not enter the output stack")
                    }
                }
            }
        }

        fn conditional<'source>(lexer: &mut Peekable<Lexer<'source>>) -> Node<'source> {
            lexer.next();
            let mut diagnostics = Diagnostics::default();
            let condition = Expression::from(&mut *lexer);
            if diagnostics
                .expect(lexer.peek().copied(), &[Lexigram::Then])
                .is_some()
            {
                lexer.next();
            }
            let first = Block::from(Ast::from(&mut *lexer));
            let second = if matches!(
                diagnostics.wrap(lexer.peek().copied()),
                Some(Token {
                    lexigram: Lexigram::Else,
                    ..
                })
            ) {
                lexer.next();
                match diagnostics.wrap(lexer.peek().copied()) {
                    Some(Token {
                        lexigram: Lexigram::Then,
                        ..
                    }) => {
                        lexer.next();
                        Block::from(Ast::from(&mut *lexer))
                    }
                    Some(Token {
                        lexigram: Lexigram::If,
                        ..
                    }) => Block {
                        statements: Vec::new(),
                        result: Expression {
                            contents: vec![conditional(&mut *lexer)],
                            diagnostics: Diagnostics::default(),
                        },
                        diagnostics: Diagnostics::default(),
                    },
                    _ => {
                        diagnostics.expect(lexer.peek().copied(), &[Lexigram::Then, Lexigram::If]);
                        Block::default()
                    }
                }
            } else {
                Block::default()
            };
            diagnostics.expect(lexer.peek().copied(), &[Lexigram::End]);
            Node::If {
                condition,
                first,
                second,
                diagnostics,
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
                        | Lexigram::EqualTo
                        | Lexigram::NotEqualTo
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
        let flush = |output: &mut Vec<Node>, stack: &mut Vec<Operation>| {
            while let Some(op) = stack.pop_if(|x| !matches!(x, Operation::SubExpression)) {
                output.push(op.into());
            }
        };
        let push_with_precedence =
            |output: &mut Vec<Node>, stack: &mut Vec<Operation>, operator: Operation| {
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
                    push_with_precedence(&mut contents, &mut stack, Operation::$op)
                };
            }
            let unary_position = unary_position(last_token);
            let t = diagnostics.wrap(lexer.peek().copied());
            match t {
                // Terminals
                //
                // A terminal value outside of unary position implies a function call,
                // so flush the operator stack in this case.
                Some(Token {
                    lexigram: Lexigram::Number(number),
                    ..
                }) => {
                    if !unary_position {
                        push_with_precedence(&mut contents, &mut stack, Operation::Call);
                    }
                    contents.push(Node::Number(number));
                }
                Some(Token {
                    lexigram: Lexigram::Ident(number),
                    ..
                }) => {
                    if !unary_position {
                        push_with_precedence(&mut contents, &mut stack, Operation::Call);
                    }
                    contents.push(Node::Ident(number));
                }
                lexi!(OpenParen) => {
                    if !unary_position {
                        push_with_precedence(&mut contents, &mut stack, Operation::Call);
                    }
                    stack.push(Operation::SubExpression);
                }
                lexi!(OpenBrace) => {
                    if !unary_position {
                        push_with_precedence(&mut contents, &mut stack, Operation::Call);
                    }
                    lexer.next();
                    contents.push(Node::Block(Block::from(Ast::from(&mut *lexer))));
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
                lexi!(EqualTo) if !unary_position => op!(EqualTo),
                lexi!(NotEqualTo) if !unary_position => op!(NotEqualTo),
                lexi!(Greater) if !unary_position => op!(Greater),
                lexi!(GreaterEqual) if !unary_position => op!(GreaterEqual),
                lexi!(Lesser) if !unary_position => op!(Lesser),
                lexi!(LesserEqual) if !unary_position => op!(LesserEqual),
                lexi!(And) if !unary_position => op!(LogicalAnd),
                lexi!(Or) if !unary_position => op!(LogicalOr),
                lexi!(Triangle) if !unary_position => op!(Pipe),
                lexi!(Colon) if !unary_position => op!(Name),
                lexi!(Comma) if !unary_position => op!(Tuple),
                lexi!(CloseParen) if !unary_position => {
                    while let Some(op) = stack.pop_if(|x| !matches!(x, Operation::SubExpression)) {
                        contents.push(op.into());
                    }
                    if !matches!(stack.pop(), Some(Operation::SubExpression)) {
                        diagnostics
                            .contents
                            .push(Diagnostic::Error(Error::UnexpectedCloseParen(t)))
                    }
                }
                lexi!(If) => {
                    contents.push(conditional(lexer));
                }
                lexi!(For) => {
                    lexer.next();
                    let mut diagnostics = Diagnostics::default();

                    let binding = match diagnostics.wrap(lexer.peek().copied()) {
                        Some(Token {
                            lexigram: Lexigram::Ident(ident),
                            ..
                        }) => {
                            lexer.next();
                            Some(ident)
                        }
                        Some(Token {
                            lexigram: Lexigram::Discard,
                            ..
                        }) => {
                            lexer.next();
                            None
                        }
                        _ => {
                            diagnostics.expect(
                                lexer.peek().copied(),
                                &[Lexigram::Ident(""), Lexigram::Discard],
                            );
                            None
                        }
                    };
                    if diagnostics
                        .expect(lexer.peek().copied(), &[Lexigram::In])
                        .is_some()
                    {
                        lexer.next();
                    }
                    let iterator = Expression::from(&mut *lexer);
                    if diagnostics
                        .expect(lexer.peek().copied(), &[Lexigram::Then])
                        .is_some()
                    {
                        lexer.next();
                    }
                    let first = Block::from(Ast::from(&mut *lexer));
                    let result = match diagnostics.wrap(lexer.peek().copied()) {
                        Some(Token {
                            lexigram: Lexigram::End,
                            ..
                        }) => Node::For {
                            binding,
                            iterator,
                            first,
                            second: Block::default(),
                            diagnostics,
                        },
                        Some(Token {
                            lexigram: Lexigram::Else,
                            ..
                        }) => {
                            lexer.next();
                            let second = Block::from(Ast::from(&mut *lexer));
                            diagnostics.expect(lexer.peek().copied(), &[Lexigram::End]);
                            Node::For {
                                binding,
                                iterator,
                                first,
                                second,
                                diagnostics,
                            }
                        }
                        _ => {
                            diagnostics
                                .expect(lexer.peek().copied(), &[Lexigram::End, Lexigram::Else]);
                            Node::For {
                                binding,
                                iterator,
                                first,
                                second: Block::default(),
                                diagnostics,
                            }
                        }
                    };
                    contents.push(result);
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
                                .contents
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
            last_token = lexer.next().transpose().unwrap_or(None);
        }
    }
}

#[derive(Debug, Eq, PartialEq)]
pub struct Binding<'source> {
    pub ident: &'source str,
    pub ty: Option<&'source str>,
}

#[derive(Debug, Eq, PartialEq)]
pub enum Action<'source> {
    Binding(Binding<'source>),
    Break,
}

impl<'source> From<Binding<'source>> for Action<'source> {
    fn from(binding: Binding<'source>) -> Self {
        Action::Binding(binding)
    }
}

#[derive(Debug, Default, Eq, PartialEq)]
pub struct Statement<'source> {
    pub action: Option<Action<'source>>,
    pub expression: Option<Expression<'source>>,
    pub diagnostics: Diagnostics<'source>,
}

#[derive(Debug, Default, Eq, PartialEq)]
pub struct Block<'source> {
    statements: Vec<Statement<'source>>,
    result: Expression<'source>,
    diagnostics: Diagnostics<'source>,
}

impl<'source> From<Ast<'source, '_>> for Block<'source> {
    fn from(mut ast: Ast<'source, '_>) -> Self {
        let mut statements = Vec::new();
        for statement in &mut ast {
            statements.push(statement);
        }
        let Ast {
            closed,
            diagnostics,
            ..
        } = ast;
        Self {
            statements,
            result: closed.expect("ast must be closed by resolve"),
            diagnostics,
        }
    }
}

pub struct Ast<'source, 'iter> {
    lexer: &'iter mut Peekable<Lexer<'source>>,
    closed: Option<Expression<'source>>,
    diagnostics: Diagnostics<'source>,
}

impl<'source, 'iter> From<&'iter mut Peekable<Lexer<'source>>> for Ast<'source, 'iter> {
    fn from(lexer: &'iter mut Peekable<Lexer<'source>>) -> Self {
        Self {
            lexer,
            closed: None,
            diagnostics: Diagnostics::default(),
        }
    }
}

impl<'source> Iterator for Ast<'source, '_> {
    type Item = Statement<'source>;
    fn next(&mut self) -> Option<Self::Item> {
        if self.closed.is_some() {
            return None;
        }
        match self.diagnostics.wrap(self.lexer.peek().copied()) {
            Some(Token {
                lexigram: Lexigram::Break,
                ..
            }) => {
                self.lexer.next();
                let expression = Expression::from(&mut *self.lexer);
                let mut diagnostics = Diagnostics::default();
                if diagnostics
                    .expect(self.lexer.peek().copied(), &[Lexigram::Semicolon])
                    .is_some()
                {
                    self.lexer.next();
                }
                Some(Statement {
                    action: Some(Action::Break),
                    expression: Some(expression),
                    diagnostics,
                })
            }
            Some(Token {
                lexigram: Lexigram::Let,
                ..
            }) => {
                self.lexer.next();
                let mut st_diagnostics = Diagnostics::default();
                let token = st_diagnostics.wrap(self.lexer.next());
                if let Some(Token {
                    lexigram: Lexigram::Ident(ident),
                    ..
                }) = token
                {
                    match st_diagnostics.wrap(self.lexer.peek().copied()) {
                        Some(Token {
                            lexigram: Lexigram::Equals,
                            ..
                        }) => {
                            self.lexer.next();
                            let expression = Expression::from(&mut *self.lexer);
                            if st_diagnostics
                                .expect(self.lexer.peek().copied(), &[Lexigram::Semicolon])
                                .is_some()
                            {
                                self.lexer.next();
                            }
                            Some(Statement {
                                action: Some(Binding { ident, ty: None }.into()),
                                expression: Some(expression),
                                diagnostics: st_diagnostics,
                            })
                        }
                        Some(Token {
                            lexigram: Lexigram::Semicolon,
                            ..
                        }) => {
                            self.lexer.next();
                            Some(Statement {
                                action: Some(Binding { ident, ty: None }.into()),
                                expression: None,
                                diagnostics: st_diagnostics,
                            })
                        }
                        _ => {
                            st_diagnostics.expect(
                                self.lexer.peek().copied(),
                                &[Lexigram::Equals, Lexigram::Semicolon],
                            );
                            Some(Statement {
                                action: Some(Binding { ident, ty: None }.into()),
                                expression: None,
                                diagnostics: st_diagnostics,
                            })
                        }
                    }
                } else {
                    st_diagnostics
                        .contents
                        .push(Diagnostic::Error(Error::MissingToken {
                            expected: &[Lexigram::Ident("")],
                            actual: token,
                        }));
                    Some(Statement {
                        action: None,
                        expression: None,
                        diagnostics: st_diagnostics,
                    })
                }
            }
            _ => {
                let mut st_diagnostics = Diagnostics::default();
                let expression = Expression::from(&mut *self.lexer);
                if let Some(Token {
                    lexigram: Lexigram::Semicolon,
                    ..
                }) = st_diagnostics.wrap(self.lexer.peek().copied())
                {
                    self.lexer.next();
                    Some(Statement {
                        action: None,
                        expression: Some(expression),
                        diagnostics: st_diagnostics,
                    })
                } else {
                    self.closed = Some(expression);
                    None
                }
            }
        }
    }
}
