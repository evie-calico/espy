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
                // TODO: If `lexer::Error` carried a position we could convert `lexer::Error::ReservedSymbol` to `Lexigram::Ident` for fault-tolerance.
                self.contents.push(Diagnostic::Error(Error::Lexer(e)));
                None
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

    Positive,
    Negative,
    Mul,
    Div,
    Add,
    Sub,
    EqualTo,
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
            Positive,
            Negative,

            Mul,
            Div,

            Add,
            Sub,

            EqualTo,

            Name,

            Tuple,

            SubExpression,
        }

        impl Operation {
            fn precedence(self) -> usize {
                match self {
                    Operation::Positive | Operation::Negative => 6,
                    Operation::Mul | Operation::Div => 5,
                    Operation::Add | Operation::Sub => 4,
                    Operation::EqualTo => 3,
                    Operation::Name => 2,
                    Operation::Tuple => 1,
                    Operation::SubExpression => 0,
                }
            }
        }

        impl From<Operation> for Node<'_> {
            fn from(op: Operation) -> Self {
                match op {
                    Operation::Positive => Node::Positive,
                    Operation::Negative => Node::Negative,
                    Operation::Mul => Node::Mul,
                    Operation::Div => Node::Div,
                    Operation::Add => Node::Add,
                    Operation::Sub => Node::Sub,
                    Operation::EqualTo => Node::EqualTo,
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
                        | Lexigram::EqualTo
                        | Lexigram::Comma
                        | Lexigram::Colon,
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
                while let Some(op) = stack.pop_if(|x| x.precedence() > operator.precedence()) {
                    // SubExpression has the lowest precedence, so this cannot panic.
                    output.push(op.into());
                }
                stack.push(operator);
            };
        loop {
            let unary_position = unary_position(last_token);
            let t = diagnostics.wrap(lexer.peek().copied());
            match t {
                // Terminals
                Some(Token {
                    lexigram: Lexigram::Number(number),
                    ..
                }) if unary_position => {
                    contents.push(Node::Number(number));
                }
                Some(Token {
                    lexigram: Lexigram::Ident(number),
                    ..
                }) if unary_position => {
                    contents.push(Node::Ident(number));
                }

                // A terminal value outside of unary position implies a function call,
                // so flush the operator stack.
                Some(Token {
                    lexigram: Lexigram::Number(number),
                    ..
                }) if !unary_position => {
                    flush(&mut contents, &mut stack);
                    contents.push(Node::Number(number));
                }
                Some(Token {
                    lexigram: Lexigram::Ident(number),
                    ..
                }) if !unary_position => {
                    flush(&mut contents, &mut stack);
                    contents.push(Node::Ident(number));
                }

                // # Operators
                // unary positive
                Some(Token {
                    lexigram: Lexigram::Plus,
                    ..
                }) if unary_position => {
                    push_with_precedence(&mut contents, &mut stack, Operation::Positive);
                }
                // binary add
                Some(Token {
                    lexigram: Lexigram::Plus,
                    ..
                }) if !unary_position => {
                    push_with_precedence(&mut contents, &mut stack, Operation::Add);
                }
                // unary negative
                Some(Token {
                    lexigram: Lexigram::Minus,
                    ..
                }) if unary_position => {
                    push_with_precedence(&mut contents, &mut stack, Operation::Negative);
                }
                // binary sub
                Some(Token {
                    lexigram: Lexigram::Minus,
                    ..
                }) if !unary_position => {
                    push_with_precedence(&mut contents, &mut stack, Operation::Sub);
                }
                // binary mul
                Some(Token {
                    lexigram: Lexigram::Star,
                    ..
                }) if !unary_position => {
                    push_with_precedence(&mut contents, &mut stack, Operation::Mul);
                }
                // binary div
                Some(Token {
                    lexigram: Lexigram::Slash,
                    ..
                }) if !unary_position => {
                    push_with_precedence(&mut contents, &mut stack, Operation::Div);
                }
                Some(Token {
                    lexigram: Lexigram::EqualTo,
                    ..
                }) if !unary_position => {
                    push_with_precedence(&mut contents, &mut stack, Operation::EqualTo);
                }
                // binary named tuple construction
                Some(Token {
                    lexigram: Lexigram::Colon,
                    ..
                }) if !unary_position => {
                    push_with_precedence(&mut contents, &mut stack, Operation::Name);
                }
                // binary tuple concatenation
                Some(Token {
                    lexigram: Lexigram::Comma,
                    ..
                }) if !unary_position => {
                    push_with_precedence(&mut contents, &mut stack, Operation::Tuple);
                }
                // parenthesized expressions
                Some(Token {
                    lexigram: Lexigram::OpenParen,
                    ..
                }) => {
                    stack.push(Operation::SubExpression);
                }
                Some(Token {
                    lexigram: Lexigram::CloseParen,
                    ..
                }) if !unary_position => {
                    while let Some(op) = stack.pop_if(|x| !matches!(x, Operation::SubExpression)) {
                        contents.push(op.into());
                    }
                    if !matches!(stack.pop(), Some(Operation::SubExpression)) {
                        diagnostics
                            .contents
                            .push(Diagnostic::Error(Error::UnexpectedCloseParen(t)))
                    }
                }
                // brace block
                Some(Token {
                    lexigram: Lexigram::OpenBrace,
                    ..
                }) => {
                    lexer.next();
                    contents.push(Node::Block(Block::from(Ast::from(&mut *lexer))));
                    diagnostics.expect(lexer.peek().copied(), &[Lexigram::CloseBrace]);
                }
                // if block
                Some(Token {
                    lexigram: Lexigram::If,
                    ..
                }) => {
                    contents.push(conditional(lexer));
                }
                // for block
                Some(Token {
                    lexigram: Lexigram::For,
                    ..
                }) => {
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
                    if !contents.is_empty() || !stack.is_empty() {
                        diagnostics
                            .contents
                            .push(Diagnostic::Error(Error::IncompleteExpression));
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
