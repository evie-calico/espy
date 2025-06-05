use espy_eyes::{Lexer, Token, TokenType, UnexpectedCharacter};
use std::iter::Peekable;

#[cfg(test)]
mod tests;

#[derive(Debug, Eq, PartialEq)]
pub enum Error<'source> {
    UnexpectedCharacter(UnexpectedCharacter),
    MissingToken {
        expected: &'static [TokenType<'static>],
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
        t: Option<Result<Token<'source>, UnexpectedCharacter>>,
        expected: &'static [TokenType<'static>],
    ) -> Option<Token<'source>> {
        let actual = self.wrap(t);
        if actual.is_some_and(|actual| expected.contains(&actual.ty)) {
            actual
        } else {
            self.contents
                .push(Diagnostic::Error(Error::MissingToken { expected, actual }));
            None
        }
    }

    fn wrap(
        &mut self,
        t: Option<Result<Token<'source>, UnexpectedCharacter>>,
    ) -> Option<Token<'source>> {
        match t? {
            Ok(t) => Some(t),
            Err(e) => {
                self.contents
                    .push(Diagnostic::Error(Error::UnexpectedCharacter(e)));
                None
            }
        }
    }
}

#[derive(Debug, Eq, PartialEq)]
pub struct Binding<'source> {
    pub ident: &'source str,
    pub ty: Option<&'source str>,
}

#[derive(Debug, Default, Eq, PartialEq)]
pub struct Statement<'source> {
    pub binding: Option<Binding<'source>>,
    pub expression: Option<Expression<'source>>,
    pub diagnostics: Diagnostics<'source>,
}

#[derive(Debug, Eq, PartialEq)]
pub enum ExpressionNode<'source> {
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
    Name,
    Tuple,
}

/// This type must not contain any incomplete expressions.
#[derive(Debug, Default, Eq, PartialEq)]
pub struct Expression<'source> {
    // TODO: This field should be exposed through an iterator or method, not pub.
    contents: Vec<ExpressionNode<'source>>,
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

            Name,

            Tuple,

            SubExpression,
        }

        impl Operation {
            fn precedence(self) -> usize {
                match self {
                    Operation::Positive | Operation::Negative => 5,
                    Operation::Mul | Operation::Div => 4,
                    Operation::Add | Operation::Sub => 3,
                    Operation::Name => 2,
                    Operation::Tuple => 1,
                    Operation::SubExpression => 0,
                }
            }
        }

        impl From<Operation> for ExpressionNode<'_> {
            fn from(op: Operation) -> Self {
                match op {
                    Operation::Positive => ExpressionNode::Positive,
                    Operation::Negative => ExpressionNode::Negative,
                    Operation::Mul => ExpressionNode::Mul,
                    Operation::Div => ExpressionNode::Div,
                    Operation::Add => ExpressionNode::Add,
                    Operation::Sub => ExpressionNode::Sub,
                    Operation::Name => ExpressionNode::Name,
                    Operation::Tuple => ExpressionNode::Tuple,
                    Operation::SubExpression => {
                        panic!("sub expressions may not enter the output stack")
                    }
                }
            }
        }

        fn conditional<'source>(lexer: &mut Peekable<Lexer<'source>>) -> ExpressionNode<'source> {
            lexer.next();
            let mut diagnostics = Diagnostics::default();
            let condition = Expression::from(&mut *lexer);
            if diagnostics
                .expect(lexer.peek().copied(), &[TokenType::Then])
                .is_some()
            {
                lexer.next();
            }
            let first = Block::from(Ast::from(&mut *lexer));
            let second = if matches!(
                diagnostics.wrap(lexer.peek().copied()),
                Some(Token {
                    ty: TokenType::Else,
                    ..
                })
            ) {
                lexer.next();
                match diagnostics.wrap(lexer.peek().copied()) {
                    Some(Token {
                        ty: TokenType::Then,
                        ..
                    }) => {
                        lexer.next();
                        Block::from(Ast::from(&mut *lexer))
                    }
                    Some(Token {
                        ty: TokenType::If, ..
                    }) => Block {
                        statements: Vec::new(),
                        result: Expression {
                            contents: vec![conditional(&mut *lexer)],
                            diagnostics: Diagnostics::default(),
                        },
                        diagnostics: Diagnostics::default(),
                    },
                    _ => {
                        diagnostics
                            .expect(lexer.peek().copied(), &[TokenType::Then, TokenType::If]);
                        Block::default()
                    }
                }
            } else {
                Block::default()
            };
            diagnostics.expect(lexer.peek().copied(), &[TokenType::End]);
            ExpressionNode::If {
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
                    ty: TokenType::Plus
                        | TokenType::Minus
                        | TokenType::Star
                        | TokenType::Slash
                        | TokenType::Comma
                        | TokenType::Colon,
                    ..
                })
            )
        };
        let flush = |output: &mut Vec<ExpressionNode>, stack: &mut Vec<Operation>| {
            while let Some(op) = stack.pop_if(|x| !matches!(x, Operation::SubExpression)) {
                output.push(op.into());
            }
        };
        let push_with_precedence =
            |output: &mut Vec<ExpressionNode>, stack: &mut Vec<Operation>, operator: Operation| {
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
                    ty: TokenType::Number(number),
                    ..
                }) if unary_position => {
                    contents.push(ExpressionNode::Number(number));
                }
                Some(Token {
                    ty: TokenType::Ident(number),
                    ..
                }) if unary_position => {
                    contents.push(ExpressionNode::Ident(number));
                }

                // A terminal value outside of unary position implies a function call,
                // so flush the operator stack.
                Some(Token {
                    ty: TokenType::Number(number),
                    ..
                }) if !unary_position => {
                    flush(&mut contents, &mut stack);
                    contents.push(ExpressionNode::Number(number));
                }
                Some(Token {
                    ty: TokenType::Ident(number),
                    ..
                }) if !unary_position => {
                    flush(&mut contents, &mut stack);
                    contents.push(ExpressionNode::Ident(number));
                }

                // # Operators
                // unary positive
                Some(Token {
                    ty: TokenType::Plus,
                    ..
                }) if unary_position => {
                    push_with_precedence(&mut contents, &mut stack, Operation::Positive);
                }
                // binary add
                Some(Token {
                    ty: TokenType::Plus,
                    ..
                }) if !unary_position => {
                    push_with_precedence(&mut contents, &mut stack, Operation::Add);
                }
                // unary negative
                Some(Token {
                    ty: TokenType::Minus,
                    ..
                }) if unary_position => {
                    push_with_precedence(&mut contents, &mut stack, Operation::Negative);
                }
                // binary sub
                Some(Token {
                    ty: TokenType::Minus,
                    ..
                }) if !unary_position => {
                    push_with_precedence(&mut contents, &mut stack, Operation::Sub);
                }
                // binary mul
                Some(Token {
                    ty: TokenType::Star,
                    ..
                }) if !unary_position => {
                    push_with_precedence(&mut contents, &mut stack, Operation::Mul);
                }
                // binary div
                Some(Token {
                    ty: TokenType::Slash,
                    ..
                }) if !unary_position => {
                    push_with_precedence(&mut contents, &mut stack, Operation::Div);
                }
                // binary named tuple construction
                Some(Token {
                    ty: TokenType::Colon,
                    ..
                }) if !unary_position => {
                    push_with_precedence(&mut contents, &mut stack, Operation::Name);
                }
                // binary tuple concatenation
                Some(Token {
                    ty: TokenType::Comma,
                    ..
                }) if !unary_position => {
                    push_with_precedence(&mut contents, &mut stack, Operation::Tuple);
                }
                // parenthesized expressions
                Some(Token {
                    ty: TokenType::OpenParen,
                    ..
                }) => {
                    stack.push(Operation::SubExpression);
                }
                Some(Token {
                    ty: TokenType::CloseParen,
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
                    ty: TokenType::OpenBrace,
                    ..
                }) => {
                    lexer.next();
                    contents.push(ExpressionNode::Block(Block::from(Ast::from(&mut *lexer))));
                    diagnostics.expect(lexer.peek().copied(), &[TokenType::CloseBrace]);
                }
                // if block
                Some(Token {
                    ty: TokenType::If, ..
                }) => {
                    contents.push(conditional(lexer));
                }
                // for block
                Some(Token {
                    ty: TokenType::For, ..
                }) => {
                    lexer.next();
                    let mut diagnostics = Diagnostics::default();

                    let binding = match diagnostics.wrap(lexer.peek().copied()) {
                        Some(Token {
                            ty: TokenType::Ident(ident),
                            ..
                        }) => {
                            lexer.next();
                            Some(ident)
                        }
                        Some(Token {
                            ty: TokenType::Discard,
                            ..
                        }) => {
                            lexer.next();
                            None
                        }
                        _ => {
                            diagnostics.expect(
                                lexer.peek().copied(),
                                &[TokenType::Ident(""), TokenType::Discard],
                            );
                            None
                        }
                    };
                    if diagnostics
                        .expect(lexer.peek().copied(), &[TokenType::In])
                        .is_some()
                    {
                        lexer.next();
                    }
                    let iterator = Expression::from(&mut *lexer);
                    if diagnostics
                        .expect(lexer.peek().copied(), &[TokenType::Then])
                        .is_some()
                    {
                        lexer.next();
                    }
                    let first = Block::from(Ast::from(&mut *lexer));
                    let result = match diagnostics.wrap(lexer.peek().copied()) {
                        Some(Token {
                            ty: TokenType::End, ..
                        }) => ExpressionNode::For {
                            binding,
                            iterator,
                            first,
                            second: Block::default(),
                            diagnostics,
                        },
                        Some(Token {
                            ty: TokenType::Else,
                            ..
                        }) => {
                            lexer.next();
                            let second = Block::from(Ast::from(&mut *lexer));
                            diagnostics.expect(lexer.peek().copied(), &[TokenType::End]);
                            ExpressionNode::For {
                                binding,
                                iterator,
                                first,
                                second,
                                diagnostics,
                            }
                        }
                        _ => {
                            diagnostics
                                .expect(lexer.peek().copied(), &[TokenType::End, TokenType::Else]);
                            ExpressionNode::For {
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
                        diagnostics.expect(None, &[TokenType::CloseParen]);
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
        if let Some(Token {
            ty: TokenType::Let, ..
        }) = self.diagnostics.wrap(self.lexer.peek().copied())
        {
            self.lexer.next();
            let mut st_diagnostics = Diagnostics::default();
            let t = st_diagnostics.wrap(self.lexer.next());
            if let Some(Token {
                ty: TokenType::Ident(ident),
                ..
            }) = t
            {
                match st_diagnostics.wrap(self.lexer.peek().copied()) {
                    Some(Token {
                        ty: TokenType::Equals,
                        ..
                    }) => {
                        self.lexer.next();
                        let expression = Expression::from(&mut *self.lexer);
                        if st_diagnostics
                            .expect(self.lexer.peek().copied(), &[TokenType::Semicolon])
                            .is_some()
                        {
                            self.lexer.next();
                        }
                        Some(Statement {
                            binding: Some(Binding { ident, ty: None }),
                            expression: Some(expression),
                            diagnostics: st_diagnostics,
                        })
                    }
                    Some(Token {
                        ty: TokenType::Semicolon,
                        ..
                    }) => {
                        self.lexer.next();
                        Some(Statement {
                            binding: Some(Binding { ident, ty: None }),
                            expression: None,
                            diagnostics: st_diagnostics,
                        })
                    }
                    _ => {
                        st_diagnostics.expect(
                            self.lexer.peek().copied(),
                            &[TokenType::Equals, TokenType::Semicolon],
                        );
                        Some(Statement {
                            binding: Some(Binding { ident, ty: None }),
                            expression: None,
                            diagnostics: st_diagnostics,
                        })
                    }
                }
            } else {
                st_diagnostics
                    .contents
                    .push(Diagnostic::Error(Error::MissingToken {
                        expected: &[TokenType::Ident("")],
                        actual: t,
                    }));
                Some(Statement {
                    binding: None,
                    expression: None,
                    diagnostics: st_diagnostics,
                })
            }
        } else {
            let mut st_diagnostics = Diagnostics::default();
            let expression = Expression::from(&mut *self.lexer);
            if let Some(Token {
                ty: TokenType::Semicolon,
                ..
            }) = st_diagnostics.wrap(self.lexer.peek().copied())
            {
                self.lexer.next();
                Some(Statement {
                    binding: None,
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
