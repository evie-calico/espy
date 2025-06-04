use espy_eyes::{Lexer, Token, TokenType};
use std::iter::Peekable;

#[cfg(test)]
mod tests;

#[derive(Debug, Eq, PartialEq)]
pub struct Binding<'source> {
    pub ident: &'source str,
    pub ty: Option<&'source str>,
}

#[derive(Debug, Eq, PartialEq)]
pub struct Statement<'source> {
    pub binding: Option<Binding<'source>>,
    pub expression: Option<Expression<'source>>,
}

/// This type must not contain any incomplete expressions.
#[derive(Debug, Eq, PartialEq)]
pub struct Expression<'source>(
    // TODO: This field should be exposed through an iterator or method, not pub.
    pub Vec<ExpressionNode<'source>>,
);

#[derive(Debug, Eq, PartialEq)]
pub enum ExpressionNode<'source> {
    Number(&'source str),
    Ident(&'source str),
    Block {
        statements: Box<[Statement<'source>]>,
        result: Expression<'source>,
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
            Operation::SubExpression => panic!("sub expressions may not enter the output stack"),
        }
    }
}

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

/// Parse an expression until an unexpected token is upcoming (via peek).
impl<'source> From<&mut Peekable<Lexer<'source>>> for Expression<'source> {
    fn from(lexer: &mut Peekable<Lexer<'source>>) -> Self {
        let mut output = Vec::new();
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
            match lexer.peek() {
                // Terminals
                Some(Token {
                    ty: TokenType::Number(number),
                    ..
                }) if unary_position => {
                    output.push(ExpressionNode::Number(number));
                }
                Some(Token {
                    ty: TokenType::Ident(number),
                    ..
                }) if unary_position => {
                    output.push(ExpressionNode::Ident(number));
                }

                // A terminal value outside of unary position implies a function call,
                // so flush the operator stack.
                Some(Token {
                    ty: TokenType::Number(number),
                    ..
                }) if !unary_position => {
                    flush(&mut output, &mut stack);
                    output.push(ExpressionNode::Number(number));
                }
                Some(Token {
                    ty: TokenType::Ident(number),
                    ..
                }) if !unary_position => {
                    flush(&mut output, &mut stack);
                    output.push(ExpressionNode::Ident(number));
                }

                // # Operators
                // unary positive
                Some(Token {
                    ty: TokenType::Plus,
                    ..
                }) if unary_position => {
                    push_with_precedence(&mut output, &mut stack, Operation::Positive);
                }
                // binary add
                Some(Token {
                    ty: TokenType::Plus,
                    ..
                }) if !unary_position => {
                    push_with_precedence(&mut output, &mut stack, Operation::Add);
                }
                // unary negative
                Some(Token {
                    ty: TokenType::Minus,
                    ..
                }) if unary_position => {
                    push_with_precedence(&mut output, &mut stack, Operation::Negative);
                }
                // binary sub
                Some(Token {
                    ty: TokenType::Minus,
                    ..
                }) if !unary_position => {
                    push_with_precedence(&mut output, &mut stack, Operation::Sub);
                }
                // binary mul
                Some(Token {
                    ty: TokenType::Star,
                    ..
                }) if !unary_position => {
                    push_with_precedence(&mut output, &mut stack, Operation::Mul);
                }
                // binary div
                Some(Token {
                    ty: TokenType::Slash,
                    ..
                }) if !unary_position => {
                    push_with_precedence(&mut output, &mut stack, Operation::Div);
                }
                // binary named tuple construction
                Some(Token {
                    ty: TokenType::Colon,
                    ..
                }) if !unary_position => {
                    push_with_precedence(&mut output, &mut stack, Operation::Name);
                }
                // binary tuple concatenation
                Some(Token {
                    ty: TokenType::Comma,
                    ..
                }) if !unary_position => {
                    push_with_precedence(&mut output, &mut stack, Operation::Tuple);
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
                        output.push(op.into());
                    }
                    if !matches!(stack.pop(), Some(Operation::SubExpression)) {
                        panic!("closing parenthesis without matching opening parenthesis")
                    }
                }
                // brace block
                Some(Token {
                    ty: TokenType::OpenBrace,
                    ..
                }) => {
                    lexer.next();
                    let mut ast = Ast::from(&mut *lexer);
                    // This should be a collect but i don't know how to express the `&mut` part.
                    let mut statements = Vec::new();
                    for statement in &mut ast {
                        statements.push(statement);
                    }
                    let result = ast.close();
                    output.push(ExpressionNode::Block {
                        statements: statements.into_boxed_slice(),
                        result,
                    });
                    if !matches!(
                        lexer.peek(),
                        Some(Token {
                            ty: TokenType::CloseBrace,
                            ..
                        })
                    ) {
                        panic!("expected }}");
                    }
                }
                _ if !unary_position => {
                    flush(&mut output, &mut stack);
                    if !stack.is_empty() {
                        panic!("opening parenthesis without matching closing parenthesis");
                    }
                    return Expression(output);
                }
                _ => {
                    if output.is_empty() && stack.is_empty() {
                        return Expression(Vec::new());
                    }
                    panic!("incomplete expression");
                }
            }
            last_token = lexer.next();
        }
    }
}

pub struct Ast<'source, 'iter> {
    lexer: &'iter mut Peekable<Lexer<'source>>,
    closed: Option<Expression<'source>>,
}
impl<'source> Ast<'source, '_> {
    /// # Panics
    ///
    /// Panics if you call this function before exhausting the Ast of its statements.
    ///
    /// The simplest way to ensure this doesn't happen is to use it in a `for` loop first.
    /// Make sure you write `for _ in &mut ast {}` instead of `for _ in ast {}`,
    /// so that the for loop doesn't move `ast`.
    pub fn close(self) -> Expression<'source> {
        let Some(closed) = self.closed else {
            panic!("attempted to close ast without exhausting its statements");
        };
        closed
    }
}

impl<'source, 'iter> From<&'iter mut Peekable<Lexer<'source>>> for Ast<'source, 'iter> {
    fn from(lexer: &'iter mut Peekable<Lexer<'source>>) -> Self {
        Self {
            lexer,
            closed: None,
        }
    }
}

impl<'source> Iterator for Ast<'source, '_> {
    type Item = Statement<'source>;
    fn next(&mut self) -> Option<Self::Item> {
        if self.closed.is_some() {
            return None;
        }
        let lexer = &mut *self.lexer;
        if lexer.next_if(|x| x.ty == TokenType::Let).is_some() {
            let Some(Token {
                ty: TokenType::Ident(ident),
                ..
            }) = lexer.next()
            else {
                panic!("expected binding following `let`");
            };
            match lexer.next() {
                Some(Token {
                    ty: TokenType::Equals,
                    ..
                }) => {
                    let expression = Expression::from(&mut *lexer);
                    let Some(Token {
                        ty: TokenType::Semicolon,
                        ..
                    }) = lexer.next()
                    else {
                        // Actually, there's no reason syntactically that a } couldn't terminate an assignment,
                        // but a semicolon is more correct.
                        // That said, this is the type of error where we should continue parsing once error handling is implemented.
                        panic!("let expression must be terminated by a semicolon");
                    };
                    Some(Statement {
                        binding: Some(Binding { ident, ty: None }),
                        expression: Some(expression),
                    })
                }
                Some(Token {
                    ty: TokenType::Semicolon,
                    ..
                }) => Some(Statement {
                    binding: Some(Binding { ident, ty: None }),
                    expression: None,
                }),
                // TODO: Just log the erroneous statement and parse another expression. This is probably a forgotten = or ; and will parse fine.
                _ => panic!("expected = or ;"),
            }
        } else {
            let expression = Expression::from(&mut *lexer);
            if lexer
                .next_if(|x| matches!(x.ty, TokenType::Semicolon))
                .is_some()
            {
                Some(Statement {
                    binding: None,
                    expression: Some(expression),
                })
            } else {
                self.closed = Some(expression);
                None
            }
        }
    }
}
