use espy_eyes::{Lexer, Token, TokenType};
use std::iter::Peekable;

fn main() {
    for i in std::env::args().skip(1) {
        let mut ast = Ast::from(Lexer::from(i.as_str()).peekable());
        for statement in &mut ast {
            println!("{statement:?}");
        }
        let (_, result) = ast.close();
        println!("result: {result:?}");
    }
}

#[derive(Debug)]
struct Binding<'source> {
    ident: &'source str,
    ty: Option<&'source str>,
}

#[derive(Debug)]
struct Statement<'source> {
    binding: Option<Binding<'source>>,
    expression: Option<Expression<'source>>,
}

/// This type must not contain any incomplete expressions.
#[derive(Debug)]
struct Expression<'source>(Vec<ExpressionNode<'source>>);

#[derive(Debug)]
enum ExpressionNode<'source> {
    Operation(Operation),
    Number(&'source str),
    Ident(&'source str),
}

impl From<Operation> for ExpressionNode<'_> {
    fn from(op: Operation) -> Self {
        Self::Operation(op)
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum Operation {
    Add,
    Sub,
    Mul,
    Div,
    Tuple,

    Positive,
    Negative,
}

impl Operation {
    fn precedence(self) -> usize {
        match self {
            Operation::Positive | Operation::Negative => 3,
            Operation::Mul | Operation::Div => 2,
            Operation::Add | Operation::Sub => 1,
            Operation::Tuple => 0,
        }
    }
}

/// Parse an expression until an unexpected token is upcoming (via peek).
impl<'source, Iter: Iterator<Item = Token<'source>>> From<&mut Peekable<Iter>>
    for Expression<'source>
{
    fn from(lexer: &mut Peekable<Iter>) -> Self {
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
                        | TokenType::Comma,
                    ..
                })
            )
        };
        let flush = |output: &mut Vec<ExpressionNode>, stack: &mut Vec<Operation>| {
            while let Some(op) = stack.pop() {
                output.push(op.into());
            }
        };
        let push_with_precedence =
            |output: &mut Vec<ExpressionNode>, stack: &mut Vec<Operation>, operator: Operation| {
                while let Some(op) = stack.pop_if(|x| x.precedence() > operator.precedence()) {
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
                // binary tuple concatenation
                Some(Token {
                    ty: TokenType::Comma,
                    ..
                }) if !unary_position => {
                    push_with_precedence(&mut output, &mut stack, Operation::Tuple);
                }
                _ if !unary_position => {
                    flush(&mut output, &mut stack);
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

struct Ast<'source, Iter: Iterator<Item = Token<'source>>> {
    lexer: Peekable<Iter>,
    closed: Option<Expression<'source>>,
}

impl<'source, Iter: Iterator<Item = Token<'source>>> Ast<'source, Iter> {
    fn close(self) -> (Peekable<Iter>, Expression<'source>) {
        let Some(closed) = self.closed else {
            panic!("attempted to close ast without exausting its statements");
        };
        (self.lexer, closed)
    }
}

impl<'source, Iter: Iterator<Item = Token<'source>>> From<Iter> for Ast<'source, Iter> {
    fn from(lexer: Iter) -> Self {
        Self {
            lexer: lexer.peekable(),
            closed: None,
        }
    }
}

impl<'source, Iter: Iterator<Item = Token<'source>>> Iterator for Ast<'source, Iter> {
    type Item = Statement<'source>;
    fn next(&mut self) -> Option<Self::Item> {
        if self.closed.is_some() {
            return None;
        }
        let lexer = &mut self.lexer;
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
                })
                | None => Some(Statement {
                    binding: Some(Binding { ident, ty: None }),
                    expression: None,
                }),
                // TODO: Just log the erroneous statement and parse another expression. This is probably a forgotten = or ; and will parse fine.
                _ => panic!("expected =, ;, or EOF"),
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
