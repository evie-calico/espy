use crate::{Ast, Binding, Expression, ExpressionNode, Statement};
use espy_eyes::Lexer;

#[test]
fn assignment() {
    let source = "let x = 1;";
    let mut lexer = Lexer::from(source).peekable();
    let expected = [Statement {
        binding: Some(Binding {
            ident: "x",
            ty: None,
        }),
        expression: Some(Expression(vec![ExpressionNode::Number("1")])),
    }];
    assert!(Ast::from(&mut lexer).eq(expected));
}

#[test]
fn tuples() {
    let source = "1 * 2, 3";
    let mut lexer = Lexer::from(source).peekable();
    let mut ast = Ast::from(&mut lexer);
    ast.next();
    let expected = Expression(vec![
        ExpressionNode::Number("1"),
        ExpressionNode::Number("2"),
        ExpressionNode::Mul,
        ExpressionNode::Number("3"),
        ExpressionNode::Tuple,
    ]);
    assert_eq!(ast.close(), expected);
}

#[test]
fn named_tuple() {
    let source = "x: 1, y: 2";
    let mut lexer = Lexer::from(source).peekable();
    let mut ast = Ast::from(&mut lexer);
    ast.next();
    let expected = Expression(vec![
        ExpressionNode::Ident("x"),
        ExpressionNode::Number("1"),
        ExpressionNode::Name,
        ExpressionNode::Ident("y"),
        ExpressionNode::Number("2"),
        ExpressionNode::Name,
        ExpressionNode::Tuple,
    ]);
    assert_eq!(ast.close(), expected);
}

#[test]
fn block_expression() {
    let source = "let x = { let y = 2; y * 3 };";
    let mut lexer = Lexer::from(source).peekable();
    let expected = [Statement {
        binding: Some(Binding {
            ident: "x",
            ty: None,
        }),
        expression: Some(Expression(vec![ExpressionNode::Block {
            statements: [Statement {
                binding: Some(Binding {
                    ident: "y",
                    ty: None,
                }),
                expression: Some(Expression(vec![ExpressionNode::Number("2")])),
            }]
            .into(),
            result: Expression(vec![
                ExpressionNode::Ident("y"),
                ExpressionNode::Number("3"),
                ExpressionNode::Mul,
            ]),
        }])),
    }];
    assert!(Ast::from(&mut lexer).eq(expected));
}
