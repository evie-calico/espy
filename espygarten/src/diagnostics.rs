use espy::lexer;
use espy::parser::{Block, BlockResult, Error, Expression, Statement};
use std::fmt::Write;

// While this is comprehensive, lexigrams unused by `expect` have less thought put into them.
// These are marked with "symbol only".
fn format_lexigram(mut f: impl Write, lexigram: lexer::Lexigram) {
    let _ = match lexigram {
        lexer::Lexigram::Ampersand => write!(f, "& (ampersand)"),
        lexer::Lexigram::And => write!(f, "and"),
        lexer::Lexigram::BangEqual => write!(f, "!="), // symbol only
        lexer::Lexigram::Bang => write!(f, "! (bang)"),
        lexer::Lexigram::Caret => write!(f, "^ (caret)"),
        lexer::Lexigram::CloseBrace => write!(f, "}} (closing curly brace)"),
        lexer::Lexigram::CloseParen => write!(f, ") (closing parenthesis)"),
        lexer::Lexigram::CloseSquare => write!(f, "] (closing square bracket)"),
        lexer::Lexigram::Colon => write!(f, ": (colon)"),
        lexer::Lexigram::Comma => write!(f, ", (comma)"),
        lexer::Lexigram::Discard => write!(f, "_ (discard)"),
        lexer::Lexigram::DotDotEqual => write!(f, "..="), // symbol only
        lexer::Lexigram::DotDot => write!(f, ".."),       // symbol only
        lexer::Lexigram::Dot => write!(f, ". (dot)"),
        lexer::Lexigram::DotStar => write!(f, ".*"), // symbol only
        lexer::Lexigram::DoubleArrow => write!(f, "=> (double arrow)"),
        lexer::Lexigram::DoubleEqual => write!(f, "=="), // symbol only
        lexer::Lexigram::Ellipses => write!(f, "..."),   // symbol only
        lexer::Lexigram::Else => write!(f, "else"),
        lexer::Lexigram::End => write!(f, "end"),
        lexer::Lexigram::Enum => write!(f, "enum"),
        lexer::Lexigram::False => write!(f, "false"),
        lexer::Lexigram::GreaterEqual => write!(f, "greaterequal"), // symbol only
        lexer::Lexigram::Greater => write!(f, ">"),                 // symbol only
        lexer::Lexigram::Ident => write!(f, "identifier"),
        lexer::Lexigram::If => write!(f, "if"),
        lexer::Lexigram::LesserEqual => write!(f, "lesserequal"), // symbol only
        lexer::Lexigram::Lesser => write!(f, "lesser"),           // symbol only
        lexer::Lexigram::Let => write!(f, "let"),
        lexer::Lexigram::Match => write!(f, "match"),
        lexer::Lexigram::Minus => write!(f, "minus"), // symbol only
        lexer::Lexigram::Number => write!(f, "number"),
        lexer::Lexigram::OpenBrace => write!(f, "{{ (opening curly brace)"),
        lexer::Lexigram::OpenParen => write!(f, "( (opening parenthesis)"),
        lexer::Lexigram::OpenSquare => write!(f, "[ (opening square bracket)"),
        lexer::Lexigram::Or => write!(f, "or"),
        lexer::Lexigram::Pipe => write!(f, "|"), // symbol only
        lexer::Lexigram::Plus => write!(f, "plus"), // symbol only
        lexer::Lexigram::Semicolon => write!(f, "; (semicolon)"),
        lexer::Lexigram::Set => write!(f, "set"),
        lexer::Lexigram::SingleArrow => write!(f, "-> (single arrow)"),
        lexer::Lexigram::SingleEqual => write!(f, "= (equals)"),
        lexer::Lexigram::Slash => write!(f, "slash"), // symbol only
        lexer::Lexigram::Star => write!(f, "star"),   // symbol only
        lexer::Lexigram::String => write!(f, "string"),
        lexer::Lexigram::Then => write!(f, "then"),
        lexer::Lexigram::Triangle => write!(f, "triangle"), // symbol only
        lexer::Lexigram::True => write!(f, "true"),
        lexer::Lexigram::With => write!(f, "with"),
    };
}

pub struct Comment {
    pub message: String,
    pub range: Option<(usize, usize)>,
}

pub struct Diagnostic {
    pub primary: Comment,
    pub secondary: Vec<Comment>,
}

impl Diagnostic {
    fn from_error(error: &Error<'_>, source: &str) -> Self {
        match error {
            Error::Lexer(lexer::Error {
                origin,
                kind: lexer::ErrorKind::UnexpectedCharacter,
            }) => Self {
                primary: Comment {
                    message: format!("unexpected character: {origin}"),
                    range: Some(origin_range(origin, source)),
                },
                secondary: Vec::new(),
            },
            Error::Lexer(lexer::Error {
                origin,
                kind: lexer::ErrorKind::ReservedSymbol,
            }) => Self {
                primary: Comment {
                    message: format!("reserved symbol: {origin}"),
                    range: Some(origin_range(origin, source)),
                },
                secondary: Vec::new(),
            },
            Error::Lexer(lexer::Error {
                origin,
                kind: lexer::ErrorKind::UnterminatedString,
            }) => Self {
                primary: Comment {
                    message: "unterminated string".to_string(),
                    range: Some(origin_range(origin, source)),
                },
                secondary: Vec::new(),
            },
            Error::Lexer(lexer::Error {
                origin,
                kind: lexer::ErrorKind::UnterminatedIdentifier,
            }) => Self {
                primary: Comment {
                    message: "unterminated identifier".to_string(),
                    range: Some(origin_range(origin, source)),
                },
                secondary: Vec::new(),
            },
            Error::MissingToken { expected, actual } => {
                let mut message = "expected ".to_string();
                format_lexigram(
                    &mut message,
                    *expected
                        .first()
                        .expect("must be at least one expected token"),
                );
                for expected in expected.iter().skip(1) {
                    message.push_str(", or ");
                    format_lexigram(&mut message, *expected);
                }
                message.push_str(", got ");
                if let Some(actual) = actual {
                    format_lexigram(&mut message, actual.lexigram);
                } else {
                    message.push_str("end of stream");
                }

                Self {
                    primary: Comment {
                        message,
                        range: if let Some(actual) = actual {
                            Some(origin_range(actual.origin, source))
                        } else {
                            Some((source.len(), source.len()))
                        },
                    },
                    secondary: Vec::new(),
                }
            }
            Error::UnexpectedCloseParen(token) => Self {
                primary: Comment {
                    message: "unexpected closing parenthesis".to_string(),
                    range: Some(origin_range(token.origin, source)),
                },
                secondary: Vec::new(),
            },
            Error::IncompleteExpression => Self {
                primary: Comment {
                    message: "incomplete expression".to_string(),
                    range: None,
                },
                secondary: Vec::new(),
            },
            Error::ExpectedExpression => Self {
                primary: Comment {
                    message: "expected expression".to_string(),
                    range: None,
                },
                secondary: Vec::new(),
            },
            Error::ExpectedStatementOrExpression(token) => Self {
                primary: Comment {
                    message: "expected statement or expression".to_string(),
                    range: Some(origin_range(token.origin, source)),
                },
                secondary: Vec::new(),
            },
        }
    }
}

pub fn origin_range(origin: &str, source: &str) -> (usize, usize) {
    let start = origin.as_ptr() as isize - source.as_ptr() as isize;
    let end = start + origin.len() as isize;
    if start < 0 || end - start > source.len() as isize {
        panic!("source string does not contain token origin");
    }
    (start as usize, end as usize)
}

fn expression_origin(expression: &Expression<'_>, source: &str) -> Option<(usize, usize)> {
    expression.first_token.map(|first_token| {
        let first_range = origin_range(first_token.origin, source);
        (
            first_range.0,
            expression
                .last_token
                .map(|last_token| origin_range(last_token.origin, source).1)
                // I think this can only ever be reached by an incomplete
                // expression consisting of only one token.
                .unwrap_or(first_range.1),
        )
    })
}

pub fn for_each(source: &str, block: &Block, mut for_each: impl FnMut(Diagnostic)) {
    diagnose_block(source, block, &mut for_each);
}

fn diagnose_block(source: &str, block: &Block, for_each: &mut impl FnMut(Diagnostic)) {
    for error in &block.diagnostics.errors {
        for_each(Diagnostic::from_error(error, source));
    }
    for statement in &block.statements {
        diagnose_statement(source, statement, &mut *for_each)
    }
    match &block.result {
        BlockResult::Expression(expression) => {
            diagnose_expression(source, expression, &mut *for_each)
        }
        BlockResult::Function(function) => {
            for error in &function.diagnostics.errors {
                for_each(Diagnostic::from_error(error, source));
            }
            diagnose_block(source, &function.block, &mut *for_each);
        }
    }
}

fn diagnose_statement(source: &str, statement: &Statement, for_each: &mut impl FnMut(Diagnostic)) {
    match statement {
        Statement::Evaluation(evaluation) => {
            let let_range = evaluation
                .binding
                .as_ref()
                .map(|binding| origin_range(binding.let_token.origin, source));
            let anchored_range = evaluation
                .expression
                .as_ref()
                .and_then(|x| x.first_token)
                .map(|first_token| {
                    let (first, last) = origin_range(first_token.origin, source);
                    (let_range.map_or(first, |(x, _)| x), last)
                })
                .or(let_range);
            for error in &evaluation.diagnostics.errors {
                let mut diagnostic = Diagnostic::from_error(error, source);
                if let Some(anchored_range) = anchored_range
                    && diagnostic
                        .primary
                        .range
                        .is_none_or(|range| range.0 > anchored_range.1)
                {
                    diagnostic.secondary.push(Comment {
                        message: if evaluation.binding.is_some() {
                            "for this binding"
                        } else {
                            "for this expression"
                        }
                        .to_string(),
                        range: Some(anchored_range),
                    })
                }
                for_each(diagnostic);
            }
            diagnose_expression(source, &evaluation.expression, &mut *for_each);
        }
        Statement::Set(set) => {
            let set_range = origin_range(set.set_token.origin, source);
            let anchored_range = set
                .expression
                .as_ref()
                .and_then(|x| x.first_token)
                .map(|first_token| {
                    let (_, last) = origin_range(first_token.origin, source);
                    (set_range.0, last)
                })
                .unwrap_or(set_range);
            for error in &set.diagnostics.errors {
                let mut diagnostic = Diagnostic::from_error(error, source);
                if diagnostic
                    .primary
                    .range
                    .is_none_or(|range| range.0 > anchored_range.1)
                {
                    diagnostic.secondary.push(Comment {
                        message: "for this assignment".to_string(),
                        range: Some(anchored_range),
                    })
                }
                for_each(diagnostic);
            }
            diagnose_expression(source, &set.target, &mut *for_each);
            diagnose_expression(source, &set.expression, &mut *for_each);
        }
    }
}

fn diagnose_expression(
    source: &str,
    expression: &Option<Box<Expression>>,
    for_each: &mut impl FnMut(Diagnostic),
) {
    let Some(expression) = expression else {
        return;
    };
    let range = expression_origin(expression, source);
    for error in &expression.diagnostics.errors {
        let mut diagnostic = Diagnostic::from_error(error, source);
        if let Some(range) = range {
            diagnostic.secondary.push(Comment {
                message: "in this expression".to_string(),
                range: Some(range),
            })
        }
        for_each(diagnostic);
    }
    for node in &expression.contents {
        match node {
            espy::parser::Node::Block(block) => diagnose_block(source, block, for_each),
            espy::parser::Node::If(if_node) => {
                let mut range = origin_range(if_node.if_token.origin, source);
                if let Some(token) = if_node
                    .end_token
                    .or(if_node.else_token)
                    .or(if_node.then_token)
                {
                    range.1 = origin_range(token.origin, source).1;
                }
                for error in &if_node.diagnostics.errors {
                    let mut diagnostic = Diagnostic::from_error(error, source);
                    diagnostic.secondary.push(Comment {
                        message: "in this conditional block".to_string(),
                        range: Some(range),
                    });
                    for_each(diagnostic);
                }
                diagnose_expression(source, &if_node.condition, for_each);
                diagnose_block(source, &if_node.first, for_each);
                diagnose_block(source, &if_node.second, for_each);
            }
            espy::parser::Node::Match(match_node) => {
                let mut range = origin_range(match_node.match_token.origin, source);
                if let Some(token) = match_node.end_token.or(match_node.then_token) {
                    range.1 = origin_range(token.origin, source).1;
                }
                for error in &match_node.diagnostics.errors {
                    let mut diagnostic = Diagnostic::from_error(error, source);
                    diagnostic.secondary.push(Comment {
                        message: "in this match block".to_string(),
                        range: Some(range),
                    });
                    for_each(diagnostic);
                }
                diagnose_expression(source, &match_node.expression, for_each);
                for case in &match_node.cases {
                    diagnose_expression(source, &case.case, for_each);
                    diagnose_expression(source, &case.expression, for_each);
                }
            }
            espy::parser::Node::Enum(enum_node) => {
                let mut range = origin_range(enum_node.enum_token.origin, source);
                if let Some(token) = enum_node.end_token {
                    range.1 = origin_range(token.origin, source).1;
                }
                for error in &enum_node.diagnostics.errors {
                    let mut diagnostic = Diagnostic::from_error(error, source);
                    diagnostic.secondary.push(Comment {
                        message: "in this structure definition".to_string(),
                        range: Some(range),
                    });
                    for_each(diagnostic);
                }
                diagnose_expression(source, &enum_node.variants, for_each);
            }
            _ => {}
        }
    }
}
