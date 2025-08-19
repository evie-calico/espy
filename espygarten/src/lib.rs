mod diagnostics;

use crate::diagnostics::origin_range;
use std::fmt::Write;
use std::{cell::RefCell, rc::Rc};
use wasm_bindgen::prelude::*;

#[derive(Debug, Default)]
struct EspygartenLibContainer {
    std: espystandard::StdLib,
    espygarten: EspygartenLib,
}

impl espyscript::Extern for EspygartenLibContainer {
    fn index<'host>(
        &'host self,
        index: espyscript::Value<'host>,
    ) -> Result<espyscript::Value<'host>, espyscript::interpreter::Error<'host>> {
        match index {
            espyscript::Value {
                storage: espyscript::Storage::String(index),
            } if &*index == "std" => Ok(espyscript::Value::borrow(&self.std)),
            espyscript::Value {
                storage: espyscript::Storage::String(index),
            } if &*index == "espygarten" => Ok(espyscript::Value::borrow(&self.espygarten)),
            index => Err(espyscript::Error::IndexNotFound {
                index,
                container: espyscript::Storage::Borrow(self).into(),
            }),
        }
    }

    fn debug(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::write!(f, "espygarten libraries")
    }
}

#[derive(Debug, Default)]
struct EspygartenLib {
    print: PrintFn,
}

impl espyscript::Extern for EspygartenLib {
    fn index<'host>(
        &'host self,
        index: espyscript::Value<'host>,
    ) -> Result<espyscript::Value<'host>, espyscript::interpreter::Error<'host>> {
        match index {
            espyscript::Value {
                storage: espyscript::Storage::String(index),
            } if &*index == "print" => Ok(espyscript::Function::borrow(&self.print).into()),
            index => Err(espyscript::Error::IndexNotFound {
                index,
                container: espyscript::Storage::Borrow(self).into(),
            }),
        }
    }

    fn debug(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::write!(f, "espygarten module")
    }
}

#[derive(Debug, Default)]
struct PrintFn {
    output: RefCell<String>,
}

impl espyscript::ExternFn for PrintFn {
    fn call<'host>(
        &'host self,
        message: espyscript::Value<'host>,
    ) -> Result<espyscript::Value<'host>, espyscript::Error<'host>> {
        let message = message.into_str()?;
        let mut output = self.output.borrow_mut();
        output.push_str(&message);
        output.push('\n');
        Ok(().into())
    }

    fn debug(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::write!(f, "std.io.print function")
    }
}

/// Returns a tuple of two tuples containing the line and column number for the start and end positions, respectively.
pub fn find_location(start: usize, end: usize, source: &str) -> ((usize, usize), (usize, usize)) {
    // Count all newlines that occur before the starting position...
    let start_line = 1 + source[..start].chars().filter(|c| *c == '\n').count();
    // ...and all characters that come before the start character and the last newline.
    let start_column = 1 + source[..start]
        .chars()
        .rev()
        .take_while(|c| *c != '\n')
        .count();
    // This is only counting the number of newlines between the start and end,
    // and adding it the the starting position's line number.
    let end_line = start_line + source[start..end].chars().filter(|c| *c == '\n').count();
    // Unlike end_line, end_column cannot be offset by start_column because the start and end lines may differ.
    let end_column = 1 + source[..end]
        .chars()
        .rev()
        .take_while(|c| *c != '\n')
        .count();
    ((start_line, start_column), (end_line, end_column))
}

/// Returns the lines containing the provided range.
pub fn expand_to_snippet(start: usize, end: usize, source: &str) -> &str {
    let snippet_start = source[..start]
        .char_indices()
        .rev()
        .find(|(_, c)| *c == '\n')
        .map(|(at, _)| at + "\n".len())
        .unwrap_or(0);
    let snippet_end = source[end..]
        .char_indices()
        .find(|(_, c)| *c == '\n')
        // char_indices is treating `end` as the 0th position
        .map(|(at, _)| at + end)
        .unwrap_or(source.len());
    &source[snippet_start..snippet_end]
}

struct SnippetFmt<'source> {
    snippet: &'source str,
    line: usize,
    column: usize,
}

impl<'source> SnippetFmt<'source> {
    fn new((start, end): (usize, usize), source: &'source str) -> Self {
        let snippet = expand_to_snippet(start, end, source);
        let ((line, column), (_, _)) = find_location(start, end, source);
        Self {
            snippet,
            line,
            column,
        }
    }
}

impl std::fmt::Display for SnippetFmt<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let Self {
            snippet,
            line,
            column,
        } = self;
        write!(
            f,
            "<figure><code>{snippet}</code><figcaption>at line {line} column {column}</figcaption></figure>"
        )
    }
}

struct MaybeSnippetFmt<'source> {
    snippet: Option<SnippetFmt<'source>>,
}

impl<'source> MaybeSnippetFmt<'source> {
    fn new(range: Option<(usize, usize)>, source: &'source str) -> Self {
        Self {
            snippet: range.map(|range| SnippetFmt::new(range, source)),
        }
    }
}

impl std::fmt::Display for MaybeSnippetFmt<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(snippet) = &self.snippet {
            write!(f, "{snippet}")?;
        }
        Ok(())
    }
}

#[wasm_bindgen]
pub fn espyscript_eval(source: &str) -> String {
    let ast =
        espyscript::parser::Block::new(&mut espyscript::lexer::Lexer::from(source).peekable());
    let mut parser_diagnostics = None;
    diagnostics::for_each(source, &ast, |diagnostic| {
        let f =
            parser_diagnostics.get_or_insert_with(|| "<section id=\"parse-error\">".to_string());
        let message = diagnostic.primary.message;
        let snippet = MaybeSnippetFmt::new(diagnostic.primary.range, source);
        let _ = write!(f, "<h6>{message}</h6>{snippet}");
        if !diagnostic.secondary.is_empty() {
            let _ = write!(f, "<ol>");
            for secondary in diagnostic.secondary {
                let message = secondary.message;
                let snippet = MaybeSnippetFmt::new(secondary.range, source);
                let _ = write!(f, "<li>{message}{snippet}</li>");
            }
            let _ = write!(f, "</ol>");
        }
    });

    if let Some(mut parser_diagnostics) = parser_diagnostics {
        parser_diagnostics.push_str("</section>");
        return parser_diagnostics;
    }

    match espyscript::compiler::Program::try_from(ast) {
        Ok(program) => match espyscript::interpreter::Program::try_from(Rc::from(program.compile())).expect("textual programs may not produce invalid bytecode").eval(0, Vec::new()) {
            Ok(result) => match espyscript::Function::try_from(result) {
                Ok(function) => {
                    let libs = EspygartenLibContainer::default();

                    match function
                        .piped(espyscript::Value::borrow(&libs))
                        .eval()
                    {
                        Ok(result) => {
                            let result = format!("{result:#?}");
                            let output = libs.espygarten.print.output.into_inner();

                            format!(
                                "<pre id=\"console-output\">{output}</pre><pre id=\"return-value\">{result}</pre>"
                            )
                        }
                        Err(e) => {
                            let e = format!("{e:#?}");
                            let output = libs.espygarten.print.output.into_inner();
                            format!(
                                "<pre id=\"console-output\">{output}</pre><pre id=\"eval-error\">Failed to evaluate program: {e}</pre>"
                            )
                        }
                    }
                }
                Err(espyscript::Error::ExpectedFunction(value)) => {
                    format!("<pre id=\"return-value\">{value:?}</pre>")
                }
                Err(_) => unreachable!("Function::try_from may only return ExpectedFunction"),
            },
            Err(e) => {
                format!("<pre id=\"eval-error\">Failed to evaluate program: {e:?}</pre>")
            }
        },
        Err(e) => match e {
            espyscript::compiler::Error::ProgramLimitExceeded => {
                "<p id=\"compile-error\">Program limit exceeded (bytecode must be less than 4GiB)</p>"
                    .to_string()
            }
            espyscript::compiler::Error::InvalidBreak(token) => {
                let snippet = SnippetFmt::new(origin_range(token.origin, source), source);
                format!(
                    "<p id=\"compile-error\">Attempted to break out of a scope, but no parent scope accepted unlabeled breaks.{snippet}</p>"
                )
            }
            espyscript::compiler::Error::InvalidInteger(token, e) => {
                let snippet = SnippetFmt::new(origin_range(token.origin, source), source);
                format!(
                    "<p id=\"compile-error\">Invalid integer literal: {e}.{snippet}</p>"
                )
            }
            espyscript::compiler::Error::InvalidString(token, e) => {
                let snippet = SnippetFmt::new(origin_range(token.origin, source), source);
                format!(
                    "<p id=\"compile-error\">Invalid string literal: {e:?}.{snippet}</p>"
                )
            }
            espyscript::compiler::Error::InvalidIdentifier(token, e) => {
                let snippet = SnippetFmt::new(origin_range(token.origin, source), source);
                format!(
                    "<p id=\"compile-error\">Invalid raw identifier: {e:?}.{snippet}</p>"
                )
            }
            espyscript::compiler::Error::UndefinedSymbol(token) => {
                let symbol = token.origin;
                let snippet = SnippetFmt::new(origin_range(token.origin, source), source);
                format!(
                    "<p id=\"compile-error\">Undefined symbol: {symbol}.{snippet}</p>"
                )
            }
            espyscript::compiler::Error::InvalidAst(e) => {
                format!("<p id=\"parse-error\">Failed to parse program:<br><pre>{e:#?}</pre></p>")
            }
        },
    }
}
