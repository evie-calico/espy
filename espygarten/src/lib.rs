use std::cell::RefCell;

use wasm_bindgen::prelude::*;

#[derive(Debug, Default)]
struct StdLib {
    io: IoLib,
    string: StringLib,
}

impl espyscript::Extern for StdLib {
    fn index<'host>(
        &'host self,
        index: espyscript::Value<'host>,
    ) -> Result<espyscript::Value<'host>, espyscript::interpreter::Error<'host>> {
        match index {
            espyscript::Value {
                storage: espyscript::Storage::String(index),
            } if &*index == "io" => Ok(espyscript::Storage::Borrow(&self.io).into()),
            espyscript::Value {
                storage: espyscript::Storage::String(index),
            } if &*index == "string" => Ok(espyscript::Storage::Borrow(&self.string).into()),
            index => Err(espyscript::Error::IndexNotFound {
                index,
                container: espyscript::Storage::Borrow(self).into(),
            }),
        }
    }

    fn debug(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::write!(f, "std module")
    }
}

#[derive(Debug, Default)]
struct IoLib {
    print: IoPrintFn,
}

impl espyscript::Extern for IoLib {
    fn index<'host>(
        &'host self,
        index: espyscript::Value<'host>,
    ) -> Result<espyscript::Value<'host>, espyscript::interpreter::Error<'host>> {
        match index {
            espyscript::Value {
                storage: espyscript::Storage::String(index),
            } if &*index == "print" => Ok(espyscript::Storage::Borrow(&self.print).into()),
            index => Err(espyscript::Error::IndexNotFound {
                index,
                container: espyscript::Storage::Borrow(self).into(),
            }),
        }
    }

    fn debug(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::write!(f, "std.io module")
    }
}

#[derive(Debug, Default)]
struct IoPrintFn {
    output: RefCell<String>,
}

impl espyscript::Extern for IoPrintFn {
    fn call<'host>(
        &'host self,
        argument: espyscript::Value<'host>,
    ) -> Result<espyscript::Value<'host>, espyscript::Error<'host>> {
        match argument {
            espyscript::Value {
                storage: espyscript::Storage::String(message),
            } => {
                let mut output = self.output.borrow_mut();
                output.push_str(&message);
                output.push('\n');
                Ok(espyscript::Storage::Unit.into())
            }
            argument => Err(espyscript::Error::TypeError {
                value: argument,
                ty: espyscript::Storage::StringType.into(),
            }),
        }
    }

    fn debug(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::write!(f, "std.io.print function")
    }
}

#[derive(Debug, Default)]
struct StringLib {
    concat: StringConcatFn,
}

impl espyscript::Extern for StringLib {
    fn index<'host>(
        &'host self,
        index: espyscript::Value<'host>,
    ) -> Result<espyscript::Value<'host>, espyscript::interpreter::Error<'host>> {
        match index {
            espyscript::Value {
                storage: espyscript::Storage::String(index),
            } if &*index == "concat" => Ok(espyscript::Storage::Borrow(&self.concat).into()),
            index => Err(espyscript::Error::IndexNotFound {
                index,
                container: espyscript::Storage::Borrow(self).into(),
            }),
        }
    }

    fn debug(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::write!(f, "std.string module")
    }
}

#[derive(Debug, Default)]
struct StringConcatFn;

impl espyscript::Extern for StringConcatFn {
    fn call<'host>(
        &'host self,
        argument: espyscript::Value<'host>,
    ) -> Result<espyscript::Value<'host>, espyscript::Error<'host>> {
        argument
            .into_tuple()?
            .values()
            .map(|value| match value {
                espyscript::Value {
                    storage: espyscript::Storage::String(s),
                } => Ok(s as &str),
                value => Err(espyscript::Error::TypeError {
                    value: value.clone(),
                    ty: espyscript::Storage::StringType.into(),
                }),
            })
            .collect::<Result<String, _>>()
            .map(|s| espyscript::Storage::String(s.into()).into())
    }

    fn debug(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::write!(f, "std.string.concat function")
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
        .map(|(at, _)| at + 1)
        .unwrap_or(0);
    let snippet_end = source[end..]
        .char_indices()
        .find(|(_, c)| *c == '\n')
        .map(|(at, _)| at - 1)
        .unwrap_or(source.len());
    &source[snippet_start..snippet_end]
}

#[wasm_bindgen]
pub fn espyscript_eval(source: &str) -> String {
    match espyscript::Program::try_from(source) {
        Ok(program) => match program.eval() {
            Ok(result) => match espyscript::Function::try_from(result) {
                Ok(function) => {
                    let std = StdLib::default();

                    match function
                        .piped(espyscript::Storage::Borrow(&std).into())
                        .eval()
                    {
                        Ok(result) => {
                            let result = format!("{result:#?}");
                            let output = std.io.print.output.into_inner();

                            format!(
                                "<pre id=\"console-output\">{output}</pre><pre id=\"return-value\">{result}</pre>"
                            )
                        }
                        Err(e) => {
                            let e = format!("{e:#?}");
                            let output = std.io.print.output.into_inner();
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
                let (start, end) = token.origin_range(source);
                let snippet = expand_to_snippet(start, end, source);
                let ((line, column), (_, _)) = find_location(start, end, source);
                format!(
                    "<p id=\"compile-error\">Attempted to break out of a scope, but no parent scope accepted unlabeled breaks.<figure><code>{snippet}</code><figcaption>at line {line} column {column}</figcaption></figure></p>"
                )
            }
            espyscript::compiler::Error::InvalidInteger(token, e) => {
                let (start, end) = token.origin_range(source);
                let snippet = expand_to_snippet(start, end, source);
                let ((line, column), (_, _)) = find_location(start, end, source);
                format!(
                    "<p id=\"compile-error\">Invalid integer literal: {e}.<figure><code>{snippet}</code><figcaption>at line {line} column {column}</figcaption></figure></p>"
                )
            }
            espyscript::compiler::Error::UndefinedSymbol(token) => {
                let symbol = token.origin;
                let (start, end) = token.origin_range(source);
                let snippet = expand_to_snippet(start, end, source);
                let ((line, column), (_, _)) = find_location(start, end, source);
                format!(
                    "<p id=\"compile-error\">Undefined symbol \"{symbol}\".<figure><code>{snippet}</code><figcaption>at line {line} column {column}</figcaption></figure></p>"
                )
            }
            espyscript::compiler::Error::UnexpectedEnumResult => todo!(),
            espyscript::compiler::Error::InvalidAst(e) => {
                format!("<p id=\"parse-error\">Failed to parse program:<br><pre>{e:#?}</pre></p>")
            }
        },
    }
}
