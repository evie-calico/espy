use std::cell::RefCell;

use wasm_bindgen::prelude::*;

#[derive(Debug, Default)]
struct Std {
    io: Io,
}

impl espyscript::Extern for Std {
    fn index<'host>(
        &'host self,
        index: espyscript::Value<'host>,
    ) -> Result<espyscript::Value<'host>, espyscript::interpreter::Error<'host>> {
        match index {
            espyscript::Value {
                storage: espyscript::Storage::String(index),
            } if &*index == "io" => Ok(espyscript::Storage::Borrow(&self.io).into()),
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
struct Io {
    print: Print,
}

impl espyscript::Extern for Io {
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
struct Print {
    output: RefCell<String>,
}

impl espyscript::Extern for Print {
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

#[wasm_bindgen]
pub fn espyscript_eval(src: &str) -> String {
    match espyscript::Program::try_from(src) {
        Ok(program) => match program.eval() {
            Ok(result) => match espyscript::Function::try_from(result) {
                Ok(function) => {
                    let std = Std::default();

                    match function
                        .piped(espyscript::Storage::Borrow(&std).into())
                        .eval()
                    {
                        Ok(result) => {
                            let result = format!("{result:#?}");
                            let output = std.io.print.output.into_inner();

                            format!(
                                "<pre id=\"console-output\">{output}</pre><pre id=\"return-value\">{result}</p>"
                            )
                        }
                        Err(e) => {
                            format!("<p id=\"eval-error\">Failed to evaluate program: {e:?}</p>")
                        }
                    }
                }
                Err(espyscript::Error::ExpectedFunction(value)) => {
                    format!("<pre id=\"return-value\">{value:?}</pre>")
                }
                Err(_) => unreachable!(),
            },
            Err(e) => {
                format!("<p id=\"eval-error\">Failed to evaluate program: {e:?}</p>")
            }
        },
        Err(e) => {
            format!("<p id=\"parse-error\">Failed to parse program: {e:?}</p>")
        }
    }
}
