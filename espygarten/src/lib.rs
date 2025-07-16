use std::{cell::RefCell, sync::RwLock};

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

static LAST_OUTPUT: RwLock<Option<String>> = RwLock::new(None);
static LAST_RESULT: RwLock<Option<String>> = RwLock::new(None);

#[wasm_bindgen]
pub fn espyscript_eval(src: &str) {
    let std = Std::default();

    let result =
        espyscript::Function::try_from(espyscript::Program::try_from(src).unwrap().eval().unwrap())
            .unwrap()
            .piped(espyscript::Storage::Borrow(&std).into())
            .eval()
            .unwrap();

    *LAST_RESULT.write().unwrap() = Some(format!("{result:#?}"));
    *LAST_OUTPUT.write().unwrap() = Some(std.io.print.output.into_inner());
}

#[wasm_bindgen]
pub fn espyscript_last_output() -> Option<String> {
    LAST_OUTPUT.read().unwrap().clone()
}

#[wasm_bindgen]
pub fn espyscript_last_result() -> Option<String> {
    LAST_RESULT.read().unwrap().clone()
}
