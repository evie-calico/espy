use espy_paws::{
    ComplexType, Error, Extern, ExternError, ExternFn, Function, Storage, Tuple, Type, Value,
};
use std::rc::Rc;

#[derive(Debug)]
pub enum LibraryError {
    UnwrapFailed,
    ExpectFailed(Rc<str>),
}

impl std::fmt::Display for LibraryError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LibraryError::UnwrapFailed => write!(f, "attempted to unwrap None value"),
            LibraryError::ExpectFailed(msg) => write!(f, "{msg}"),
        }
    }
}

impl std::error::Error for LibraryError {}

#[derive(Debug, Default)]
pub struct StdLib {
    string: StringLib,
    option: OptionLib,
}

impl Extern for StdLib {
    fn index<'host>(&'host self, index: Value<'host>) -> Result<Value<'host>, Error<'host>> {
        match index {
            Value {
                storage: Storage::String(index),
            } if &*index == "string" => Ok(Value::borrow(&self.string)),
            Value {
                storage: Storage::String(index),
            } if &*index == "option" => Ok(Value::borrow(&self.option)),
            index => Err(Error::IndexNotFound {
                index,
                container: Value::borrow(self),
            }),
        }
    }

    fn debug(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::write!(f, "std module")
    }
}

#[derive(Debug, Default)]
pub struct StringLib {
    concat: StringConcatFn,
}

impl Extern for StringLib {
    fn index<'host>(&'host self, index: Value<'host>) -> Result<Value<'host>, Error<'host>> {
        match index {
            Value {
                storage: Storage::String(index),
            } if &*index == "concat" => Ok(Function::borrow(&self.concat).into()),
            index => Err(Error::IndexNotFound {
                index,
                container: Value::borrow(self),
            }),
        }
    }

    fn debug(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::write!(f, "std.string module")
    }
}

#[derive(Debug, Default)]
pub struct StringConcatFn;

impl ExternFn for StringConcatFn {
    fn call<'host>(&'host self, argument: Value<'host>) -> Result<Value<'host>, Error<'host>> {
        argument
            .into_tuple()?
            .values()
            .map(|s| {
                s.as_str()
                    .ok_or_else(|| Error::type_error(s.clone(), Type::String))
            })
            .collect::<Result<String, _>>()
            .map(|s| Value::from(Rc::<str>::from(s.as_str())))
    }

    fn debug(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::write!(f, "std.string.concat function")
    }
}

#[derive(Debug, Default)]
pub struct OptionLib {
    unwrap: OptionUnwrapFn,
    expect: OptionExpectFn,
}

impl Extern for OptionLib {
    fn index<'host>(&'host self, index: Value<'host>) -> Result<Value<'host>, Error<'host>> {
        match index {
            Value {
                storage: Storage::String(index),
            } if &*index == "unwrap" => Ok(Function::borrow(&self.unwrap).into()),
            Value {
                storage: Storage::String(index),
            } if &*index == "expect" => Ok(Function::borrow(&self.expect).into()),
            index => Err(Error::IndexNotFound {
                index,
                container: Value::borrow(self),
            }),
        }
    }

    fn debug(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::write!(f, "std.string module")
    }
}

#[derive(Debug, Default)]
pub struct OptionUnwrapFn;

impl ExternFn for OptionUnwrapFn {
    fn call<'host>(&'host self, argument: Value<'host>) -> Result<Value<'host>, Error<'host>> {
        // TODO: Owned external values would allow for typechecking within the option library.
        let Storage::Option { contents, ty } = argument.storage else {
            return Err(Error::type_error(
                argument,
                Type::Option(Rc::new(Type::Any.into())),
            ));
        };
        let Some(contents) = contents else {
            Err(ExternError::Other(Box::new(LibraryError::UnwrapFailed)))?
        };
        Ok(Rc::unwrap_or_clone(contents))
    }

    fn debug(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::write!(f, "std.option.unwrap function")
    }
}

#[derive(Debug, Default)]
pub struct OptionExpectFn;

impl ExternFn for OptionExpectFn {
    fn call<'host>(&'host self, argument: Value<'host>) -> Result<Value<'host>, Error<'host>> {
        let arguments = argument.into_tuple()?;
        let Some((
            Value {
                // TODO: Owned external values would allow for typechecking within the option library.
                storage: Storage::Option { contents, ty },
            },
            Value {
                storage: Storage::String(msg),
            },
        )) = arguments.value(0).zip(arguments.value(1))
        else {
            return Err(Error::type_error(
                Storage::Tuple(arguments).into(),
                Tuple::from([
                    ComplexType::Simple(Type::Option(Rc::new(Type::Any.into()))),
                    ComplexType::Simple(Type::String),
                ]),
            ));
        };
        let Some(contents) = contents else {
            Err(ExternError::Other(Box::new(LibraryError::ExpectFailed(
                msg.clone(),
            ))))?
        };
        Ok((**contents).clone())
    }

    fn debug(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::write!(f, "std.option.expect function")
    }
}
