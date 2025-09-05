use espy_paws::{Error, Extern, ExternError, ExternFn, Function, Type, Value};
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
    iter: IterLib,
    string: StringLib,
    option: OptionLib,
}

impl Extern for StdLib {
    fn index<'host>(&'host self, index: Value<'host>) -> Result<Value<'host>, Error<'host>> {
        let index = index.into_str()?;
        match &*index {
            "iter" => Ok(Value::borrow(&self.iter)),
            "string" => Ok(Value::borrow(&self.string)),
            "option" => Ok(Value::borrow(&self.option)),
            _ => Err(Error::IndexNotFound {
                index: index.into(),
                container: Value::borrow(self),
            }),
        }
    }

    fn debug(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::write!(f, "std module")
    }
}

#[derive(Debug, Default)]
pub struct IterLib {
    foreach: IterForeachFn,
    reduce: IterReduceFn,
    fold: IterFoldFn,
}

impl Extern for IterLib {
    fn index<'host>(&'host self, index: Value<'host>) -> Result<Value<'host>, Error<'host>> {
        let index = index.into_str()?;
        match &*index {
            "foreach" => Ok(Function::borrow(&self.foreach).into()),
            "reduce" => Ok(Function::borrow(&self.reduce).into()),
            "fold" => Ok(Function::borrow(&self.fold).into()),
            _ => Err(Error::IndexNotFound {
                index: index.into(),
                container: Value::borrow(self),
            }),
        }
    }

    fn debug(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::write!(f, "std.iter module")
    }
}

#[derive(Debug, Default)]
pub struct IterForeachFn;

impl ExternFn for IterForeachFn {
    fn call<'host>(&'host self, argument: Value<'host>) -> Result<Value<'host>, Error<'host>> {
        let mut iterator = argument.get(0)?;
        let next = argument.get(1)?.into_function()?;
        let foreach = argument.get(2)?.into_function()?;
        while let Some(result) = next.clone().piped(iterator).eval()?.into_option()? {
            iterator = result.get(0)?;
            foreach.clone().piped(result.get(1)?).eval()?;
        }
        Ok(().into())
    }

    fn debug(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::write!(f, "std.iter.foreach function")
    }
}

#[derive(Debug, Default)]
pub struct IterFoldFn;

impl ExternFn for IterFoldFn {
    fn call<'host>(&'host self, argument: Value<'host>) -> Result<Value<'host>, Error<'host>> {
        let mut iterator = argument.get(0)?;
        let next = argument.get(1)?.into_function()?;
        let mut accumulator = argument.get(2)?;
        let fold = argument.get(3)?.into_function()?;
        while let Some(result) = next.clone().piped(iterator).eval()?.into_option()? {
            iterator = result.get(0)?;
            accumulator = fold
                .clone()
                .piped(accumulator)
                .piped(result.get(1)?)
                .eval()?;
        }
        Ok(accumulator)
    }

    fn debug(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::write!(f, "std.iter.fold function")
    }
}

#[derive(Debug, Default)]
pub struct IterReduceFn;

impl ExternFn for IterReduceFn {
    fn call<'host>(&'host self, argument: Value<'host>) -> Result<Value<'host>, Error<'host>> {
        let mut iterator = argument.get(0)?;
        let next = argument.get(1)?.into_function()?;
        let mut accumulator: Option<Value<'host>> = None;
        let fold = argument.get(2)?.into_function()?;
        while let Some(result) = next.clone().piped(iterator).eval()?.into_option()? {
            iterator = result.get(0)?;
            accumulator = Some(if let Some(accumulator) = accumulator.take() {
                fold.clone()
                    .piped(accumulator)
                    .piped(result.get(1)?)
                    .eval()?
            } else {
                result.get(1)?
            });
        }
        Ok(Value::Option {
            contents: accumulator.map(Rc::new),
            ty: Rc::new(Type::Any.into()),
        })
    }

    fn debug(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::write!(f, "std.iter.reduce function")
    }
}

#[derive(Debug, Default)]
pub struct StringLib {
    concat: StringConcatFn,
}

impl Extern for StringLib {
    fn index<'host>(&'host self, index: Value<'host>) -> Result<Value<'host>, Error<'host>> {
        let index = index.into_str()?;
        match &*index {
            "concat" => Ok(Function::borrow(&self.concat).into()),
            _ => Err(Error::IndexNotFound {
                index: index.into(),
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
            .map(|s| Value::String(s.into()))
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
        let index = index.into_str()?;
        match &*index {
            "unwrap" => Ok(Function::borrow(&self.unwrap).into()),
            "expect" => Ok(Function::borrow(&self.expect).into()),
            _ => Err(Error::IndexNotFound {
                index: index.into(),
                container: Value::borrow(self),
            }),
        }
    }

    fn debug(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::write!(f, "std.option module")
    }
}

#[derive(Debug, Default)]
pub struct OptionUnwrapFn;

impl ExternFn for OptionUnwrapFn {
    fn call<'host>(&'host self, argument: Value<'host>) -> Result<Value<'host>, Error<'host>> {
        let Some(contents) = argument.get(0)?.into_option()? else {
            Err(ExternError::Other(Box::new(LibraryError::UnwrapFailed)))?
        };
        Ok(contents)
    }

    fn debug(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::write!(f, "std.option.unwrap function")
    }
}

#[derive(Debug, Default)]
pub struct OptionExpectFn;

impl ExternFn for OptionExpectFn {
    fn call<'host>(&'host self, argument: Value<'host>) -> Result<Value<'host>, Error<'host>> {
        let message = argument.get(1)?.into_str()?;
        let Some(contents) = argument.get(0)?.into_option()? else {
            Err(ExternError::Other(Box::new(LibraryError::ExpectFailed(
                message,
            ))))?
        };
        Ok(contents)
    }

    fn debug(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::write!(f, "std.option.expect function")
    }
}
