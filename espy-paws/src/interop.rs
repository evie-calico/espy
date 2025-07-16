use crate::*;
use std::cell::RefCell;

pub trait Extern {
    fn call<'host>(&'host self, _argument: Value<'host>) -> Result<Value<'host>, Error<'host>> {
        Err(ExternError::MissingFunctionImpl)?
    }

    fn debug(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{{external value}}")
    }
}

pub trait ExternMut {
    fn call<'host>(&mut self, _argument: Value<'host>) -> Result<Value<'host>, Error<'host>> {
        Err(ExternError::MissingFunctionImpl)?
    }

    fn debug(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{{external value}}")
    }
}

pub struct ExternCell<T: ExternMut>(RefCell<T>);

impl<T: ExternMut> From<T> for ExternCell<T> {
    fn from(value: T) -> Self {
        Self(RefCell::new(value))
    }
}

impl<T: ExternMut> Extern for ExternCell<T> {
    fn call<'host>(&'host self, argument: Value<'host>) -> Result<Value<'host>, Error<'host>> {
        self.0
            .try_borrow_mut()
            .map_err(|_| ExternError::BorrowMutError)?
            .call(argument)
    }
}

pub struct Function<F: for<'host> Fn(Value<'host>) -> Result<Value<'host>, Error<'host>>>(F);

pub fn function<F: for<'host> Fn(Value<'host>) -> Result<Value<'host>, Error<'host>>>(
    f: F,
) -> Function<F> {
    Function::from(f)
}

impl<F> From<F> for Function<F>
where
    F: for<'host> Fn(Value<'host>) -> Result<Value<'host>, Error<'host>>,
{
    fn from(f: F) -> Self {
        Self(f)
    }
}

impl<F> Extern for Function<F>
where
    F: for<'host> Fn(Value<'host>) -> Result<Value<'host>, Error<'host>>,
{
    fn call<'host>(&self, argument: Value<'host>) -> Result<Value<'host>, Error<'host>> {
        self.0(argument)
    }
}

pub struct FunctionMut<F: for<'host> FnMut(Value<'host>) -> Result<Value<'host>, Error<'host>>>(
    RefCell<F>,
);

pub fn function_mut<F: for<'host> FnMut(Value<'host>) -> Result<Value<'host>, Error<'host>>>(
    f: F,
) -> FunctionMut<F> {
    FunctionMut::from(f)
}

impl<F> From<F> for FunctionMut<F>
where
    F: for<'host> FnMut(Value<'host>) -> Result<Value<'host>, Error<'host>>,
{
    fn from(f: F) -> Self {
        Self(RefCell::new(f))
    }
}

impl<F> Extern for FunctionMut<F>
where
    F: for<'host> FnMut(Value<'host>) -> Result<Value<'host>, Error<'host>>,
{
    fn call<'host>(&self, argument: Value<'host>) -> Result<Value<'host>, Error<'host>> {
        self.0
            .try_borrow_mut()
            .map_err(|_| ExternError::BorrowMutError)?(argument)
    }
}
