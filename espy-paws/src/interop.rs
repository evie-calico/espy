use crate::*;
use std::cell::RefCell;

pub trait Extern {
    fn index<'host>(&'host self, _index: Value<'host>) -> Result<Value<'host>, Error<'host>> {
        Err(ExternError::MissingIndexImpl)?
    }

    fn debug(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{{external value}}")
    }
}

pub trait ExternFn {
    fn call<'host>(&'host self, _argument: Value<'host>) -> Result<Value<'host>, Error<'host>> {
        Err(ExternError::MissingFunctionImpl)?
    }

    fn debug(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{{external function}}")
    }
}

pub struct FunctionWrapper<F: for<'host> Fn(Value<'host>) -> Result<Value<'host>, Error<'host>>>(F);

pub fn wrap_fn<F: for<'host> Fn(Value<'host>) -> Result<Value<'host>, Error<'host>>>(
    f: F,
) -> FunctionWrapper<F> {
    FunctionWrapper::from(f)
}

impl<F> From<F> for FunctionWrapper<F>
where
    F: for<'host> Fn(Value<'host>) -> Result<Value<'host>, Error<'host>>,
{
    fn from(f: F) -> Self {
        Self(f)
    }
}

impl<F> ExternFn for FunctionWrapper<F>
where
    F: for<'host> Fn(Value<'host>) -> Result<Value<'host>, Error<'host>>,
{
    fn call<'host>(&self, argument: Value<'host>) -> Result<Value<'host>, Error<'host>> {
        self.0(argument)
    }
}

pub struct FunctionWrapperMut<
    F: for<'host> FnMut(Value<'host>) -> Result<Value<'host>, Error<'host>>,
>(RefCell<F>);

pub fn wrap_fn_mut<F: for<'host> FnMut(Value<'host>) -> Result<Value<'host>, Error<'host>>>(
    f: F,
) -> FunctionWrapperMut<F> {
    FunctionWrapperMut::from(f)
}

impl<F> From<F> for FunctionWrapperMut<F>
where
    F: for<'host> FnMut(Value<'host>) -> Result<Value<'host>, Error<'host>>,
{
    fn from(f: F) -> Self {
        Self(RefCell::new(f))
    }
}

impl<F> ExternFn for FunctionWrapperMut<F>
where
    F: for<'host> FnMut(Value<'host>) -> Result<Value<'host>, Error<'host>>,
{
    fn call<'host>(&self, argument: Value<'host>) -> Result<Value<'host>, Error<'host>> {
        self.0.try_borrow_mut()?(argument)
    }
}
