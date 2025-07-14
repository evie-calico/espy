use crate::*;

pub trait Extern {
    fn call<'host>(&self, _argument: Value<'host>) -> Result<Value<'host>, Error<'host>> {
        Err(ExternError::MissingFunctionImpl)?
    }

    fn debug(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{{external value}}")
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
    fn call<'host>(&self, arg: Value<'host>) -> Result<Value<'host>, Error<'host>> {
        self.0(arg)
    }
}
