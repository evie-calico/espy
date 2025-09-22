use espy_paws::{Error, Extern, ExternFn, ExternFnOwned, Function, Tuple, Type, Value};
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
pub struct StdLib;

impl Extern for StdLib {
    fn index<'host>(&'host self, index: Value<'host>) -> Result<Value<'host>, Error<'host>> {
        let index = index.into_str()?;
        match &*index {
            "iter" => Ok(Value::borrow(&IterLib)),
            "string" => Ok(Value::borrow(&StringLib)),
            "option" => Ok(Value::borrow(&OptionLib)),
            "typeof" => Ok(Function::borrow(&TypeofFn).into()),
            _ => Err(Error::IndexNotFound {
                index: index.into(),
                container: Value::borrow(self),
            }),
        }
    }

    fn as_static(&self) -> Option<Value<'static>> {
        Some(espy_paws::Value::borrow(&StdLib))
    }

    fn debug(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::write!(f, "std module")
    }
}

#[derive(Debug, Default)]
pub struct TypeofFn;

impl ExternFn for TypeofFn {
    fn call<'host>(&'host self, argument: Value<'host>) -> Result<Value<'host>, Error<'host>> {
        Ok(argument.type_of()?.into())
    }

    fn as_static(&self) -> Option<Function<'static>> {
        Some(espy_paws::Function::borrow(&TypeofFn))
    }

    fn debug(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::write!(f, "std.typeof function")
    }
}

#[derive(Debug, Default)]
pub struct IterLib;

impl Extern for IterLib {
    fn index<'host>(&'host self, index: Value<'host>) -> Result<Value<'host>, Error<'host>> {
        let index = index.into_str()?;
        match &*index {
            "filter" => Ok(Function::borrow(&IterFilterFn).into()),
            "fold" => Ok(Function::borrow(&IterFoldFn).into()),
            "foreach" => Ok(Function::borrow(&IterForeachFn).into()),
            "map" => Ok(Function::borrow(&IterMapFn).into()),
            "range" => Ok(Function::borrow(&IterRangeFn).into()),
            "reduce" => Ok(Function::borrow(&IterReduceFn).into()),
            "reduce_once" => Ok(Function::borrow(&IterReduceOnceFn).into()),
            "repeat" => Ok(Function::borrow(&IterReduceFn).into()),
            "skip" => Ok(Function::borrow(&IterSkipFn).into()),
            "take" => Ok(Function::borrow(&IterTakeFn).into()),
            _ => Err(Error::IndexNotFound {
                index: index.into(),
                container: Value::borrow(self),
            }),
        }
    }

    fn as_static(&self) -> Option<Value<'static>> {
        Some(espy_paws::Value::borrow(&IterLib))
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

    fn as_static(&self) -> Option<Function<'static>> {
        Some(espy_paws::Function::borrow(&IterForeachFn))
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

    fn as_static(&self) -> Option<Function<'static>> {
        Some(espy_paws::Function::borrow(&IterFoldFn))
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
        Ok(accumulator.into())
    }

    fn as_static(&self) -> Option<Function<'static>> {
        Some(espy_paws::Function::borrow(&IterReduceFn))
    }

    fn debug(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::write!(f, "std.iter.reduce function")
    }
}

#[derive(Debug, Default)]
pub struct IterReduceOnceFn;

impl ExternFn for IterReduceOnceFn {
    fn call<'host>(&'host self, argument: Value<'host>) -> Result<Value<'host>, Error<'host>> {
        let iterator = argument.get(0)?;
        let next = argument.get(1)?.into_function()?;
        Ok(next
            .piped(iterator)
            .eval()?
            .into_option()?
            .map(|x| x.get(1))
            .transpose()?
            .into())
    }

    fn as_static(&self) -> Option<Function<'static>> {
        Some(espy_paws::Function::borrow(&IterReduceOnceFn))
    }

    fn debug(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::write!(f, "std.iter.reduce_once function")
    }
}

#[derive(Debug, Default)]
pub struct IterRangeFn;

impl ExternFn for IterRangeFn {
    fn call<'host>(&'host self, argument: Value<'host>) -> Result<Value<'host>, Error<'host>> {
        let current = argument.get(0)?;
        let limit = argument.get(1)?.into_i64()?;

        Ok(Value::Tuple(Tuple::from([
            current,
            Function::owned(Rc::new(RangeIter { limit })).into(),
        ])))
    }

    fn as_static(&self) -> Option<Function<'static>> {
        Some(espy_paws::Function::borrow(&IterRangeFn))
    }

    fn debug(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::write!(f, "std.iter.range function")
    }
}

#[derive(Debug, Default)]
pub struct RangeIter {
    limit: i64,
}

impl ExternFnOwned for RangeIter {
    fn call<'host>(self: Rc<Self>, argument: Value<'host>) -> Result<Value<'host>, Error<'host>> {
        let current = argument.into_i64()?;
        Ok(Value::from(if current < self.limit {
            Some(Value::Tuple(Tuple::from([
                Value::from(current + 1),
                current.into(),
            ])))
        } else {
            None
        }))
    }

    fn debug(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::write!(f, "std.iter.range iterator")
    }
}

#[derive(Debug, Default)]
pub struct IterRepeatFn;

impl ExternFn for IterRepeatFn {
    fn call<'host>(&'host self, value: Value<'host>) -> Result<Value<'host>, Error<'host>> {
        Ok(Value::Tuple(Tuple::from([
            value,
            Function::borrow(&RepeatIter).into(),
        ])))
    }

    fn as_static(&self) -> Option<Function<'static>> {
        Some(espy_paws::Function::borrow(&IterRepeatFn))
    }

    fn debug(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::write!(f, "std.iter.repeat function")
    }
}

#[derive(Debug, Default)]
pub struct RepeatIter;

impl ExternFn for RepeatIter {
    fn call<'host>(&self, argument: Value<'host>) -> Result<Value<'host>, Error<'host>> {
        Ok(Value::from(Some(Value::Tuple(Tuple::from([
            argument.clone(),
            argument,
        ])))))
    }

    fn as_static(&self) -> Option<Function<'static>> {
        Some(espy_paws::Function::borrow(&RepeatIter))
    }

    fn debug(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::write!(f, "std.iter.repeat iterator")
    }
}

#[derive(Debug, Default)]
pub struct IterSkipFn;

impl ExternFn for IterSkipFn {
    fn call<'host>(&'host self, argument: Value<'host>) -> Result<Value<'host>, Error<'host>> {
        let mut iterator = argument.get(0)?;
        let next = argument.get(1)?.into_function()?;
        let limit = argument.get(2)?.into_i64()?;
        for _ in 0..limit {
            if let Some(result) = next.clone().piped(iterator.clone()).eval()?.into_option()? {
                iterator = result.get(0)?;
            } else {
                break;
            }
        }

        Ok(Value::Tuple([iterator, next.into()].into()))
    }

    fn as_static(&self) -> Option<Function<'static>> {
        Some(espy_paws::Function::borrow(&IterSkipFn))
    }

    fn debug(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::write!(f, "std.iter.skip function")
    }
}

#[derive(Debug, Default)]
pub struct IterTakeFn;

impl ExternFn for IterTakeFn {
    fn call<'host>(&'host self, argument: Value<'host>) -> Result<Value<'host>, Error<'host>> {
        let iterator = argument.get(0)?;
        let next = argument.get(1)?;
        let count = argument.get(2)?;

        Ok(Value::Tuple(Tuple::from([
            Value::Tuple(Tuple::from([iterator, count])),
            Function::borrow(&TakeIter).piped(next).into(),
        ])))
    }

    fn as_static(&self) -> Option<Function<'static>> {
        Some(espy_paws::Function::borrow(&IterTakeFn))
    }

    fn debug(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::write!(f, "std.iter.take function")
    }
}

#[derive(Debug, Default)]
pub struct TakeIter;

impl ExternFn for TakeIter {
    fn call<'host>(&self, argument: Value<'host>) -> Result<Value<'host>, Error<'host>> {
        let next = argument.get(0)?.into_function()?;
        let iterator = argument.get(1)?;
        let count = argument.get(2)?.into_i64()?;

        Ok(Value::from(
            if count > 0
                && let Some(result) = next.piped(iterator).eval()?.into_option()?
            {
                let iterator = result.get(0)?;
                let item = result.get(1)?;
                Some(Rc::new(Value::Tuple(Tuple::from([
                    Value::Tuple(Tuple::from([iterator, Value::from(count - 1)])),
                    item,
                ]))))
            } else {
                None
            },
        ))
    }

    fn as_static(&self) -> Option<Function<'static>> {
        Some(espy_paws::Function::borrow(&TakeIter))
    }

    fn debug(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::write!(f, "std.iter.take iterator")
    }
}

#[derive(Debug, Default)]
pub struct IterMapFn;

impl ExternFn for IterMapFn {
    fn call<'host>(&'host self, argument: Value<'host>) -> Result<Value<'host>, Error<'host>> {
        let iterator = argument.get(0)?;
        let next = argument.get(1)?;
        let map = argument.get(2)?;

        Ok(Value::Tuple(Tuple::from([
            iterator,
            Function::borrow(&MapIter).piped(next).piped(map).into(),
        ])))
    }

    fn as_static(&self) -> Option<Function<'static>> {
        Some(espy_paws::Function::borrow(&IterMapFn))
    }

    fn debug(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::write!(f, "std.iter.map function")
    }
}

#[derive(Debug, Default)]
pub struct MapIter;

impl ExternFn for MapIter {
    fn call<'host>(&'host self, argument: Value<'host>) -> Result<Value<'host>, Error<'host>> {
        let next = argument.get(0)?.into_function()?;
        let map = argument.get(1)?.into_function()?;
        let iterator = argument.get(2)?;
        Ok(next
            .piped(iterator)
            .eval()?
            .into_option()?
            .map(|x| {
                let iterator = x.get(0)?;
                let item = x.get(1)?;
                Ok::<_, Error>(Value::Tuple(Tuple::from([
                    iterator,
                    map.clone().piped(item).eval()?,
                ])))
            })
            .transpose()?
            .into())
    }

    fn as_static(&self) -> Option<Function<'static>> {
        Some(espy_paws::Function::borrow(&MapIter))
    }

    fn debug(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::write!(f, "std.iter.map iterator")
    }
}

#[derive(Debug, Default)]
pub struct IterFilterFn;

impl ExternFn for IterFilterFn {
    fn call<'host>(&'host self, argument: Value<'host>) -> Result<Value<'host>, Error<'host>> {
        let iterator = argument.get(0)?;
        let next = argument.get(1)?;
        let map = argument.get(2)?;

        Ok(Value::Tuple(Tuple::from([
            iterator,
            Function::borrow(&FilterIter).piped(next).piped(map).into(),
        ])))
    }

    fn as_static(&self) -> Option<Function<'static>> {
        Some(espy_paws::Function::borrow(&IterFilterFn))
    }

    fn debug(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::write!(f, "std.iter.filter function")
    }
}

#[derive(Debug, Default)]
pub struct FilterIter;

impl ExternFn for FilterIter {
    fn call<'host>(&'host self, argument: Value<'host>) -> Result<Value<'host>, Error<'host>> {
        let next = argument.get(0)?.into_function()?;
        let filter = argument.get(1)?.into_function()?;
        let mut iterator = argument.get(2)?;
        while let Some(result) = next.clone().piped(iterator).eval()?.into_option()? {
            iterator = result.get(0)?;
            let item = result.get(1)?;
            if filter.clone().piped(item.clone()).eval()?.into_bool()? {
                return Ok(Some(item).into());
            }
        }
        Ok(None::<Value>.into())
    }

    fn as_static(&self) -> Option<Function<'static>> {
        Some(espy_paws::Function::borrow(&FilterIter))
    }

    fn debug(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::write!(f, "std.iter.filter iterator")
    }
}

#[derive(Debug, Default)]
pub struct StringLib;

impl Extern for StringLib {
    fn index<'host>(&'host self, index: Value<'host>) -> Result<Value<'host>, Error<'host>> {
        let index = index.into_str()?;
        match &*index {
            "concat" => Ok(Function::borrow(&StringConcatFn).into()),
            "from_i64" => Ok(Function::borrow(&StringFromI64Fn).into()),
            "parse_i64" => Ok(Function::borrow(&StringParseI64Fn).into()),
            "split" => Ok(Function::borrow(&StringSplitFn).into()),
            "split_whitespace" => Ok(Function::borrow(&StringSplitWhitespaceFn).into()),
            "trim_whitespace" => Ok(Function::borrow(&StringTrimWhitespaceFn).into()),
            _ => Err(Error::IndexNotFound {
                index: index.into(),
                container: Value::borrow(self),
            }),
        }
    }

    fn as_static(&self) -> Option<Value<'static>> {
        Some(espy_paws::Value::borrow(&StringLib))
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

    fn as_static(&self) -> Option<Function<'static>> {
        Some(espy_paws::Function::borrow(&StringConcatFn))
    }

    fn debug(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::write!(f, "std.string.concat function")
    }
}

#[derive(Debug, Default)]
pub struct StringFromI64Fn;

impl ExternFn for StringFromI64Fn {
    fn call<'host>(&'host self, argument: Value<'host>) -> Result<Value<'host>, Error<'host>> {
        Ok(Value::String(
            argument.into_i64()?.to_string().as_str().into(),
        ))
    }

    fn as_static(&self) -> Option<Function<'static>> {
        Some(espy_paws::Function::borrow(&StringFromI64Fn))
    }

    fn debug(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::write!(f, "std.string.from_i64 function")
    }
}

#[derive(Debug, Default)]
pub struct StringParseI64Fn;

impl ExternFn for StringParseI64Fn {
    fn call<'host>(&'host self, argument: Value<'host>) -> Result<Value<'host>, Error<'host>> {
        Ok(argument
            .into_str()?
            .parse::<i64>()
            .ok()
            .map(Value::I64)
            .into())
    }

    fn as_static(&self) -> Option<Function<'static>> {
        Some(espy_paws::Function::borrow(&StringParseI64Fn))
    }

    fn debug(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::write!(f, "std.string.parse_i64 function")
    }
}

#[derive(Debug, Default)]
pub struct StringSplitFn;

impl ExternFn for StringSplitFn {
    fn call<'host>(&'host self, argument: Value<'host>) -> Result<Value<'host>, Error<'host>> {
        let string = argument.get(0)?.into_str()?;
        let pattern = argument.get(1)?.into_str()?;

        Ok(Value::Tuple(
            [
                Value::from(0),
                Function::owned(Rc::new(SplitIter { pattern, string })).into(),
            ]
            .into(),
        ))
    }

    fn as_static(&self) -> Option<Function<'static>> {
        Some(espy_paws::Function::borrow(&StringSplitFn))
    }

    fn debug(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::write!(f, "std.string.split function")
    }
}

#[derive(Debug, Default)]
pub struct SplitIter {
    pattern: Rc<str>,
    string: Rc<str>,
}

impl ExternFnOwned for SplitIter {
    fn call<'host>(self: Rc<Self>, argument: Value<'host>) -> Result<Value<'host>, Error<'host>> {
        let cursor = argument.into_i64()? as usize;
        Ok(Value::from(self.string.get(cursor..).map(|remaining| {
            let (result, cursor) = remaining
                .split_once(&*self.pattern)
                .map_or((remaining, self.string.len() + 1), |(result, _)| {
                    (result, cursor + result.len() + self.pattern.len())
                });
            Value::Tuple([(cursor as i64).into(), Value::String(Rc::from(result))].into())
        })))
    }

    fn debug(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::write!(f, "std.string.split iterator")
    }
}

#[derive(Debug, Default)]
pub struct StringSplitWhitespaceFn;

impl ExternFn for StringSplitWhitespaceFn {
    fn call<'host>(&'host self, argument: Value<'host>) -> Result<Value<'host>, Error<'host>> {
        let string = argument.into_str()?;

        Ok(Value::Tuple(
            [
                Value::from(0),
                Function::owned(Rc::new(SplitWhitespaceIter { string })).into(),
            ]
            .into(),
        ))
    }

    fn as_static(&self) -> Option<Function<'static>> {
        Some(espy_paws::Function::borrow(&StringSplitWhitespaceFn))
    }

    fn debug(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::write!(f, "std.string.split_whitespace function")
    }
}

#[derive(Debug, Default)]
pub struct SplitWhitespaceIter {
    string: Rc<str>,
}

impl ExternFnOwned for SplitWhitespaceIter {
    fn call<'host>(self: Rc<Self>, argument: Value<'host>) -> Result<Value<'host>, Error<'host>> {
        let cursor = argument.into_i64()? as usize;
        Ok(Value::from(self.string.get(cursor..).map(|remaining| {
            let (result, cursor) = remaining.split_whitespace().next().map_or(
                (remaining, self.string.len()),
                |result| {
                    (
                        result,
                        cursor
                            + result.len()
                            + (result.as_ptr() as usize - remaining.as_ptr() as usize),
                    )
                },
            );
            Value::Tuple(
                [
                    (if cursor == self.string.len() {
                        i64::MAX
                    } else {
                        cursor as i64
                    })
                    .into(),
                    Value::String(Rc::from(result)),
                ]
                .into(),
            )
        })))
    }

    fn debug(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::write!(f, "std.string.split_whitespace iterator")
    }
}

#[derive(Debug, Default)]
pub struct StringTrimWhitespaceFn;

impl ExternFn for StringTrimWhitespaceFn {
    fn call<'host>(&'host self, argument: Value<'host>) -> Result<Value<'host>, Error<'host>> {
        Ok(Value::String(argument.into_str()?.trim().into()))
    }

    fn as_static(&self) -> Option<Function<'static>> {
        Some(espy_paws::Function::borrow(&StringTrimWhitespaceFn))
    }

    fn debug(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::write!(f, "std.string.split_whitespace function")
    }
}

#[derive(Debug, Default)]
pub struct OptionLib;

impl Extern for OptionLib {
    fn index<'host>(&'host self, index: Value<'host>) -> Result<Value<'host>, Error<'host>> {
        let index = index.into_str()?;
        match &*index {
            "expect" => Ok(Function::borrow(&OptionExpectFn).into()),
            "unwrap" => Ok(Function::borrow(&OptionUnwrapFn).into()),
            "unwrap_or" => Ok(Function::borrow(&OptionUnwrapOrFn).into()),
            _ => Err(Error::IndexNotFound {
                index: index.into(),
                container: Value::borrow(self),
            }),
        }
    }

    fn as_static(&self) -> Option<Value<'static>> {
        Some(espy_paws::Value::borrow(&OptionLib))
    }

    fn debug(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::write!(f, "std.option module")
    }
}

#[derive(Debug, Default)]
pub struct OptionUnwrapFn;

impl ExternFn for OptionUnwrapFn {
    fn call<'host>(&'host self, argument: Value<'host>) -> Result<Value<'host>, Error<'host>> {
        Ok(argument.into_option()?.ok_or(LibraryError::UnwrapFailed)?)
    }

    fn as_static(&self) -> Option<Function<'static>> {
        Some(espy_paws::Function::borrow(&OptionUnwrapFn))
    }

    fn debug(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::write!(f, "std.option.unwrap function")
    }
}

#[derive(Debug, Default)]
pub struct OptionUnwrapOrFn;

impl ExternFn for OptionUnwrapOrFn {
    fn call<'host>(&'host self, argument: Value<'host>) -> Result<Value<'host>, Error<'host>> {
        Ok(argument.get(0)?.into_option()?.unwrap_or(argument.get(1)?))
    }

    fn as_static(&self) -> Option<Function<'static>> {
        Some(espy_paws::Function::borrow(&OptionUnwrapOrFn))
    }

    fn debug(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::write!(f, "std.option.unwrap_or function")
    }
}

#[derive(Debug, Default)]
pub struct OptionExpectFn;

impl ExternFn for OptionExpectFn {
    fn call<'host>(&'host self, argument: Value<'host>) -> Result<Value<'host>, Error<'host>> {
        Ok(argument
            .get(0)?
            .into_option()?
            .ok_or(LibraryError::ExpectFailed(argument.get(1)?.into_str()?))?)
    }

    fn as_static(&self) -> Option<Function<'static>> {
        Some(espy_paws::Function::borrow(&OptionExpectFn))
    }

    fn debug(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::write!(f, "std.option.expect function")
    }
}
