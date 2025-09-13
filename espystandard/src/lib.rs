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
        static ITER: IterLib = IterLib;
        static STRING: StringLib = StringLib;
        static OPTION: OptionLib = OptionLib::new();
        static TYPE_OF: TypeofFn = TypeofFn;

        let index = index.into_str()?;
        match &*index {
            "iter" => Ok(Value::borrow(&ITER)),
            "string" => Ok(Value::borrow(&STRING)),
            "option" => Ok(Value::borrow(&OPTION)),
            "typeof" => Ok(Function::borrow(&TYPE_OF).into()),
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
pub struct TypeofFn;

impl ExternFn for TypeofFn {
    fn call<'host>(&'host self, argument: Value<'host>) -> Result<Value<'host>, Error<'host>> {
        Ok(argument.type_of()?.into())
    }

    fn debug(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::write!(f, "std.typeof function")
    }
}

#[derive(Debug, Default)]
pub struct IterLib;

impl Extern for IterLib {
    fn index<'host>(&'host self, index: Value<'host>) -> Result<Value<'host>, Error<'host>> {
        static FILTER: IterFilterFn = IterFilterFn::new();
        static FOLD: IterFoldFn = IterFoldFn;
        static FOREACH: IterForeachFn = IterForeachFn;
        static MAP: IterMapFn = IterMapFn::new();
        static RANGE: IterRangeFn = IterRangeFn;
        static REDUCE: IterReduceFn = IterReduceFn;
        static REDUCE_ONCE: IterReduceOnceFn = IterReduceOnceFn;
        static REPEAT: IterRepeatFn = IterRepeatFn::new();
        static SKIP: IterSkipFn = IterSkipFn;
        static TAKE: IterTakeFn = IterTakeFn;

        let index = index.into_str()?;
        match &*index {
            "filter" => Ok(Function::borrow(&FILTER).into()),
            "fold" => Ok(Function::borrow(&FOLD).into()),
            "foreach" => Ok(Function::borrow(&FOREACH).into()),
            "map" => Ok(Function::borrow(&MAP).into()),
            "range" => Ok(Function::borrow(&RANGE).into()),
            "reduce" => Ok(Function::borrow(&REDUCE).into()),
            "reduce_once" => Ok(Function::borrow(&REDUCE_ONCE).into()),
            "repeat" => Ok(Function::borrow(&REPEAT).into()),
            "skip" => Ok(Function::borrow(&SKIP).into()),
            "take" => Ok(Function::borrow(&TAKE).into()),
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
        Ok(accumulator.into())
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

    fn debug(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::write!(f, "std.iter.range function")
    }
}

#[derive(Debug, Default)]
pub struct RangeIter {
    limit: i64,
}

impl ExternFnOwned for RangeIter {
    fn call<'host>(&self, argument: Value<'host>) -> Result<Value<'host>, Error<'host>> {
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
pub struct IterRepeatFn {
    iter: RepeatIter,
}

impl IterRepeatFn {
    #[must_use]
    pub const fn new() -> Self {
        Self { iter: RepeatIter }
    }
}

impl ExternFn for IterRepeatFn {
    fn call<'host>(&'host self, value: Value<'host>) -> Result<Value<'host>, Error<'host>> {
        Ok(Value::Tuple(Tuple::from([
            value,
            Function::borrow(&self.iter).into(),
        ])))
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

    fn debug(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::write!(f, "std.iter.skip function")
    }
}

#[derive(Debug, Default)]
pub struct IterTakeFn;

impl ExternFn for IterTakeFn {
    fn call<'host>(&'host self, argument: Value<'host>) -> Result<Value<'host>, Error<'host>> {
        static ITER: TakeIter = TakeIter;

        let iterator = argument.get(0)?;
        let next = argument.get(1)?;
        let count = argument.get(2)?;

        Ok(Value::Tuple(Tuple::from([
            Value::Tuple(Tuple::from([iterator, count])),
            Function::borrow(&ITER).piped(next).into(),
        ])))
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

    fn debug(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::write!(f, "std.iter.take iterator")
    }
}

#[derive(Debug, Default)]
pub struct IterMapFn {
    iter: MapIter,
}

impl IterMapFn {
    #[must_use]
    pub const fn new() -> Self {
        Self { iter: MapIter }
    }
}

impl ExternFn for IterMapFn {
    fn call<'host>(&'host self, argument: Value<'host>) -> Result<Value<'host>, Error<'host>> {
        let iterator = argument.get(0)?;
        let next = argument.get(1)?;
        let map = argument.get(2)?;

        Ok(Value::Tuple(Tuple::from([
            iterator,
            Function::borrow(&self.iter).piped(next).piped(map).into(),
        ])))
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

    fn debug(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::write!(f, "std.iter.map iterator")
    }
}

#[derive(Debug, Default)]
pub struct IterFilterFn {
    iter: FilterIter,
}

impl IterFilterFn {
    #[must_use]
    pub const fn new() -> Self {
        Self { iter: FilterIter }
    }
}

impl ExternFn for IterFilterFn {
    fn call<'host>(&'host self, argument: Value<'host>) -> Result<Value<'host>, Error<'host>> {
        let iterator = argument.get(0)?;
        let next = argument.get(1)?;
        let map = argument.get(2)?;

        Ok(Value::Tuple(Tuple::from([
            iterator,
            Function::borrow(&self.iter).piped(next).piped(map).into(),
        ])))
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

    fn debug(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::write!(f, "std.iter.filter iterator")
    }
}

#[derive(Debug, Default)]
pub struct StringLib;

impl Extern for StringLib {
    fn index<'host>(&'host self, index: Value<'host>) -> Result<Value<'host>, Error<'host>> {
        static CONCAT: StringConcatFn = StringConcatFn;
        static SPLIT: StringSplitFn = StringSplitFn;
        static SPLIT_WHITESPACE: StringSplitWhitespaceFn = StringSplitWhitespaceFn;

        let index = index.into_str()?;
        match &*index {
            "concat" => Ok(Function::borrow(&CONCAT).into()),
            "split" => Ok(Function::borrow(&SPLIT).into()),
            "split_whitespace" => Ok(Function::borrow(&SPLIT_WHITESPACE).into()),
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
    fn call<'host>(&self, argument: Value<'host>) -> Result<Value<'host>, Error<'host>> {
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

    fn debug(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::write!(f, "std.string.split_whitespace function")
    }
}

#[derive(Debug, Default)]
pub struct SplitWhitespaceIter {
    string: Rc<str>,
}

impl ExternFnOwned for SplitWhitespaceIter {
    fn call<'host>(&self, argument: Value<'host>) -> Result<Value<'host>, Error<'host>> {
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
pub struct OptionLib {
    unwrap: OptionUnwrapFn,
    expect: OptionExpectFn,
}

impl OptionLib {
    #[must_use]
    pub const fn new() -> Self {
        Self {
            unwrap: OptionUnwrapFn,
            expect: OptionExpectFn,
        }
    }
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
        Ok(argument.into_option()?.ok_or(LibraryError::UnwrapFailed)?)
    }

    fn debug(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::write!(f, "std.option.unwrap function")
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

    fn debug(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::write!(f, "std.option.expect function")
    }
}
