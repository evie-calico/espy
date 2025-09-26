use espy_paws::{Error, ExternFnOwned, Function, Tuple, Type, Value, extern_impl};
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

extern_impl! {
    pub struct Lib {
        iter: Value::borrow(&iter::Lib),
        string: Value::borrow(&string::Lib),
        option: Value::borrow(&option::Lib),
        typeof: Function::borrow(&Typeof),
    }
}

extern_impl! {
    #[espy(debug = "std.typeof function")]
    pub fn Typeof<'host>(&self, argument) {
        Ok(argument.type_of()?.into())
    }
}

mod iter {
    use super::*;

    extern_impl! {
        #[espy(debug = "std.iter module")]
        pub struct Lib {
            filter: Function::borrow(&Filter),
            fold: Function::borrow(&Fold),
            foreach: Function::borrow(&Foreach),
            map: Function::borrow(&Map),
            range: Function::borrow(&Range),
            reduce: Function::borrow(&Reduce),
            reduce_once: Function::borrow(&ReduceOnce),
            repeat: Function::borrow(&Repeat),
            skip: Function::borrow(&Skip),
            take: Function::borrow(&Take),
        }
    }

    extern_impl! {
        #[espy(debug = "std.iter.foreach function")]
        pub fn Foreach<'host>(&self, argument) {
            let mut iterator = argument.get(0)?;
            let next = argument.get(1)?.into_function()?;
            let foreach = argument.get(2)?.into_function()?;
            while let Some(result) = next.clone().piped(iterator).eval()?.into_option()? {
                iterator = result.get(0)?;
                foreach.clone().piped(result.get(1)?).eval()?;
            }
            Ok(().into())
        }
    }

    extern_impl! {
        #[espy(debug = "std.iter.fold function")]
        pub fn Fold<'host>(&self, argument) {
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
    }

    extern_impl! {
        #[espy(debug = "std.iter.reduce function")]
        pub fn Reduce<'host>(&self, argument) {
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
    }

    extern_impl! {
        #[espy(debug = "std.iter.reduce_once function")]
        pub fn ReduceOnce<'host>(&self, argument) {
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
    }

    extern_impl! {
        #[espy(debug = "std.iter.range function")]
        pub fn Range<'host>(&self, argument) {
            let current = argument.get(0)?;
            let limit = argument.get(1)?.into_i64()?;

            Ok(Value::Tuple(Tuple::from([
                current,
                Function::owned(Rc::new(RangeIter { limit })).into(),
            ])))
        }
    }

    #[derive(Debug, Default)]
    pub struct RangeIter {
        limit: i64,
    }

    impl ExternFnOwned for RangeIter {
        fn call<'host>(
            self: Rc<Self>,
            argument: Value<'host>,
        ) -> Result<Value<'host>, Error<'host>> {
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

    extern_impl! {
        #[espy(debug = "std.iter.repeat function")]
        pub fn Repeat<'host>(&self, argument) {
            Ok(Value::Tuple(Tuple::from([
                argument,
                Function::borrow(&RepeatIter).into(),
            ])))
        }
    }

    extern_impl! {
        #[espy(debug = "std.iter.repeat iterator")]
        pub fn RepeatIter<'host>(&self, argument) {
            Ok(Value::from(Some(Value::Tuple(Tuple::from([
                argument.clone(),
                argument,
            ])))))
        }
    }

    extern_impl! {
        #[espy(debug = "std.iter.skip function")]
        pub fn Skip<'host>(&self, argument) {
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
    }

    extern_impl! {
        #[espy(debug = "std.iter.take function")]
        pub fn Take<'host>(&self, argument) {
            let iterator = argument.get(0)?;
            let next = argument.get(1)?;
            let count = argument.get(2)?;

            Ok(Value::Tuple(Tuple::from([
                Value::Tuple(Tuple::from([iterator, count])),
                Function::borrow(&TakeIter).piped(next).into(),
            ])))
        }
    }

    extern_impl! {
        #[espy(debug = "std.iter.take iterator")]
        pub fn TakeIter<'host>(&self, argument) {
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
    }

    extern_impl! {
        #[espy(debug = "std.iter.map function")]
        pub fn Map<'host>(&self, argument) {
            let iterator = argument.get(0)?;
            let next = argument.get(1)?;
            let map = argument.get(2)?;

            Ok(Value::Tuple(Tuple::from([
                iterator,
                Function::borrow(&MapIter).piped(next).piped(map).into(),
            ])))
        }
    }

    extern_impl! {
        #[espy(debug = "std.iter.map iterator")]
        pub fn MapIter<'host>(&self, argument) {
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
    }

    extern_impl! {
        #[espy(debug = "std.iter.filter function")]
        pub fn Filter<'host>(&self, argument) {
            let iterator = argument.get(0)?;
            let next = argument.get(1)?;
            let map = argument.get(2)?;

            Ok(Value::Tuple(Tuple::from([
                iterator,
                Function::borrow(&FilterIter).piped(next).piped(map).into(),
            ])))
        }
    }

    extern_impl! {
        #[espy(debug = "std.iter.filter iterator")]
        pub fn FilterIter<'host>(&self, argument) {
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
    }
}

mod string {
    use super::*;

    extern_impl! {
        #[espy(debug = "std.string module")]
        pub struct Lib {
            concat: Function::borrow(&Concat),
            from_i64: Function::borrow(&FromI64),
            parse_i64: Function::borrow(&ParseI64),
            split: Function::borrow(&Split),
            split_whitespace: Function::borrow(&SplitWhitespace),
            trim_whitespace: Function::borrow(&TrimWhitespace),
        }
    }

    extern_impl! {
        #[espy(debug = "std.string.concat function")]
        pub fn Concat<'host>(&self, argument) {
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
    }

    extern_impl! {
        #[espy(debug = "std.string.from_i64 function")]
        pub fn FromI64<'host>(&self, argument) {
            Ok(Value::String(
                argument.into_i64()?.to_string().as_str().into(),
            ))
        }
    }

    extern_impl! {
        #[espy(debug = "std.string.parse_i64 function")]
        pub fn ParseI64<'host>(&self, argument) {
            Ok(argument
                .into_str()?
                .parse::<i64>()
                .ok()
                .map(Value::I64)
                .into())
        }
    }

    extern_impl! {
        #[espy(debug = "std.string.split function")]
        fn Split<'host>(&self, argument) {
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
    }

    #[derive(Debug, Default)]
    pub struct SplitIter {
        pattern: Rc<str>,
        string: Rc<str>,
    }

    impl ExternFnOwned for SplitIter {
        fn call<'host>(
            self: Rc<Self>,
            argument: Value<'host>,
        ) -> Result<Value<'host>, Error<'host>> {
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

    extern_impl! {
        #[espy(debug = "std.string.split_whitespace function")]
        fn SplitWhitespace <'host>(&self, argument) {
            let string = argument.into_str()?;

            Ok(Value::Tuple(
                [
                    Value::from(0),
                    Function::owned(Rc::new(SplitWhitespaceIter { string })).into(),
                ]
                .into(),
            ))
        }
    }

    #[derive(Debug, Default)]
    pub struct SplitWhitespaceIter {
        string: Rc<str>,
    }

    impl ExternFnOwned for SplitWhitespaceIter {
        fn call<'host>(
            self: Rc<Self>,
            argument: Value<'host>,
        ) -> Result<Value<'host>, Error<'host>> {
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

    extern_impl! {
        #[espy(debug = "std.string.trim_whitespace function")]
        fn TrimWhitespace<'host>(&self, argument) {
            Ok(Value::String(argument.into_str()?.trim().into()))
        }
    }
}

mod option {
    use super::*;

    extern_impl! {
        #[espy(debug = "std.option module")]
        pub struct Lib {
            expect: espy_paws::Function::borrow(&Expect),
            unwrap: espy_paws::Function::borrow(&Unwrap),
            unwrap_or: espy_paws::Function::borrow(&UnwrapOr),
        }
    }

    extern_impl! {
        #[espy(debug = "std.option.expect function")]
        pub fn Expect<'host>(&self, argument) {
            Ok(argument
                .get(0)?
                .into_option()?
                .ok_or(LibraryError::ExpectFailed(argument.get(1)?.into_str()?))?)
        }
    }

    extern_impl! {
        #[espy(debug = "std.option.unwrap function")]
        pub fn Unwrap<'host>(&self, argument) {
            Ok(argument.into_option()?.ok_or(LibraryError::UnwrapFailed)?)
        }
    }

    extern_impl! {
        #[espy(debug = "std.option.unwrap_or function")]
        pub fn UnwrapOr<'host>(&self, argument) {
            Ok(argument.get(0)?.into_option()?.unwrap_or(argument.get(1)?))
        }
    }
}
