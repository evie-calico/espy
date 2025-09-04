use std::rc::Rc;

pub use espy_ears as parser;
pub use espy_eyes as lexer;
pub use espy_paws as interpreter;
pub use espy_tail as compiler;

pub use interpreter::{
    Error, Extern, ExternFn, Function, Storage, Type, Value, wrap_fn, wrap_fn_mut,
};

#[derive(Debug)]
pub struct Program(interpreter::Program);

impl Program {
    pub fn eval<'host>(&self) -> Result<Value<'host>, Error<'host>> {
        self.0.eval(0, &mut Vec::new())
    }
}

impl<'source> TryFrom<&'source str> for Program {
    type Error = compiler::Error<'source>;

    fn try_from(s: &'source str) -> Result<Self, Self::Error> {
        compiler::Program::try_from(parser::Block::new(&mut lexer::Lexer::from(s).peekable())).map(
            |program| {
                Program(
                    interpreter::Program::try_from(Rc::from(program.compile()))
                        .expect("textual programs may not produce invalid bytecode"),
                )
            },
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn arithmetic() {
        let actual = Program::try_from("(1 + 2) * 4").unwrap();
        println!("{actual:?}");
        assert!(actual.eval().unwrap().eq(12.into()).unwrap())
    }

    #[test]
    fn tuples() {
        let actual =
            Program::try_from("let (one, two) = 1, 2; let y = 3, _: (4, 5); two * (y.1).0")
                .unwrap();
        println!("{actual:?}");
        assert!(actual.eval().unwrap().eq(8.into()).unwrap())
    }

    #[test]
    fn named_tuples() {
        let actual = Program::try_from(
            "let { `second number`: second } = first: 1, `second number`: 2; let { third: three } = third: 3, fourth: 4; second * three",
        )
        .unwrap();
        println!("{actual:?}");
        assert!(actual.eval().unwrap().eq(6.into()).unwrap())
    }

    #[test]
    fn functions() {
        let actual = Program::try_from("let f = {with x; x * x}; f 4").unwrap();
        println!("{actual:?}");
        assert!(actual.eval().unwrap().eq(16.into()).unwrap())
    }

    #[test]
    fn closures() {
        let actual = Program::try_from("let f = {let y = 10; with x; x * y}; f 4").unwrap();
        println!("{actual:?}");
        assert!(actual.eval().unwrap().eq(40.into()).unwrap())
    }

    #[test]
    fn pipes() {
        let actual = Program::try_from("let f = {with args; args.0 * args.1}; 2 |> f 128").unwrap();
        println!("{actual:?}");
        assert!(actual.eval().unwrap().eq(256.into()).unwrap())
    }

    #[test]
    fn structs_usage() {
        let actual = Program::try_from(
            "let Point = struct x: i64, y: i64 then new: { with args; x: args.0, y: args.1 }, x: { with this; this.x } end; (Point.new 1, 2).x ()",
        )
        .unwrap();
        println!("{actual:?}");
        assert!(actual.eval().unwrap().eq(1.into()).unwrap())
    }

    #[test]
    fn enums_usage() {
        let actual =
            Program::try_from("let Option = enum Some: any, None: unit end; Option.Some 1")
                .unwrap();
        println!("{actual:?}");
        let (variant, value) = interpreter::EnumVariant::try_from(actual.eval().unwrap())
            .unwrap()
            .unwrap();
        assert_eq!(variant, "Some".into());
        assert!(value.eq(1.into()).unwrap());
    }

    #[test]
    fn options() {
        let actual = Program::try_from(
            "let OptionI64 = option i64; (OptionI64.Some 1), (OptionI64.None ())",
        )
        .unwrap();
        println!("{actual:?}");
        assert!(
            actual
                .eval()
                .unwrap()
                .eq(Value::concat(Some(1).into(), None::<i64>.into()))
                .unwrap()
        )
    }

    #[test]
    fn structures() {
        let actual = Program::try_from("struct x: any, y: any end").unwrap();
        println!("{actual:?}");
        let value = actual.eval().unwrap();
        let struct_type = value.into_struct_type().unwrap();
        assert_eq!(
            struct_type.inner,
            interpreter::ComplexType::from(interpreter::Tuple::from([
                (Rc::from("x"), Type::Any.into()),
                (Rc::from("y"), Type::Any.into()),
            ]))
        )
    }

    #[test]
    fn rust_function() {
        fn f(arg: Value) -> Result<Value, Error> {
            Ok(Value::from(arg.into_i64()? * 4))
        }

        assert!(
            Function::try_from(Program::try_from("with f; f(3)").unwrap().eval().unwrap(),)
                .unwrap()
                .piped(Function::borrow(&wrap_fn(f)).into())
                .eval()
                .unwrap()
                .eq(12.into())
                .unwrap()
        )
    }

    #[test]
    fn rust_closure() {
        let four = 4;
        let f = wrap_fn(|arg| Ok(Value::from(arg.into_i64()? * four)));

        assert!(
            Function::try_from(Program::try_from("with f; f(3)").unwrap().eval().unwrap(),)
                .unwrap()
                .piped(Function::borrow(&f).into())
                .eval()
                .unwrap()
                .eq(12.into())
                .unwrap()
        )
    }

    #[test]
    fn hello_world() {
        let mut message = Rc::from("");
        let print = wrap_fn_mut(|arg| {
            message = arg.into_str()?;
            println!("{message}");
            Ok(().into())
        });

        assert!(
            Function::try_from(
                Program::try_from("with print; print \"Hello, world!\"")
                    .unwrap()
                    .eval()
                    .unwrap(),
            )
            .unwrap()
            .piped(Function::borrow(&print).into())
            .eval()
            .unwrap()
            .eq(().into())
            .unwrap()
                && &*message == "Hello, world!"
        )
    }

    #[test]
    fn print() {
        #[derive(Default)]
        struct Io {
            print: Print,
        }
        #[derive(Default)]
        struct Print {
            log: std::cell::Cell<Vec<Rc<str>>>,
        }

        impl Extern for Io {
            fn index<'host>(
                &'host self,
                index: Value<'host>,
            ) -> Result<Value<'host>, Error<'host>> {
                let index = index.into_str()?;
                match &*index {
                    "print" => Ok(Function::borrow(&self.print).into()),
                    _ => Err(Error::IndexNotFound {
                        index: index.into(),
                        container: Value::borrow(self),
                    }),
                }
            }

            fn debug(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                write!(f, "io module")
            }
        }

        impl ExternFn for Print {
            fn call<'host>(
                &'host self,
                message: Value<'host>,
            ) -> Result<Value<'host>, Error<'host>> {
                let message = message.into_str()?;
                println!("{message}");
                let mut log = self.log.take();
                log.push(message);
                self.log.set(log);
                Ok(().into())
            }

            fn debug(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                write!(f, "io.print")
            }
        }

        let io = Io::default();

        assert!(
            Function::try_from(
                Program::try_from("with io; io.print \"Hello, world!\"")
                    .unwrap()
                    .eval()
                    .unwrap(),
            )
            .unwrap()
            .piped(Value::borrow(&io))
            .eval()
            .unwrap()
            .eq(().into())
            .unwrap()
                && io
                    .print
                    .log
                    .take()
                    .iter()
                    .map(|s| &**s)
                    .eq(["Hello, world!"])
        )
    }

    #[test]
    fn iter() {
        #[derive(Default)]
        struct ForEach;

        impl ExternFn for ForEach {
            fn call<'host>(&'host self, args: Value<'host>) -> Result<Value<'host>, Error<'host>> {
                let mut iterator = args.get(0)?;
                let next = args.get(1)?.into_function()?;
                let foreach = args.get(2)?.into_function()?;
                while let Some(result) = next.clone().piped(iterator).eval()?.into_option()? {
                    iterator = result.get(0)?;
                    foreach.clone().piped(result.get(1)?).eval()?;
                }
                Ok(().into())
            }
        }

        let foreach = ForEach;

        assert_eq!(
            Function::try_from(
                Program::try_from(
                    "with foreach;
                let countdown = {
                    let item = option i64, i64;
                    with i;
                    if i >= 0 then
                        item.Some i - 1, i
                    else then
                        item.None ()
                    end
                };
                let accumulator = mut 0;
                let sum = {
                    let accumulator = accumulator;
                    with i;
                    set accumulator = *accumulator + i;
                };
                10, countdown |> foreach sum;
                *accumulator",
                )
                .unwrap()
                .eval()
                .unwrap(),
            )
            .unwrap()
            .piped(Function::borrow(&foreach).into())
            .eval()
            .unwrap()
            .into_i64()
            .unwrap(),
            55
        );
    }
}
