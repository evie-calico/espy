pub use espy_ears as parser;
pub use espy_eyes as lexer;
pub use espy_paws as interpreter;
pub use espy_tail as compiler;

pub use interpreter::{Error, Extern, ExternFn, ExternFnOwned, ExternOwned, Function, Type, Value};

#[derive(Debug)]
pub struct Program(interpreter::Program);

impl Program {
    /// # Errors
    ///
    /// Returns an error if the evaluating the program's root block results in an error
    pub fn eval<'host>(&self) -> Result<Value<'host>, Error<'host>> {
        self.0.eval()
    }
}

impl<'source> TryFrom<&'source str> for Program {
    type Error = compiler::Error<'source>;

    fn try_from(s: &'source str) -> Result<Self, Self::Error> {
        compiler::compile(parser::Block::new(&mut lexer::Lexer::from(s).peekable())).map(
            |program| {
                Program(
                    interpreter::Program::try_from(program)
                        .expect("textual programs may not produce invalid bytecode"),
                )
            },
        )
    }
}

#[cfg(test)]
mod tests {
    use std::rc::Rc;

    use super::*;

    #[test]
    fn arithmetic() {
        let actual = Program::try_from("(1 + 2) * 4").unwrap();
        println!("{actual:?}");
        assert!(actual.eval().unwrap().eq(12.into()).unwrap());
    }

    #[test]
    fn tuples() {
        let actual =
            Program::try_from("let (one, two) = 1, 2; let y = 3, _: (4, 5); two * (y.1).0")
                .unwrap();
        println!("{actual:?}");
        assert!(actual.eval().unwrap().eq(8.into()).unwrap());
    }

    #[test]
    fn named_tuples() {
        let actual = Program::try_from(
            "let { `second number`: second } = first: 1, `second number`: 2; let { third: three } = third: 3, fourth: 4; second * three",
        )
        .unwrap();
        println!("{actual:?}");
        assert!(actual.eval().unwrap().eq(6.into()).unwrap());
    }

    #[test]
    fn functions() {
        let actual = Program::try_from("let f = {with x; x * x}; f 4").unwrap();
        println!("{actual:?}");
        assert!(actual.eval().unwrap().eq(16.into()).unwrap());
    }

    #[test]
    fn closures() {
        let actual = Program::try_from("let f = {let y = 10; with x; x * y}; f 4").unwrap();
        println!("{actual:?}");
        assert!(actual.eval().unwrap().eq(40.into()).unwrap());
    }

    #[test]
    fn pipes() {
        let actual = Program::try_from("let f = {with args; args.0 * args.1}; 2 |> f 128").unwrap();
        println!("{actual:?}");
        assert!(actual.eval().unwrap().eq(256.into()).unwrap());
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
        );
    }

    #[test]
    fn rust_function() {
        struct F;
        impl ExternFn for F {
            fn call<'host>(&self, arg: Value<'host>) -> Result<Value<'host>, Error<'host>> {
                Ok(Value::from(arg.into_i64()? * 4))
            }
        }

        assert!(
            Function::try_from(Program::try_from("with f; f(3)").unwrap().eval().unwrap(),)
                .unwrap()
                .piped(Function::borrow(&F).into())
                .eval()
                .unwrap()
                .eq(12.into())
                .unwrap()
        );
    }

    #[test]
    fn rust_closure() {
        struct F {
            four: i64,
        }
        impl ExternFn for F {
            fn call<'host>(&self, arg: Value<'host>) -> Result<Value<'host>, Error<'host>> {
                Ok(Value::from(arg.into_i64()? * self.four))
            }
        }

        assert!(
            Function::try_from(Program::try_from("with f; f(3)").unwrap().eval().unwrap(),)
                .unwrap()
                .piped(Function::borrow(&F { four: 4 }).into())
                .eval()
                .unwrap()
                .eq(12.into())
                .unwrap()
        );
    }

    #[test]
    fn rust_owned() {
        struct F;
        impl ExternFn for F {
            fn call<'host>(&self, arg: Value<'host>) -> Result<Value<'host>, Error<'host>> {
                struct Owned {
                    num: i64,
                }
                impl ExternFnOwned for Owned {
                    fn call<'host>(
                        self: Rc<Self>,
                        arg: Value<'host>,
                    ) -> Result<Value<'host>, Error<'host>> {
                        Ok(Value::from(arg.into_i64()? * self.num))
                    }
                }
                Ok(Function::owned(Rc::new(Owned {
                    num: arg.into_i64()?,
                }))
                .into())
            }
        }

        assert!(
            Function::try_from(
                Program::try_from("with f; f (f 2 3) (f 1 6)")
                    .unwrap()
                    .eval()
                    .unwrap(),
            )
            .unwrap()
            .piped(Function::borrow(&F).into())
            .eval()
            .unwrap()
            .eq(36.into())
            .unwrap()
        );
    }

    #[test]
    fn hello_world() {
        #[derive(Default)]
        struct Print {
            message: std::cell::RefCell<Rc<str>>,
        }
        impl ExternFn for Print {
            fn call<'host>(&self, arg: Value<'host>) -> Result<Value<'host>, Error<'host>> {
                *self.message.borrow_mut() = arg.into_str()?;
                println!("{}", self.message.borrow());
                Ok(().into())
            }
        }

        let print = Print::default();

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
                && &**print.message.borrow() == "Hello, world!"
        );
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
        );
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
                    set accumulator = accumulator.* + i;
                };
                10, countdown |> foreach sum;
                accumulator.*",
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

    #[test]
    fn downcast() {
        #[derive(Debug, PartialEq)]
        struct Secret {
            number: i64,
        }

        impl Extern for Secret {
            fn index<'host>(
                &'host self,
                index: Value<'host>,
            ) -> Result<Value<'host>, Error<'host>> {
                Err(Error::IndexNotFound {
                    index,
                    container: Value::borrow(self),
                })
            }

            fn any(&self) -> Option<&dyn std::any::Any> {
                Some(self)
            }

            fn debug(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                write!(f, "secret value")
            }
        }

        let one = Secret { number: 1 };
        let two = Secret { number: 2 };
        let five = Secret { number: 5 };

        assert_eq!(
            Some(&five),
            Function::try_from(
                Program::try_from("with (one, two, five); five")
                    .unwrap()
                    .eval()
                    .unwrap(),
            )
            .unwrap()
            .piped(Value::Tuple(
                [
                    Value::borrow(&one),
                    Value::borrow(&two),
                    Value::borrow(&five)
                ]
                .into()
            ))
            .eval()
            .unwrap()
            .downcast_extern::<Secret>()
        );
    }
}
