use std::rc::Rc;

pub use espy_ears as parser;
pub use espy_eyes as lexer;
pub use espy_paws as interpreter;
pub use espy_tail as compiler;

pub use interpreter::{Error, Extern, Function, Storage, Type, Value, function, function_mut};

#[derive(Debug)]
pub struct Program(interpreter::Program);

impl Program {
    pub fn eval<'host>(&self) -> Result<Value<'host>, Error<'host>> {
        self.0.eval(0, Vec::new())
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
        let actual = Program::try_from("let x = 1, 2; let y = 3, 4; x.1 * y.0").unwrap();
        println!("{actual:?}");
        assert!(actual.eval().unwrap().eq(6.into()).unwrap())
    }

    #[test]
    fn named_tuples() {
        let actual = Program::try_from(
            "let x = first: 1, second: 2; let y = third: 3, fourth: 4; x.second * y.0",
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
    fn enums_usage() {
        let actual =
            Program::try_from("let Option = enum Some: any, None: unit end; Option.Some 1")
                .unwrap();
        println!("{actual:?}");
        let (variant, value) = interpreter::EnumVariant::try_from(actual.eval().unwrap())
            .unwrap()
            .unwrap();
        assert_eq!(variant, Some("Some".into()));
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
                (Some(Rc::from("x")), Type::Any.into()),
                (Some(Rc::from("y")), Type::Any.into()),
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
                .piped(Value::borrow(&function(f)))
                .eval()
                .unwrap()
                .eq(12.into())
                .unwrap()
        )
    }

    #[test]
    fn rust_closure() {
        let four = 4;
        let f = function(|arg| Ok(Value::from(arg.into_i64()? * four)));

        assert!(
            Function::try_from(Program::try_from("with f; f(3)").unwrap().eval().unwrap(),)
                .unwrap()
                .piped(Value::borrow(&f))
                .eval()
                .unwrap()
                .eq(12.into())
                .unwrap()
        )
    }

    #[test]
    fn hello_world() {
        let mut message = Rc::from("");
        let print = function_mut(|arg| {
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
            .piped(Value::borrow(&print))
            .eval()
            .unwrap()
            .eq(().into())
            .unwrap()
                && &*message == "Hello, world!"
        )
    }

    #[test]
    fn std_io() {
        #[derive(Default)]
        struct Std {
            io: Io,
        }
        #[derive(Default)]
        struct Io {
            print: Print,
        }
        #[derive(Default)]
        struct Print {
            log: std::cell::Cell<Vec<Rc<str>>>,
        }

        impl Extern for Std {
            fn index<'host>(
                &'host self,
                index: Value<'host>,
            ) -> Result<Value<'host>, Error<'host>> {
                match index {
                    Value {
                        storage: Storage::String(x),
                    } if &*x == "io" => Ok(Value::borrow(&self.io)),
                    index => Err(Error::IndexNotFound {
                        index,
                        container: Value::borrow(self),
                    }),
                }
            }

            fn debug(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                write!(f, "std module")
            }
        }

        impl Extern for Io {
            fn index<'host>(
                &'host self,
                index: Value<'host>,
            ) -> Result<Value<'host>, Error<'host>> {
                match index {
                    Value {
                        storage: Storage::String(x),
                    } if &*x == "print" => Ok(Value::borrow(&self.print)),
                    index => Err(Error::IndexNotFound {
                        index,
                        container: Value::borrow(self),
                    }),
                }
            }

            fn debug(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                write!(f, "io module")
            }
        }

        impl Extern for Print {
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

        let std = Std::default();

        assert!(
            Function::try_from(
                Program::try_from("with std; std.io.print \"Hello, world!\"")
                    .unwrap()
                    .eval()
                    .unwrap(),
            )
            .unwrap()
            .piped(Value::borrow(&std))
            .eval()
            .unwrap()
            .eq(().into())
            .unwrap()
                && std
                    .io
                    .print
                    .log
                    .take()
                    .iter()
                    .map(|s| &**s)
                    .eq(["Hello, world!"])
        )
    }
}
