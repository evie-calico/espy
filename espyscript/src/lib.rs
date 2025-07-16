use std::rc::Rc;

pub use espy_ears as parser;
pub use espy_eyes as lexer;
pub use espy_paws as interpreter;
pub use espy_tail as compiler;

pub use interpreter::Value;

#[derive(Debug)]
pub struct Program(interpreter::Program);

impl Program {
    pub fn eval<'host>(&self) -> Result<Value<'host>, interpreter::Error<'host>> {
        self.0.eval(0, Vec::new())
    }
}

impl<'source> TryFrom<&'source str> for Program {
    type Error = compiler::Error<'source>;

    fn try_from(s: &'source str) -> Result<Self, Self::Error> {
        compiler::Program::try_from(parser::Block::from(&mut lexer::Lexer::from(s).peekable())).map(
            |program| {
                Program(
                    interpreter::Program::try_from(Rc::from(compiler::Program::compile(program)))
                        .expect("textual programs may not produce invalid bytecode"),
                )
            },
        )
    }
}

#[cfg(test)]
mod tests {
    use interpreter::Storage;
    use std::rc::Rc;

    use super::*;

    #[test]
    fn arithmetic() {
        let actual = Program::try_from("(1 + 2) * 4").unwrap();
        println!("{actual:?}");
        assert!(
            actual
                .eval()
                .unwrap()
                .eq(Value {
                    storage: Storage::I64(12),
                })
                .unwrap()
        )
    }

    #[test]
    fn tuples() {
        let actual = Program::try_from("let x = 1, 2; let y = 3, 4; x.1 * y.0").unwrap();
        println!("{actual:?}");
        assert!(
            actual
                .eval()
                .unwrap()
                .eq(Value {
                    storage: Storage::I64(6),
                })
                .unwrap()
        )
    }

    #[test]
    fn named_tuples() {
        let actual = Program::try_from(
            "let x = first: 1, second: 2; let y = third: 3, fourth: 4; x.second * y.0",
        )
        .unwrap();
        println!("{actual:?}");
        assert!(
            actual
                .eval()
                .unwrap()
                .eq(Value {
                    storage: Storage::I64(6),
                })
                .unwrap()
        )
    }

    #[test]
    fn functions() {
        let actual = Program::try_from("let f = {with x; x * x}; f 4").unwrap();
        println!("{actual:?}");
        assert!(
            actual
                .eval()
                .unwrap()
                .eq(Value {
                    storage: Storage::I64(16),
                })
                .unwrap()
        )
    }

    #[test]
    fn closures() {
        let actual = Program::try_from("let f = {let y = 10; with x; x * y}; f 4").unwrap();
        println!("{actual:?}");
        assert!(
            actual
                .eval()
                .unwrap()
                .eq(Value {
                    storage: Storage::I64(40),
                })
                .unwrap()
        )
    }

    #[test]
    fn pipes() {
        let actual = Program::try_from("let f = {with args; args.0 * args.1}; 2 |> f 128").unwrap();
        println!("{actual:?}");
        assert!(
            actual
                .eval()
                .unwrap()
                .eq(Value {
                    storage: Storage::I64(256),
                })
                .unwrap()
        )
    }

    #[test]
    fn enums_usage() {
        let actual =
            Program::try_from("let Option = enum Some: any, None: () end; Option.Some 1").unwrap();
        println!("{actual:?}");
        let (variant, value) = interpreter::EnumVariant::try_from(actual.eval().unwrap())
            .unwrap()
            .unwrap();
        assert_eq!(variant, Some("Some".into()));
        assert!(value.eq(Storage::I64(1).into()).unwrap());
    }

    #[test]
    fn options() {
        let actual = Program::try_from("Some 1, None ()").unwrap();
        println!("{actual:?}");
        assert!(
            actual
                .eval()
                .unwrap()
                .eq(Value::concat(
                    Storage::Some(Rc::new(Storage::I64(1).into())).into(),
                    Storage::None.into()
                ))
                .unwrap()
        )
    }

    #[test]
    fn structures() {
        let actual = Program::try_from("struct x: any, y: any end").unwrap();
        println!("{actual:?}");
        let value = actual.eval().unwrap();
        let struct_type = interpreter::StructType::try_from(value).unwrap();
        assert!(
            struct_type
                .inner
                .eq(Storage::Tuple(interpreter::Tuple::from([
                    (Some(Rc::from("x")), Storage::Any.into()),
                    (Some(Rc::from("y")), Storage::Any.into()),
                ]))
                .into())
                .unwrap()
        )
    }

    #[test]
    fn rust_function() {
        fn f(arg: Value) -> Result<Value, interpreter::Error> {
            match arg {
                Value {
                    storage: Storage::I64(i),
                } => Ok(Storage::I64(i * 4).into()),
                arg => Err(interpreter::Error::TypeError {
                    value: arg,
                    ty: Storage::I64Type.into(),
                }),
            }
        }

        assert!(
            interpreter::Function::try_from(
                Program::try_from("with f; f(3)").unwrap().eval().unwrap(),
            )
            .unwrap()
            .piped(Storage::Borrow(&interpreter::function(f)).into())
            .eval()
            .unwrap()
            .eq(Storage::I64(12).into())
            .unwrap()
        )
    }

    #[test]
    fn rust_closure() {
        let four = 4;
        let f = interpreter::function(|arg| match arg {
            Value {
                storage: Storage::I64(i),
            } => Ok(Storage::I64(i * four).into()),
            arg => Err(interpreter::Error::TypeError {
                value: arg,
                ty: Storage::I64Type.into(),
            }),
        });

        assert!(
            interpreter::Function::try_from(
                Program::try_from("with f; f(3)").unwrap().eval().unwrap(),
            )
            .unwrap()
            .piped(Storage::Borrow(&f).into())
            .eval()
            .unwrap()
            .eq(Storage::I64(12).into())
            .unwrap()
        )
    }

    #[test]
    fn hello_world() {
        let mut message = Rc::from("");
        let print = interpreter::function_mut(|arg| match arg {
            Value {
                storage: Storage::String(i),
            } => {
                println!("{i}");
                message = i.clone();
                Ok(Storage::Unit.into())
            }
            arg => Err(interpreter::Error::TypeError {
                value: arg,
                ty: Storage::StringType.into(),
            }),
        });

        assert!(
            interpreter::Function::try_from(
                Program::try_from("with print; print \"Hello, world!\"")
                    .unwrap()
                    .eval()
                    .unwrap(),
            )
            .unwrap()
            .piped(Storage::Borrow(&print).into())
            .eval()
            .unwrap()
            .eq(Storage::Unit.into())
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

        impl interpreter::Extern for Std {
            fn index<'host>(
                &'host self,
                index: Value<'host>,
            ) -> Result<Value<'host>, interpreter::Error<'host>> {
                match index {
                    Value {
                        storage: Storage::String(x),
                    } if &*x == "io" => Ok(Storage::Borrow(&self.io).into()),
                    index => Err(interpreter::Error::IndexNotFound {
                        index,
                        container: Storage::Borrow(self).into(),
                    }),
                }
            }

            fn debug(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                write!(f, "std module")
            }
        }

        impl interpreter::Extern for Io {
            fn index<'host>(
                &'host self,
                index: Value<'host>,
            ) -> Result<Value<'host>, interpreter::Error<'host>> {
                match index {
                    Value {
                        storage: Storage::String(x),
                    } if &*x == "print" => Ok(Storage::Borrow(&self.print).into()),
                    index => Err(interpreter::Error::IndexNotFound {
                        index,
                        container: Storage::Borrow(self).into(),
                    }),
                }
            }

            fn debug(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                write!(f, "io module")
            }
        }

        impl interpreter::Extern for Print {
            fn call<'host>(
                &'host self,
                message: Value<'host>,
            ) -> Result<Value<'host>, interpreter::Error<'host>> {
                match message {
                    Value {
                        storage: Storage::String(x),
                    } => {
                        println!("{x}");
                        let mut log = self.log.take();
                        log.push(x);
                        self.log.set(log);
                        Ok(Storage::Unit.into())
                    }
                    value => Err(interpreter::Error::TypeError {
                        value,
                        ty: Storage::StringType.into(),
                    }),
                }
            }

            fn debug(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                write!(f, "io.print")
            }
        }

        let std = Std::default();

        assert!(
            interpreter::Function::try_from(
                Program::try_from("with std; std.io.print \"Hello, world!\"")
                    .unwrap()
                    .eval()
                    .unwrap(),
            )
            .unwrap()
            .piped(Storage::Borrow(&std).into())
            .eval()
            .unwrap()
            .eq(Storage::Unit.into())
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
