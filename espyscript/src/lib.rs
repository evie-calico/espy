use std::rc::Rc;

pub use espy_ears as parser;
pub use espy_eyes as lexer;
pub use espy_paws as interpreter;
pub use espy_tail as compiler;

pub use interpreter::Value;

pub struct Program {
    bytecode: Rc<[u8]>,
}

impl Program {
    fn eval(&self) -> Result<Value, interpreter::Error> {
        interpreter::Program::try_from(&self.bytecode)?.eval(0, Vec::new())
    }
}

impl<'source> TryFrom<&'source str> for Program {
    type Error = compiler::Error<'source>;

    fn try_from(s: &'source str) -> Result<Self, Self::Error> {
        compiler::Program::try_from(parser::Block::from(&mut lexer::Lexer::from(s).peekable())).map(
            |program| Program {
                bytecode: compiler::Program::compile(program).into(),
            },
        )
    }
}

// TODO: by putting the interpreter tests here i had to make too many things public
#[cfg(test)]
mod tests {
    use interpreter::Storage;
    use std::rc::Rc;

    use super::*;

    #[test]
    fn arithmetic() {
        let actual = Program::try_from("(1 + 2) * 4").unwrap();
        println!("{:?}", actual.bytecode);
        assert_eq!(
            actual.eval().unwrap(),
            Value {
                storage: Storage::I64(12),
            }
        )
    }

    #[test]
    fn tuples() {
        let actual = Program::try_from("let x = 1, 2; let y = 3, 4; x.1 * y.0").unwrap();
        println!("{:?}", actual.bytecode);
        assert_eq!(
            actual.eval().unwrap(),
            Value {
                storage: Storage::I64(6),
            }
        )
    }

    #[test]
    fn named_tuples() {
        let actual = Program::try_from(
            "let x = first: 1, second: 2; let y = third: 3, fourth: 4; x.second * y.0",
        )
        .unwrap();
        println!("{:?}", actual.bytecode);
        assert_eq!(
            actual.eval().unwrap(),
            Value {
                storage: Storage::I64(6),
            }
        )
    }

    #[test]
    fn functions() {
        let actual = Program::try_from("let f = {with x; x * x}; f 4").unwrap();
        println!("{:?}", actual.bytecode);
        assert_eq!(
            actual.eval().unwrap(),
            Value {
                storage: Storage::I64(16),
            }
        )
    }

    #[test]
    fn closures() {
        let actual = Program::try_from("let f = {let y = 10; with x; x * y}; f 4").unwrap();
        println!("{:?}", actual.bytecode);
        assert_eq!(
            actual.eval().unwrap(),
            Value {
                storage: Storage::I64(40),
            }
        )
    }

    #[test]
    fn pipes() {
        let actual = Program::try_from("let f = {with args; args.0 * args.1}; 2 |> f 128").unwrap();
        println!("{:?}", actual.bytecode);
        assert_eq!(
            actual.eval().unwrap(),
            Value {
                storage: Storage::I64(256),
            }
        )
    }

    #[test]
    fn enums_usage() {
        let actual =
            Program::try_from("let Option = enum Some: any, None: () end; Option.Some 1").unwrap();
        println!("{:?}", actual.bytecode);
        assert_eq!(
            actual.eval().unwrap(),
            Value {
                storage: Storage::EnumVariant(Rc::new(interpreter::EnumVariant {
                    contents: Storage::I64(1).into(),
                    variant: 0,
                    definition: Rc::new(interpreter::EnumType {
                        variants: Rc::new([
                            (Rc::from("Some"), Storage::Any.into()),
                            (Rc::from("None"), Storage::Unit.into())
                        ])
                    })
                })),
            }
        )
    }

    #[test]
    fn options() {
        let actual = Program::try_from("Some 1, None ()").unwrap();
        println!("{:?}", actual.bytecode);
        assert_eq!(
            actual.eval().unwrap(),
            Value {
                storage: Storage::Tuple(Rc::new([
                    Storage::Some(Rc::new(Storage::I64(1).into())).into(),
                    Storage::None.into()
                ]))
            }
        )
    }

    #[test]
    fn structures() {
        let actual = Program::try_from("struct x: any, y: any end").unwrap();
        println!("{:?}", actual.bytecode);
        assert_eq!(
            actual.eval().unwrap(),
            Storage::StructType(Rc::new(interpreter::StructType {
                inner: Storage::NamedTuple(Rc::new([
                    (Rc::from("x"), Storage::Any.into()),
                    (Rc::from("y"), Storage::Any.into()),
                ]))
                .into()
            }))
            .into()
        )
    }
}
