pub use espy_ears as parser;
pub use espy_eyes as lexer;
pub use espy_paws as interpreter;
pub use espy_tail as compiler;

pub use interpreter::Value;

pub struct Program {
    bytecode: Box<[u8]>,
}

impl Program {
    fn eval(&self) -> Result<Value, interpreter::Error> {
        interpreter::Program::try_from(&*self.bytecode)?.eval(0, Vec::new())
    }
}

impl<'source> TryFrom<&'source str> for Program {
    type Error = compiler::Error<'source>;

    fn try_from(s: &'source str) -> Result<Self, Self::Error> {
        compiler::Program::try_from(parser::Block::from(&mut lexer::Lexer::from(s).peekable())).map(
            |program| Program {
                bytecode: compiler::Program::compile(program).into_boxed_slice(),
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
        println!("{:?}", actual.bytecode);
        assert_eq!(
            actual.eval().unwrap(),
            Value {
                storage: interpreter::Storage::I64(12),
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
                storage: interpreter::Storage::I64(6),
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
                storage: interpreter::Storage::I64(6),
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
                storage: interpreter::Storage::I64(16),
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
                storage: interpreter::Storage::I64(40),
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
                storage: interpreter::Storage::I64(256),
            }
        )
    }
}
