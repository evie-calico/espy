use espy_heart::prelude::*;

#[derive(Debug)]
pub enum Error {
    ExpectedBool(Value),
    ExpectedNumber(Value),
    ExpectedNumbers(Value, Value),
    /// Errors that occur due to invalid bytecode.
    ///
    /// If this is emitted due to bytecode from the espyscript compiler,
    /// it should be considered a bug in either program.
    InvalidBytecode(InvalidBytecode),
}

#[derive(Debug)]
pub enum InvalidBytecode {
    /// Caused by an inbalance in stack operations.
    ///
    /// Well-behaved bytecode never has any reason to cause this.
    StackUnderflow,
    /// An instruction's arguments were interrupted by the end of the program.
    MissingInstructionArgument,
    /// An instruction byte had an unexpected value.
    InvalidInstruction,
    /// Occurs when the program counter becomes greater than the length of the program.
    ///
    /// Note the "*greater*"; a pc of the program's length
    /// (after the last byte) is considered an intentional return.
    ProgramOutOfBounds,
}

impl From<InvalidBytecode> for Error {
    fn from(e: InvalidBytecode) -> Error {
        Error::InvalidBytecode(e)
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Value {
    pub storage: Storage,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Storage {
    Unit,
    I64(i64),
    Bool(bool),
}

impl From<Storage> for Value {
    fn from(storage: Storage) -> Self {
        Self { storage }
    }
}

#[warn(clippy::panic, clippy::unwrap_used)]
pub fn eval(bytecode: &[u8]) -> Result<Value, Error> {
    struct Program<'a> {
        bytecode: &'a [u8],
        pc: usize,
    }

    impl Program<'_> {
        fn next(&mut self) -> Result<u8, Error> {
            let next = self
                .bytecode
                .get(self.pc)
                .ok_or(InvalidBytecode::ProgramOutOfBounds)?;
            self.pc += 1;
            Ok(*next)
        }

        fn next4(&mut self) -> Result<usize, Error> {
            Ok(
                u32::from_le_bytes([self.next()?, self.next()?, self.next()?, self.next()?])
                    as usize,
            )
        }

        fn next_i64(&mut self) -> Result<i64, Error> {
            Ok(i64::from_le_bytes([
                self.next()?,
                self.next()?,
                self.next()?,
                self.next()?,
                self.next()?,
                self.next()?,
                self.next()?,
                self.next()?,
            ]))
        }
    }
    let mut program = Program { bytecode, pc: 4 };
    program.pc = program.next4()?;
    let mut stack = Vec::<Value>::new();
    // The program counter reaching the first (and only the first)
    // out-of-bounds byte should be considered a return.
    while program.pc != program.bytecode.len() {
        macro_rules! bi_op {
            (let $l:ident, $r:ident: $type:ident => $expr_type:ident: $expr:expr) => {{
                let $r = stack.pop().ok_or(InvalidBytecode::StackUnderflow)?;
                let $l = stack.pop().ok_or(InvalidBytecode::StackUnderflow)?;
                match ($l.storage, $r.storage) {
                    (Storage::$type($l), Storage::$type($r)) => stack.push(Value {
                        storage: Storage::$expr_type($expr),
                    }),
                    _ => return Err(Error::ExpectedNumbers($l, $r)),
                }
            }};
        }
        macro_rules! bi_num {
            (let $l:ident, $r:ident => $expr:expr) => {
                bi_op!(let $l, $r: I64 => I64: $expr)
            };
        }
        macro_rules! bi_cmp {
            (let $l:ident, $r:ident => $expr:expr) => {
                bi_op!(let $l, $r: I64 => Bool: $expr)
            };
        }
        let instruction = program.next()?;
        match instruction {
            instruction::CLONE => {
                stack.push(stack[program.next4()?]);
            }
            instruction::POP => {
                stack.pop();
            }
            instruction::COLLAPSE => {
                let value = stack.pop().ok_or(InvalidBytecode::StackUnderflow)?;
                for _ in 0..(stack.len() - program.next4()?) {
                    stack.pop();
                }
                stack.push(value);
            }
            instruction::JUMP => {
                program.pc = program.next4()?;
            }
            instruction::IF => {
                let target = program.next4()?;
                if let Value {
                    storage: Storage::Bool(false),
                } = stack.pop().ok_or(InvalidBytecode::StackUnderflow)?
                {
                    program.pc = target;
                }
            }
            // TODO: need function calls and builtins for this.
            instruction::FOR => todo!(),

            instruction::PUSH_UNIT => {
                stack.push(Storage::Unit.into());
            }
            instruction::PUSH_TRUE => {
                stack.push(Storage::Bool(true).into());
            }
            instruction::PUSH_FALSE => {
                stack.push(Storage::Bool(false).into());
            }
            instruction::PUSH_I64 => {
                stack.push(Storage::I64(program.next_i64()?).into());
            }
            instruction::PUSH_STRING => todo!(),
            instruction::PUSH_FUNCTION => todo!(),
            instruction::PUSH_ENUM => todo!(),

            instruction::ADD => bi_num!(let l, r => l + r),
            instruction::SUB => bi_num!(let l, r => l - r),
            instruction::MUL => bi_num!(let l, r => l * r),
            instruction::DIV => bi_num!(let l, r => l / r),
            instruction::BITWISE_AND => bi_num!(let l, r => l & r),
            instruction::BITWISE_OR => bi_num!(let l, r => l | r),
            instruction::BITWISE_XOR => bi_num!(let l, r => l ^ r),
            instruction::GREATER => bi_cmp!(let l, r => l > r),
            instruction::GREATER_EQUAL => bi_cmp!(let l, r => l >= r),
            instruction::LESSER => bi_cmp!(let l, r => l < r),
            instruction::LESSER_EQUAL => bi_cmp!(let l, r => l <= r),
            instruction::EQUAL_TO => {
                let r = stack.pop().ok_or(InvalidBytecode::StackUnderflow)?;
                let l = stack.pop().ok_or(InvalidBytecode::StackUnderflow)?;
                stack.push(Value {
                    storage: Storage::Bool(l.storage == r.storage),
                });
            }
            instruction::NOT_EQUAL_TO => {
                let r = stack.pop().ok_or(InvalidBytecode::StackUnderflow)?;
                let l = stack.pop().ok_or(InvalidBytecode::StackUnderflow)?;
                stack.push(Value {
                    storage: Storage::Bool(l.storage != r.storage),
                });
            }
            instruction::LOGICAL_AND => bi_op!(let l, r: Bool => Bool: l && r),
            instruction::LOGICAL_OR => bi_op!(let l, r: Bool => Bool: l || r),
            instruction::PIPE => todo!(),

            instruction::CALL => todo!(),
            instruction::TUPLE => todo!(),
            instruction::INDEX => todo!(),
            instruction::NAME => todo!(),
            // TODO: This instruction shouldn't be emitted; unary + is a no-op
            instruction::POSITIVE => todo!(),
            instruction::NEGATIVE => {
                let value = stack.pop().ok_or(InvalidBytecode::StackUnderflow)?;
                let Value {
                    storage: Storage::I64(value),
                } = value
                else {
                    return Err(Error::ExpectedNumber(value));
                };
                stack.push(Value {
                    storage: Storage::I64(-value),
                });
            }

            _ => Err(InvalidBytecode::InvalidInstruction)?,
        }
    }
    Ok(stack.pop().ok_or(InvalidBytecode::StackUnderflow)?)
}
