use espy_heart::prelude::*;
use std::rc::Rc;

#[derive(Debug)]
pub enum Error {
    ExpectedBool(Value),
    ExpectedNumber(Value),
    ExpectedNumbers(Value, Value),
    IndexNotFound {
        index: Value,
        container: Value,
    },
    /// Errors that occur due to invalid bytecode.
    ///
    /// If this is emitted due to bytecode from the espyscript compiler,
    /// it should be considered a bug in either program.
    InvalidBytecode(InvalidBytecode),
}

#[derive(Debug)]
pub enum InvalidBytecode {
    /// Caused by an imbalance in stack operations.
    ///
    /// Well-behaved bytecode never has any reason to cause this.
    StackUnderflow,
    /// An instruction's arguments were interrupted by the end of the program.
    MissingInstructionArgument,
    /// An instruction byte had an unexpected value.
    InvalidInstruction,
    /// Occurs when the header is too short or
    /// describes a program which is longer than the provided slice.
    MalformedHeader,
    /// Occurs when the program counter becomes greater than the length of the program.
    ///
    /// Note the "*greater*"; a pc of the program's length
    /// (after the last byte) is considered an intentional return.
    ProgramOutOfBounds,
    /// Occurs when a string id is greater than the number of strings.
    StringOutOfBounds,
    Utf8Error(std::str::Utf8Error),
}

impl From<InvalidBytecode> for Error {
    fn from(e: InvalidBytecode) -> Error {
        Error::InvalidBytecode(e)
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Value {
    pub storage: Storage,
}

// Cloning this type should be cheap;
// every binding usage is a clone in espyscript!
// Use Rcs over boxes and try to put allocations as far up as possible.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Storage {
    Unit,
    I64(i64),
    Bool(bool),
    String(Rc<str>),
    Tuple(Rc<[Storage]>),
    NamedTuple(Rc<[(Rc<str>, Storage)]>),
}

impl From<Storage> for Value {
    fn from(storage: Storage) -> Self {
        Self { storage }
    }
}

#[warn(clippy::panic, clippy::unwrap_used)]
pub fn eval(bytes: &[u8]) -> Result<Value, Error> {
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

    fn read4(bytes: &[u8], at: usize) -> Option<usize> {
        Some(u32::from_le_bytes([
            *bytes.get(at)?,
            *bytes.get(at + 1)?,
            *bytes.get(at + 2)?,
            *bytes.get(at + 3)?,
        ]) as usize)
    }

    // least sustainable approach possible.
    // this is so bad that i think the binary format should change.
    fn with_regions<'bytes>(
        bytes: &'bytes [u8],
        f: impl FnOnce(&'bytes [u8], &'bytes [u8], &'bytes [u8]) -> Result<&'bytes [u8], Error>,
    ) -> Result<&'bytes [u8], Error> {
        let block_count = read4(bytes, 0).ok_or(InvalidBytecode::MalformedHeader)?;
        let blocks = bytes
            .get(4..(block_count * 4 + 4))
            .ok_or(InvalidBytecode::MalformedHeader)?;
        let string_start = block_count * 4 + 4;
        let string_count = read4(bytes, string_start).ok_or(InvalidBytecode::MalformedHeader)?;
        let strings = bytes
            .get((string_start + 4)..(string_count * 4 + string_start + 4))
            .ok_or(InvalidBytecode::MalformedHeader)?;
        let set_start = string_count * 4 + string_start + 4;
        let set_count = read4(bytes, set_start).ok_or(InvalidBytecode::MalformedHeader)?;
        let sets = bytes
            .get((set_start + 4)..(set_count * 4 + set_start + 4))
            .ok_or(InvalidBytecode::MalformedHeader)?;
        f(blocks, strings, sets)
    }

    fn block(bytes: &[u8], block_id: BlockId) -> Result<&[u8], Error> {
        let block_id = block_id as usize;
        with_regions(bytes, |blocks, strings, _sets| {
            let start = read4(blocks, block_id * 4).ok_or(InvalidBytecode::MalformedHeader)?;
            let (next_region, at) = if blocks.len() / 4 == block_id + 1 {
                // we don't need to check sets because if strings is empty so is sets.
                (strings, 0)
            } else {
                (blocks, block_id + 1)
            };
            let end = read4(next_region, at * 4).unwrap_or(bytes.len());
            Ok(bytes
                .get(start..end)
                .ok_or(InvalidBytecode::MalformedHeader)?)
        })
    }

    fn string(bytes: &[u8], string_id: usize) -> Result<&[u8], Error> {
        with_regions(bytes, |_blocks, strings, sets| {
            let start = read4(strings, string_id * 4).ok_or(InvalidBytecode::MalformedHeader)?;
            let (next_region, at) = if strings.len() / 4 == string_id + 1 {
                (sets, 0)
            } else {
                (strings, string_id + 1)
            };
            let end = read4(next_region, at * 4).unwrap_or(bytes.len());
            Ok(bytes
                .get(start..end)
                .ok_or(InvalidBytecode::MalformedHeader)?)
        })
    }

    let mut program = Program {
        bytecode: block(bytes, 0)?,
        pc: 0,
    };
    let mut strings = Vec::<Option<Rc<str>>>::new();
    let mut get_string = |string_id| {
        strings
            .get(string_id)
            .and_then(|x: &Option<Rc<str>>| x.clone())
            .map(Ok::<_, Error>)
            .unwrap_or_else(|| {
                let string = Rc::<str>::from(
                    str::from_utf8(string(bytes, string_id)?)
                        .map_err(InvalidBytecode::Utf8Error)?,
                );
                strings.resize(strings.len().max(string_id + 1), None);
                strings[string_id] = Some(string.clone());
                Ok(string)
            })
    };

    let mut stack = Vec::<Value>::new();
    // The program counter reaching the first (and only the first)
    // out-of-bounds byte should be considered a return.
    while program.pc != program.bytecode.len() {
        macro_rules! bi_op {
            (let $l:ident, $r:ident: $type:ident => $expr_type:ident: $expr:expr) => {{
                let $r = stack.pop().ok_or(InvalidBytecode::StackUnderflow)?;
                let $l = stack.pop().ok_or(InvalidBytecode::StackUnderflow)?;
                match (&$l.storage, &$r.storage) {
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
                stack.push(stack[program.next4()?].clone());
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
            instruction::PUSH_STRING => {
                let string_id = program.next4()?;
                let string = get_string(string_id)?;
                stack.push(Value {
                    storage: Storage::String(string),
                })
            }
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
            instruction::LOGICAL_AND => bi_op!(let l, r: Bool => Bool: *l && *r),
            instruction::LOGICAL_OR => bi_op!(let l, r: Bool => Bool: *l || *r),
            instruction::PIPE => todo!(),

            instruction::CALL => todo!(),
            instruction::TUPLE => {
                fn rc_slice_from_iter<T>(len: usize, iter: impl Iterator<Item = T>) -> Rc<[T]> {
                    let mut tuple = Rc::new_uninit_slice(len);
                    // SAFETY: `get_mut` only returns `None` if the `Rc` has been cloned.
                    let mutable_tuple = unsafe { Rc::get_mut(&mut tuple).unwrap_unchecked() };
                    let count = mutable_tuple
                        .iter_mut()
                        .zip(iter)
                        .map(|(entry, value)| {
                            entry.write(value);
                        })
                        .count();
                    assert!(
                        count == len,
                        "iter did not produce enough values ({count}) to initialize slice of length {len}"
                    );
                    // SAFETY: Since `count` == `len`, the slice is initialized.
                    unsafe { tuple.assume_init() }
                }
                let r = stack.pop().ok_or(InvalidBytecode::StackUnderflow)?;
                let l = stack.pop().ok_or(InvalidBytecode::StackUnderflow)?;
                match (l.storage, r.storage) {
                    (Storage::Tuple(l), Storage::Tuple(r)) => {
                        stack.push(Value {
                            storage: Storage::Tuple(rc_slice_from_iter(
                                l.len() + r.len(),
                                l.iter().chain(r.iter()).cloned(),
                            )),
                        });
                    }
                    (Storage::NamedTuple(l), Storage::NamedTuple(r)) => {
                        stack.push(Value {
                            storage: Storage::NamedTuple(rc_slice_from_iter(
                                l.len() + r.len(),
                                l.iter().chain(r.iter()).cloned(),
                            )),
                        });
                    }
                    (Storage::Tuple(l), r) => {
                        stack.push(Value {
                            storage: Storage::Tuple(rc_slice_from_iter(
                                l.len() + 1,
                                l.iter().cloned().chain(Some(r)),
                            )),
                        });
                    }
                    (l, Storage::Tuple(r)) => {
                        stack.push(Value {
                            storage: Storage::Tuple(rc_slice_from_iter(
                                1 + r.len(),
                                Some(l).into_iter().chain(r.iter().cloned()),
                            )),
                        });
                    }
                    (l, r) => stack.push(Value {
                        storage: Storage::Tuple(Rc::new([l, r])),
                    }),
                }
            }
            instruction::INDEX => {
                let index = stack.pop().ok_or(InvalidBytecode::StackUnderflow)?;
                let container = stack.pop().ok_or(InvalidBytecode::StackUnderflow)?;
                match (&container.storage, &index.storage) {
                    (Storage::Tuple(tuple), Storage::I64(i)) => {
                        stack.push(Value {
                            storage: tuple
                                .get(*i as usize)
                                .cloned()
                                .ok_or(Error::IndexNotFound { index, container })?,
                        });
                    }
                    (Storage::NamedTuple(tuple), Storage::I64(i)) => {
                        stack.push(Value {
                            storage: tuple
                                .get(*i as usize)
                                .map(|(_name, value)| value)
                                .cloned()
                                .ok_or(Error::IndexNotFound { index, container })?,
                        });
                    }
                    (Storage::NamedTuple(tuple), Storage::String(i)) => {
                        stack.push(Value {
                            storage: tuple
                                .iter()
                                .find(|(name, _value)| name == i)
                                .map(|(_name, value)| value)
                                .cloned()
                                .ok_or(Error::IndexNotFound { index, container })?,
                        });
                    }
                    // TODO: NamedTuple indexing by string.
                    (_, _) => return Err(Error::IndexNotFound { index, container }),
                }
            }
            instruction::NAME => {
                let name_id = program.next4()?;
                let name = get_string(name_id)?;
                let value = stack.pop().ok_or(InvalidBytecode::StackUnderflow)?;
                stack.push(Value {
                    storage: Storage::NamedTuple(Rc::new([(name, value.storage)])),
                })
            }
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
