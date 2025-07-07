use espy_heart::prelude::*;
use std::{mem, rc::Rc};

#[derive(Debug)]
pub enum Error {
    ExpectedNumbers(Value, Value),
    ExpectedFunction(Value),
    TypeError {
        value: Value,
        ty: Value,
    },
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
    /// Occurs when a stack access goes beyond the length of the stack.
    StackOutOfBounds,
    Utf8Error(std::str::Utf8Error),
}

impl From<InvalidBytecode> for Error {
    fn from(e: InvalidBytecode) -> Error {
        Error::InvalidBytecode(e)
    }
}

type Tuple = [Storage];
type NamedTuple = [(Rc<str>, Storage)];

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Value {
    pub storage: Storage,
}

// Cloning this type should be cheap;
// every binding usage is a clone in espyscript!
// Use Rcs over boxes and try to put allocations as far up as possible.
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub enum Storage {
    /// Unit is a special case of tuple.
    /// It behaves as both an empty tuple and an empty named tuple,
    /// as well as the type of itself (typeof () == ()).
    // TODO: maybe () should be the value and `unit` the type?
    #[default]
    Unit,
    Tuple(Rc<Tuple>),
    NamedTuple(Rc<NamedTuple>),

    I64(i64),
    Bool(bool),
    String(Rc<str>),
    Function(Rc<Function>),
    EnumVariant(Rc<EnumVariant>),
    Some(Rc<Value>),
    None,

    Any,
    I64Type,
    BoolType,
    StringType,
    // TODO: FunctionType
    EnumType(Rc<EnumType>),
    Option,
    /// The type of types.
    Type,
}

impl From<Storage> for Value {
    fn from(storage: Storage) -> Self {
        Self { storage }
    }
}

impl Storage {
    fn type_cmp(&self, ty: &Self) -> bool {
        match (self, ty) {
            (_, Storage::Any) => true,
            (
                Storage::Type
                | Storage::Any
                | Storage::Unit
                | Storage::I64Type
                | Storage::EnumType { .. },
                Storage::Type,
            ) => true,
            (Storage::Unit, Storage::Unit) => true,
            (Storage::I64(_), Storage::I64Type) => true,
            (Storage::Bool(_), Storage::BoolType) => true,
            (Storage::String(_), Storage::StringType) => true,
            (Storage::EnumVariant(variant), Storage::EnumType(ty)) => {
                Rc::ptr_eq(&variant.definition, ty)
            }
            (_, _) => false,
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Function {
    action: FunctionAction,
    arguments: Value,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum FunctionAction {
    Block {
        block_id: usize,
        captures: Vec<Value>,
    },
    Enum {
        variant: usize,
        definition: Rc<EnumType>,
    },
    Some,
    None,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct EnumVariant {
    pub contents: Value,
    pub variant: usize,
    pub definition: Rc<EnumType>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct EnumType {
    pub variants: Vec<(Rc<str>, Value)>,
}

fn read4(bytes: &[u8], at: usize) -> Option<usize> {
    Some(u32::from_le_bytes([
        *bytes.get(at)?,
        *bytes.get(at + 1)?,
        *bytes.get(at + 2)?,
        *bytes.get(at + 3)?,
    ]) as usize)
}

pub struct Program<'bytes> {
    bytes: &'bytes [u8],

    blocks: &'bytes [u8],
    strings: &'bytes [u8],
    sets: &'bytes [u8],

    owned_strings: Vec<Option<Rc<str>>>,
}

impl<'bytes> TryFrom<&'bytes [u8]> for Program<'bytes> {
    type Error = Error;

    fn try_from(bytes: &'bytes [u8]) -> Result<Self, Self::Error> {
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
        Ok(Self {
            bytes,

            blocks,
            strings,
            sets,

            owned_strings: Vec::new(),
        })
    }
}

impl<'bytes> Program<'bytes> {
    fn block(&mut self, block_id: usize) -> Result<&'bytes [u8], Error> {
        let start = read4(self.blocks, block_id * 4).ok_or(InvalidBytecode::MalformedHeader)?;
        let (next_region, at) = if self.blocks.len() / 4 == block_id + 1 {
            // we don't need to check sets because if strings is empty so is sets.
            (self.strings, 0)
        } else {
            (self.blocks, block_id + 1)
        };
        let end = read4(next_region, at * 4).unwrap_or(self.bytes.len());
        Ok(self
            .bytes
            .get(start..end)
            .ok_or(InvalidBytecode::MalformedHeader)?)
    }

    fn string(&mut self, string_id: usize) -> Result<Rc<str>, Error> {
        self.owned_strings
            .get(string_id)
            .and_then(|x: &Option<Rc<str>>| x.clone())
            .map(Ok::<_, Error>)
            .unwrap_or_else(|| {
                let start =
                    read4(self.strings, string_id * 4).ok_or(InvalidBytecode::MalformedHeader)?;
                let (next_region, at) = if self.strings.len() / 4 == string_id + 1 {
                    (self.sets, 0)
                } else {
                    (self.strings, string_id + 1)
                };
                let end = read4(next_region, at * 4).unwrap_or(self.bytes.len());
                let string_bytes = self
                    .bytes
                    .get(start..end)
                    .ok_or(InvalidBytecode::MalformedHeader)?;
                let string = Rc::<str>::from(
                    str::from_utf8(string_bytes).map_err(InvalidBytecode::Utf8Error)?,
                );
                self.owned_strings
                    .resize(self.owned_strings.len().max(string_id + 1), None);
                self.owned_strings[string_id] = Some(string.clone());
                Ok(string)
            })
    }

    fn set(&mut self, set_id: usize) -> Result<&'bytes [u8], Error> {
        let start = read4(self.sets, set_id * 4).ok_or(InvalidBytecode::MalformedHeader)?;
        let end = read4(self.sets, set_id * 4 + 4).unwrap_or(self.bytes.len());
        Ok(self
            .bytes
            .get(start..end)
            .ok_or(InvalidBytecode::MalformedHeader)?)
    }

    pub fn eval(&mut self, block_id: usize, mut stack: Vec<Value>) -> Result<Value, Error> {
        fn concat(l: Value, r: Value) -> Value {
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
            match (l.storage, r.storage) {
                (Storage::Tuple(l), Storage::Tuple(r)) => Value {
                    storage: Storage::Tuple(rc_slice_from_iter(
                        l.len() + r.len(),
                        l.iter().chain(r.iter()).cloned(),
                    )),
                },
                (Storage::NamedTuple(l), Storage::NamedTuple(r)) => Value {
                    storage: Storage::NamedTuple(rc_slice_from_iter(
                        l.len() + r.len(),
                        l.iter().chain(r.iter()).cloned(),
                    )),
                },
                (l, Storage::Unit) => Value { storage: l },
                (Storage::Unit, r) => Value { storage: r },
                (Storage::Tuple(l), r) => Value {
                    storage: Storage::Tuple(rc_slice_from_iter(
                        l.len() + 1,
                        l.iter().cloned().chain(Some(r)),
                    )),
                },
                (l, Storage::Tuple(r)) => Value {
                    storage: Storage::Tuple(rc_slice_from_iter(
                        1 + r.len(),
                        Some(l).into_iter().chain(r.iter().cloned()),
                    )),
                },
                (l, r) => Value {
                    storage: Storage::Tuple(Rc::new([l, r])),
                },
            }
        }

        struct Frame<'a> {
            bytecode: &'a [u8],
            pc: usize,
        }

        impl Frame<'_> {
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

        let mut program = Frame {
            bytecode: self.block(block_id)?,
            pc: 0,
        };

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
                    let index = program.next4()? as i32;
                    match index {
                        0.. => {
                            let value = stack
                                .get(index as usize)
                                .ok_or(InvalidBytecode::StackOutOfBounds)?;
                            stack.push(value.clone());
                        }
                        builtins::ANY => {
                            stack.push(Storage::Any.into());
                        }
                        builtins::I64 => {
                            stack.push(Storage::I64Type.into());
                        }
                        builtins::OPTION => {
                            stack.push(Storage::Option.into());
                        }
                        builtins::SOME => {
                            stack.push(
                                Storage::Function(Rc::new(Function {
                                    action: FunctionAction::Some,
                                    arguments: Storage::Unit.into(),
                                }))
                                .into(),
                            );
                        }
                        builtins::NONE => {
                            stack.push(
                                Storage::Function(Rc::new(Function {
                                    action: FunctionAction::None,
                                    arguments: Storage::Unit.into(),
                                }))
                                .into(),
                            );
                        }
                        _ => {}
                    }
                }
                instruction::POP => {
                    stack.pop().ok_or(InvalidBytecode::StackUnderflow)?;
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
                    let string = self.string(string_id)?;
                    stack.push(Storage::String(string).into());
                }
                instruction::PUSH_FUNCTION => {
                    let captures = program.next4()?;
                    let function = program.next4()?;
                    let new_stack = stack.split_off(stack.len() - captures);
                    stack.push(
                        Storage::Function(Rc::new(Function {
                            action: FunctionAction::Block {
                                block_id: function,
                                captures: new_stack,
                            },
                            // will be ignored by concatenation
                            arguments: Value {
                                storage: Storage::Unit,
                            },
                        }))
                        .into(),
                    );
                }
                instruction::PUSH_ENUM => {
                    let names = self.set(program.next4()?)?;
                    let methods = stack.pop().ok_or(InvalidBytecode::StackUnderflow)?;
                    let mut variants = names
                        .chunks(4)
                        .map(|name| {
                            let name = name
                                .try_into()
                                .map_err(|_| InvalidBytecode::MalformedHeader)?;
                            let name = self.string(u32::from_le_bytes(name) as usize)?;
                            Ok((name, stack.pop().ok_or(InvalidBytecode::StackUnderflow)?))
                        })
                        .collect::<Result<Vec<_>, Error>>()?;
                    variants.reverse();
                    stack.push(Storage::EnumType(Rc::new(EnumType { variants })).into())
                }

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
                    stack.push(Storage::Bool(l.storage == r.storage).into());
                }
                instruction::NOT_EQUAL_TO => {
                    let r = stack.pop().ok_or(InvalidBytecode::StackUnderflow)?;
                    let l = stack.pop().ok_or(InvalidBytecode::StackUnderflow)?;
                    stack.push(Storage::Bool(l.storage != r.storage).into());
                }
                instruction::LOGICAL_AND => bi_op!(let l, r: Bool => Bool: *l && *r),
                instruction::LOGICAL_OR => bi_op!(let l, r: Bool => Bool: *l || *r),
                instruction::PIPE => {
                    let function = stack.pop().ok_or(InvalidBytecode::StackUnderflow)?;
                    let argument = stack.pop().ok_or(InvalidBytecode::StackUnderflow)?;
                    match function.storage {
                        Storage::Function(mut function) => {
                            let function_mut = Rc::make_mut(&mut function);
                            let mut arguments = Value::from(Storage::Unit);
                            mem::swap(&mut arguments, &mut function_mut.arguments);
                            arguments = concat(arguments, argument);
                            mem::swap(&mut arguments, &mut function_mut.arguments);
                            stack.push(Storage::Function(function).into());
                        }
                        _ => return Err(Error::ExpectedFunction(function)),
                    }
                }

                instruction::CALL => {
                    let argument = stack.pop().ok_or(InvalidBytecode::StackUnderflow)?;
                    let function = stack.pop().ok_or(InvalidBytecode::StackUnderflow)?;
                    let Value {
                        storage: Storage::Function(function),
                    } = function
                    else {
                        return Err(Error::ExpectedFunction(function));
                    };
                    let function =
                        Rc::try_unwrap(function).unwrap_or_else(|function| (*function).clone());
                    let result = match function.action {
                        FunctionAction::Block {
                            block_id,
                            mut captures,
                        } => {
                            captures.push(concat(function.arguments, argument));
                            self.eval(block_id, captures)?
                        }
                        FunctionAction::Enum {
                            variant,
                            definition,
                        } => {
                            let ty = &definition
                                .variants
                                .get(variant)
                                .expect("enum variant should be empty")
                                .1;
                            if !argument.storage.type_cmp(&ty.storage) {
                                return Err(Error::TypeError {
                                    value: argument,
                                    ty: ty.clone(),
                                });
                            }
                            Storage::EnumVariant(Rc::new(EnumVariant {
                                contents: argument,
                                variant,
                                definition,
                            }))
                            .into()
                        }
                        FunctionAction::Some => Storage::Some(argument.into()).into(),
                        FunctionAction::None => {
                            if !argument.storage.type_cmp(&Storage::Unit) {
                                return Err(Error::TypeError {
                                    value: argument,
                                    ty: Storage::Unit.into(),
                                });
                            }
                            Storage::None.into()
                        }
                    };
                    stack.push(result);
                }
                instruction::TUPLE => {
                    let r = stack.pop().ok_or(InvalidBytecode::StackUnderflow)?;
                    let l = stack.pop().ok_or(InvalidBytecode::StackUnderflow)?;
                    stack.push(concat(l, r));
                }
                instruction::INDEX => {
                    let index = stack.pop().ok_or(InvalidBytecode::StackUnderflow)?;
                    let container = stack.pop().ok_or(InvalidBytecode::StackUnderflow)?;
                    match (container, index) {
                        (
                            Value {
                                storage: Storage::Tuple(tuple),
                            },
                            Value {
                                storage: Storage::I64(i),
                            },
                        ) => {
                            stack.push(
                                tuple
                                    .get(i as usize)
                                    .cloned()
                                    .ok_or(Error::IndexNotFound {
                                        index: Value {
                                            storage: Storage::Tuple(tuple),
                                        },
                                        container: Value {
                                            storage: Storage::I64(i),
                                        },
                                    })?
                                    .into(),
                            );
                        }
                        (
                            Value {
                                storage: Storage::NamedTuple(tuple),
                            },
                            Value {
                                storage: Storage::I64(i),
                            },
                        ) => {
                            stack.push(
                                tuple
                                    .get(i as usize)
                                    .map(|(_name, value)| value)
                                    .cloned()
                                    .ok_or(Error::IndexNotFound {
                                        index: Value {
                                            storage: Storage::NamedTuple(tuple),
                                        },
                                        container: Value {
                                            storage: Storage::I64(i),
                                        },
                                    })?
                                    .into(),
                            );
                        }
                        (
                            Value {
                                storage: Storage::NamedTuple(tuple),
                            },
                            Value {
                                storage: Storage::String(i),
                            },
                        ) => {
                            stack.push(
                                tuple
                                    .iter()
                                    .find(|(name, _value)| *name == i)
                                    .map(|(_name, value)| value)
                                    .cloned()
                                    .ok_or(Error::IndexNotFound {
                                        index: Value {
                                            storage: Storage::NamedTuple(tuple),
                                        },
                                        container: Value {
                                            storage: Storage::String(i),
                                        },
                                    })?
                                    .into(),
                            );
                        }
                        (
                            Value {
                                storage: Storage::EnumType(ty),
                            },
                            Value {
                                storage: Storage::I64(i),
                            },
                        ) => stack.push(
                            Storage::Function(Rc::new(Function {
                                action: FunctionAction::Enum {
                                    variant: i as usize,
                                    definition: ty,
                                },
                                arguments: Storage::Unit.into(),
                            }))
                            .into(),
                        ),
                        (
                            Value {
                                storage: Storage::EnumType(ty),
                            },
                            Value {
                                storage: Storage::String(name),
                            },
                        ) => {
                            if let Some(variant_id) = ty
                                .variants
                                .iter()
                                .enumerate()
                                .find(|(_, (variant, _))| *variant == name)
                                .map(|(i, _)| i)
                            {
                                stack.push(
                                    Storage::Function(Rc::new(Function {
                                        action: FunctionAction::Enum {
                                            variant: variant_id,
                                            definition: ty,
                                        },
                                        arguments: Storage::Unit.into(),
                                    }))
                                    .into(),
                                );
                            } else {
                                return Err(Error::IndexNotFound {
                                    index: Value {
                                        storage: Storage::EnumType(ty),
                                    },
                                    container: Value {
                                        storage: Storage::String(name),
                                    },
                                });
                            }
                        }
                        (index, container) => {
                            return Err(Error::IndexNotFound { index, container });
                        }
                    }
                }
                instruction::NAME => {
                    let name_id = program.next4()?;
                    let name = self.string(name_id)?;
                    let value = stack.pop().ok_or(InvalidBytecode::StackUnderflow)?;
                    stack.push(Storage::NamedTuple(Rc::new([(name, value.storage)])).into())
                }
                // TODO: This instruction shouldn't be emitted; unary + is a no-op
                instruction::POSITIVE => {}
                instruction::NEGATIVE => {
                    let value = stack.pop().ok_or(InvalidBytecode::StackUnderflow)?;
                    let Value {
                        storage: Storage::I64(value),
                    } = value
                    else {
                        return Err(Error::TypeError {
                            value,
                            ty: Storage::I64Type.into(),
                        });
                    };
                    stack.push(Storage::I64(-value).into());
                }

                _ => Err(InvalidBytecode::InvalidInstruction)?,
            }
        }
        Ok(stack.pop().ok_or(InvalidBytecode::StackUnderflow)?)
    }
}
