use crate::*;
use espy_heart::prelude::*;

fn read4(bytes: &[u8], at: usize) -> Option<usize> {
    Some(u32::from_le_bytes([
        *bytes.get(at)?,
        *bytes.get(at + 1)?,
        *bytes.get(at + 2)?,
        *bytes.get(at + 3)?,
    ]) as usize)
}

fn block_count(bytes: &[u8]) -> Result<usize, InvalidBytecode> {
    read4(bytes, 0).ok_or(InvalidBytecode::MalformedHeader)
}

fn string_count(bytes: &[u8]) -> Result<usize, InvalidBytecode> {
    read4(bytes, 4).ok_or(InvalidBytecode::MalformedHeader)
}

fn set_count(bytes: &[u8]) -> Result<usize, InvalidBytecode> {
    read4(bytes, 8).ok_or(InvalidBytecode::MalformedHeader)
}

fn offsets(bytes: &[u8]) -> Result<&[u8], InvalidBytecode> {
    bytes
        .get(12..(12 + 4 * (block_count(bytes)? + string_count(bytes)? + set_count(bytes)?)))
        .ok_or(InvalidBytecode::MalformedHeader)
}

fn block(bytes: &[u8], block_id: usize) -> Result<&[u8], InvalidBytecode> {
    let offsets = offsets(bytes)?;
    let start = read4(offsets, 4 * block_id).ok_or(InvalidBytecode::MalformedHeader)?;
    let end = read4(offsets, 4 * block_id + 4).unwrap_or(bytes.len());
    bytes
        .get(start..end)
        .ok_or(InvalidBytecode::MalformedHeader)
}

fn set(bytes: &[u8], set_id: usize) -> Result<&[u8], InvalidBytecode> {
    let Some(set_id) = set_id.checked_sub(1) else {
        return Ok(&[]);
    };
    let i = set_id + block_count(bytes)? + string_count(bytes)?;
    let offsets = offsets(bytes)?;
    let start = read4(offsets, 4 * i).ok_or(InvalidBytecode::MalformedHeader)?;
    let end = read4(offsets, 4 * i + 4).unwrap_or(bytes.len());
    bytes
        .get(start..end)
        .ok_or(InvalidBytecode::MalformedHeader)
}

#[derive(Clone, Debug)]
pub struct Program {
    bytes: Rc<[u8]>,
    owned_strings: Rc<[Rc<str>]>,
}

impl TryFrom<Rc<[u8]>> for Program {
    type Error = Error<'static>;

    fn try_from(bytes: Rc<[u8]>) -> Result<Self, Self::Error> {
        let string_count = string_count(&bytes)?;
        let owned_strings = (0..string_count)
            .map(|i| {
                let i = i + block_count(&bytes)?;
                let offsets = offsets(&bytes)?;
                let start = read4(offsets, 4 * i).ok_or(InvalidBytecode::MalformedHeader)?;
                let end = read4(offsets, 4 * i + 4).unwrap_or(bytes.len());
                let string_bytes = bytes
                    .get(start..end)
                    .ok_or(InvalidBytecode::MalformedHeader)?;
                let string = str::from_utf8(string_bytes).map_err(InvalidBytecode::Utf8Error)?;
                Ok(Rc::from(string))
            })
            .collect::<Result<_, Error>>()?;
        Ok(Self {
            bytes,
            owned_strings,
        })
    }
}

impl Program {
    pub fn eval<'host>(
        &self,
        block_id: usize,
        mut stack: Vec<Value<'host>>,
    ) -> Result<Value<'host>, Error<'host>> {
        struct Frame<'a> {
            bytecode: &'a [u8],
            pc: usize,
        }

        impl Frame<'_> {
            fn next(&mut self) -> Result<u8, Error<'static>> {
                let next = self
                    .bytecode
                    .get(self.pc)
                    .ok_or(InvalidBytecode::ProgramOutOfBounds)?;
                self.pc += 1;
                Ok(*next)
            }

            fn next4(&mut self) -> Result<usize, Error<'static>> {
                Ok(
                    u32::from_le_bytes([self.next()?, self.next()?, self.next()?, self.next()?])
                        as usize,
                )
            }

            fn next_i64(&mut self) -> Result<i64, Error<'static>> {
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
            bytecode: block(&self.bytes, block_id)?,
            pc: 0,
        };

        // The program counter reaching the first (and only the first)
        //,  out-of-bounds byte should be considered a return.
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
                                    argument: Storage::Unit.into(),
                                }))
                                .into(),
                            );
                        }
                        builtins::NONE => {
                            stack.push(
                                Storage::Function(Rc::new(Function {
                                    action: FunctionAction::None,
                                    argument: Storage::Unit.into(),
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
                    let string = self
                        .owned_strings
                        .get(string_id)
                        .ok_or(InvalidBytecode::UnexpectedStringId)?
                        .clone();
                    stack.push(Storage::String(string).into());
                }
                instruction::PUSH_FUNCTION => {
                    let captures = program.next4()?;
                    let function = program.next4()?;
                    let new_stack = stack.split_off(stack.len() - captures);
                    stack.push(
                        Storage::Function(Rc::new(Function {
                            action: FunctionAction::Block {
                                program: self.clone(),
                                block_id: function,
                                captures: new_stack,
                            },
                            // will be ignored by concatenation
                            argument: Value {
                                storage: Storage::Unit,
                            },
                        }))
                        .into(),
                    );
                }
                instruction::PUSH_ENUM => {
                    let methods = stack
                        .pop()
                        .ok_or(InvalidBytecode::StackUnderflow)?
                        .into_tuple_or_unit()?;
                    let mut statics = set(&self.bytes, program.next4()?)?
                        .chunks(4)
                        .map(|name| {
                            let name = name
                                .try_into()
                                .map_err(|_| InvalidBytecode::MalformedHeader)?;
                            let name = self
                                .owned_strings
                                .get(u32::from_le_bytes(name) as usize)
                                .ok_or(InvalidBytecode::UnexpectedStringId)?
                                .clone();
                            Ok((name, stack.pop().ok_or(InvalidBytecode::StackUnderflow)?))
                        })
                        .collect::<Result<Vec<_>, Error>>()?;
                    statics.reverse();
                    let variants = stack
                        .pop()
                        .ok_or(InvalidBytecode::StackUnderflow)?
                        .into_tuple()?;
                    stack.push(Storage::EnumType(Rc::new(EnumType { variants })).into());
                }
                instruction::PUSH_STRUCT => {
                    let methods = stack
                        .pop()
                        .ok_or(InvalidBytecode::StackUnderflow)?
                        .into_tuple_or_unit()?;
                    let mut statics = set(&self.bytes, program.next4()?)?
                        .chunks(4)
                        .map(|name| {
                            let name = name
                                .try_into()
                                .map_err(|_| InvalidBytecode::MalformedHeader)?;
                            let name = self
                                .owned_strings
                                .get(u32::from_le_bytes(name) as usize)
                                .ok_or(InvalidBytecode::UnexpectedStringId)?
                                .clone();
                            Ok((name, stack.pop().ok_or(InvalidBytecode::StackUnderflow)?))
                        })
                        .collect::<Result<Vec<_>, Error>>()?;
                    statics.reverse();
                    let inner = stack.pop().ok_or(InvalidBytecode::StackUnderflow)?;
                    stack.push(Storage::StructType(Rc::new(StructType { inner })).into());
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
                    stack.push(Storage::Bool(l.eq(r)?).into());
                }
                instruction::NOT_EQUAL_TO => {
                    let r = stack.pop().ok_or(InvalidBytecode::StackUnderflow)?;
                    let l = stack.pop().ok_or(InvalidBytecode::StackUnderflow)?;
                    stack.push(Storage::Bool(!l.eq(r)?).into());
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
                            mem::swap(&mut arguments, &mut function_mut.argument);
                            arguments = Value::concat(arguments, argument);
                            mem::swap(&mut arguments, &mut function_mut.argument);
                            stack.push(Storage::Function(function).into());
                        }
                        _ => return Err(Error::ExpectedFunction(function)),
                    }
                }

                instruction::CALL => {
                    let argument = stack.pop().ok_or(InvalidBytecode::StackUnderflow)?;
                    let function = stack.pop().ok_or(InvalidBytecode::StackUnderflow)?;
                    let result = match function {
                        Value {
                            storage: Storage::Borrow(borrow),
                        } => borrow.call(argument)?,
                        Value {
                            storage: Storage::Function(function),
                        } => Rc::<Function>::try_unwrap(function)
                            .unwrap_or_else(|function| (*function).clone())
                            .piped(argument)
                            .eval()?,
                        function => Err(Error::ExpectedFunction(function))?,
                    };
                    stack.push(result);
                }
                instruction::TUPLE => {
                    let r = stack.pop().ok_or(InvalidBytecode::StackUnderflow)?;
                    let l = stack.pop().ok_or(InvalidBytecode::StackUnderflow)?;
                    stack.push(Value::concat(l, r));
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
                            stack.push(tuple.value(i as usize).cloned().ok_or(
                                Error::IndexNotFound {
                                    index: Value {
                                        storage: Storage::Tuple(tuple),
                                    },
                                    container: Value {
                                        storage: Storage::I64(i),
                                    },
                                },
                            )?);
                        }
                        (
                            Value {
                                storage: Storage::Tuple(tuple),
                            },
                            Value {
                                storage: Storage::String(i),
                            },
                        ) => {
                            stack.push(tuple.find_value(&i).cloned().ok_or(
                                Error::IndexNotFound {
                                    index: Value {
                                        storage: Storage::Tuple(tuple),
                                    },
                                    container: Value {
                                        storage: Storage::String(i),
                                    },
                                },
                            )?);
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
                                argument: Storage::Unit.into(),
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
                                .as_ref()
                                .iter()
                                .enumerate()
                                .find(|(_, (variant, _))| {
                                    variant.as_ref().is_some_and(|variant| *variant == name)
                                })
                                .map(|(i, _)| i)
                            {
                                stack.push(
                                    Storage::Function(Rc::new(Function {
                                        action: FunctionAction::Enum {
                                            variant: variant_id,
                                            definition: ty,
                                        },
                                        argument: Storage::Unit.into(),
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
                    let name = self
                        .owned_strings
                        .get(name_id)
                        .ok_or(InvalidBytecode::UnexpectedStringId)?
                        .clone();
                    let value = stack.pop().ok_or(InvalidBytecode::StackUnderflow)?;
                    stack.push(Storage::Tuple(Tuple::from((name, value))).into())
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
