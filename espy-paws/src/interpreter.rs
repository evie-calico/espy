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
    pub(crate) bytes: Rc<[u8]>,
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
        stack: &mut Vec<Value<'host>>,
    ) -> Result<Value<'host>, Error<'host>> {
        struct Frame<'a> {
            bytecode: &'a [u8],
            pc: usize,
        }

        impl Frame<'_> {
            fn next(&mut self) -> Result<u8, InvalidBytecode> {
                let next = self
                    .bytecode
                    .get(self.pc)
                    .ok_or(InvalidBytecode::ProgramOutOfBounds)?;
                self.pc += 1;
                Ok(*next)
            }

            fn next4(&mut self) -> Result<usize, InvalidBytecode> {
                Ok(
                    u32::from_le_bytes([self.next()?, self.next()?, self.next()?, self.next()?])
                        as usize,
                )
            }

            fn next_i64(&mut self) -> Result<i64, InvalidBytecode> {
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

            fn pop<'host>(
                // This doesn't use self yet, but i want to include pc in errors eventually.
                &'_ self,
                stack: &mut Vec<Value<'host>>,
            ) -> Result<Value<'host>, Error<'host>> {
                stack.pop().ok_or(InvalidBytecode::StackUnderflow.into())
            }
        }

        let mut program = Frame {
            bytecode: block(&self.bytes, block_id)?,
            pc: 0,
        };

        // The program counter reaching the first (and only the first)
        // out-of-bounds byte should be considered a return.
        while program.pc != program.bytecode.len() {
            macro_rules! bi_op {
                (let $l:ident, $r:ident: $type:ident => $expr_type:ident: $expr:expr) => {{
                    let $r = program.pop(stack)?;
                    let $l = program.pop(stack)?;
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
                            stack.push(Type::Any.into());
                        }
                        builtins::UNIT => {
                            stack.push(Type::Unit.into());
                        }
                        builtins::I64 => {
                            stack.push(Type::I64.into());
                        }
                        builtins::OPTION => {
                            stack.push(
                                Storage::Function(Rc::new(FunctionAction::Option.into())).into(),
                            );
                        }
                        builtins::MUT => {
                            stack.push(
                                Storage::Function(Rc::new(FunctionAction::Mut.into())).into(),
                            );
                        }
                        _ => Err(InvalidBytecode::InvalidBuiltin)?,
                    }
                }
                instruction::POP => {
                    program.pop(stack)?;
                }
                instruction::COLLAPSE => {
                    let value = program.pop(stack)?;
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
                    } = program.pop(stack)?
                    {
                        program.pc = target;
                    }
                }
                // TODO: need function calls and builtins for this.
                instruction::FOR => todo!(),

                instruction::PUSH_UNIT => {
                    stack.push(().into());
                }
                instruction::PUSH_TRUE => {
                    stack.push(true.into());
                }
                instruction::PUSH_FALSE => {
                    stack.push(false.into());
                }
                instruction::PUSH_I64 => {
                    stack.push(program.next_i64()?.into());
                }
                instruction::PUSH_STRING => {
                    let string_id = program.next4()?;
                    let string = self
                        .owned_strings
                        .get(string_id)
                        .ok_or(InvalidBytecode::UnexpectedStringId)?
                        .clone();
                    stack.push(string.into());
                }
                instruction::PUSH_FUNCTION => {
                    let captures = program.next4()?;
                    let function = program.next4()?;
                    let output = program.pop(stack)?;
                    let input = program.pop(stack)?;
                    let new_stack = stack.split_off(stack.len() - captures);
                    stack.push(
                        Storage::Function(Rc::new(
                            FunctionAction::With {
                                program: self.clone(),
                                signature: FunctionType {
                                    input: input.try_into()?,
                                    output: output.try_into()?,
                                },
                                block_id: function,
                                captures: new_stack,
                            }
                            .into(),
                        ))
                        .into(),
                    );
                }
                instruction::PUSH_ENUM => {
                    let variants = program.pop(stack)?;
                    let Value {
                        storage: Storage::Tuple(Tuple(TupleStorage::Named(variants))),
                    } = variants
                    else {
                        Err(Error::ExpectedNamedTuple(variants))?
                    };
                    let variants = rc_slice_try_from_iter(
                        variants.len(),
                        variants.iter().map(|(name, value)| {
                            value.clone().try_into().map(|value| (name.clone(), value))
                        }),
                    )?;
                    stack.push(Type::from(EnumType { variants }).into());
                }
                instruction::PUSH_STRUCT => {
                    let methods = program
                        .pop(stack)?
                        .into_tuple_or_unit()?
                        .map(Tuple::<Function>::try_from)
                        .transpose()?;
                    let set = set(&self.bytes, program.next4()?)?;
                    let mut constructors: Rc<[(Rc<str>, Function<'_>)]> = rc_slice_try_from_iter(
                        set.len() / 4,
                        set.chunks(4).map(|name| {
                            let name = name
                                .try_into()
                                .map_err(|_| InvalidBytecode::MalformedHeader)?;
                            let name = self
                                .owned_strings
                                .get(u32::from_le_bytes(name) as usize)
                                .ok_or(InvalidBytecode::UnexpectedStringId)?
                                .clone();
                            Ok::<_, Error>((
                                name,
                                Rc::unwrap_or_clone(program.pop(stack)?.into_function()?),
                            ))
                        }),
                    )?;
                    // TODO: if set was removed and statics were inlined, this reverse could be done by the compiler.
                    Rc::make_mut(&mut constructors).reverse();
                    let inner = program.pop(stack)?.into_complex_type()?;
                    stack.push(
                        Type::from(StructType {
                            inner,
                            methods,
                            constructors,
                        })
                        .into(),
                    );
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
                    let r = program.pop(stack)?;
                    let l = program.pop(stack)?;
                    stack.push(l.eq(r)?.into());
                }
                instruction::NOT_EQUAL_TO => {
                    let r = program.pop(stack)?;
                    let l = program.pop(stack)?;
                    stack.push((!l.eq(r)?).into());
                }
                instruction::LOGICAL_AND => bi_op!(let l, r: Bool => Bool: *l && *r),
                instruction::LOGICAL_OR => bi_op!(let l, r: Bool => Bool: *l || *r),
                instruction::PIPE => {
                    let function = program.pop(stack)?;
                    let argument = program.pop(stack)?;
                    match function.storage {
                        Storage::Function(mut function) => {
                            let function_mut = Rc::make_mut(&mut function);
                            let mut arguments = ().into();
                            mem::swap(&mut arguments, &mut function_mut.argument);
                            arguments = Value::concat(arguments, argument);
                            mem::swap(&mut arguments, &mut function_mut.argument);
                            stack.push(Storage::Function(function).into());
                        }
                        _ => return Err(Error::ExpectedFunction(function)),
                    }
                }

                instruction::CALL => {
                    let argument = program.pop(stack)?;
                    let function = program.pop(stack)?;
                    let result = match function {
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
                    let r = program.pop(stack)?;
                    let l = program.pop(stack)?;
                    stack.push(Value::concat(l, r));
                }
                instruction::INDEX => {
                    let index = program.pop(stack)?;
                    let container = program.pop(stack)?;
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
                                    index: i.into(),
                                    container: Value {
                                        storage: Storage::Tuple(tuple),
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
                                    index: i.into(),
                                    container: Value {
                                        storage: Storage::Tuple(tuple),
                                    },
                                },
                            )?);
                        }
                        (
                            Value {
                                storage: Storage::Type(Type::Enum(ty)),
                            },
                            Value {
                                storage: Storage::I64(i),
                            },
                        ) => {
                            if (i as usize) < ty.variants.len() {
                                stack.push(
                                    Storage::Function(Rc::new(
                                        FunctionAction::Enum {
                                            variant: i as usize,
                                            definition: ty,
                                        }
                                        .into(),
                                    ))
                                    .into(),
                                )
                            } else {
                                return Err(Error::IndexNotFound {
                                    index: Value {
                                        storage: Storage::Type(Type::Enum(ty)),
                                    },
                                    container: i.into(),
                                });
                            }
                        }
                        (
                            Value {
                                storage: Storage::Type(Type::Enum(ty)),
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
                                .find(|(_, (variant, _))| *variant == name)
                                .map(|(i, _)| i)
                            {
                                stack.push(
                                    Storage::Function(Rc::new(
                                        FunctionAction::Enum {
                                            variant: variant_id,
                                            definition: ty,
                                        }
                                        .into(),
                                    ))
                                    .into(),
                                );
                            } else {
                                return Err(Error::IndexNotFound {
                                    index: Storage::Type(Type::Enum(ty)).into(),
                                    container: name.into(),
                                });
                            }
                        }
                        (
                            Value {
                                storage: Storage::Type(Type::Option(ty)),
                            },
                            Value {
                                storage: Storage::I64(i),
                            },
                        ) => match i {
                            0 => stack.push(
                                Storage::Function(Rc::new(FunctionAction::Some(ty).into())).into(),
                            ),
                            1 => stack.push(
                                Storage::Function(Rc::new(FunctionAction::None(ty).into())).into(),
                            ),
                            _ => {
                                return Err(Error::IndexNotFound {
                                    index: Type::Option(ty).into(),
                                    container: i.into(),
                                });
                            }
                        },
                        (
                            Value {
                                storage: Storage::Type(Type::Option(ty)),
                            },
                            Value {
                                storage: Storage::String(name),
                            },
                        ) => match &*name {
                            "Some" => stack.push(
                                Storage::Function(Rc::new(FunctionAction::Some(ty).into())).into(),
                            ),
                            "None" => stack.push(
                                Storage::Function(Rc::new(FunctionAction::None(ty).into())).into(),
                            ),
                            _ => {
                                return Err(Error::IndexNotFound {
                                    container: Type::Option(ty).into(),
                                    index: name.into(),
                                });
                            }
                        },
                        (
                            Value {
                                storage: Storage::Type(Type::Struct(structure)),
                            },
                            Value {
                                storage: Storage::String(index),
                            },
                        ) => {
                            if let Some(function) = structure
                                .constructors
                                .iter()
                                .find(|(name, _value)| *name == index)
                                .map(|(_name, value)| value)
                            {
                                stack.push(function.clone().as_constructor(structure).into())
                            } else {
                                return Err(Error::IndexNotFound {
                                    index: index.into(),
                                    container: Type::Struct(structure).into(),
                                });
                            }
                        }
                        (
                            Value {
                                storage: Storage::Struct { inner, ty },
                            },
                            Value {
                                storage: Storage::String(index),
                            },
                        ) => {
                            if let Some(method) =
                                ty.methods.as_ref().and_then(|x| x.find_value(&index))
                            {
                                stack.push(method.clone().piped(Rc::unwrap_or_clone(inner)).into());
                            } else {
                                return Err(Error::IndexNotFound {
                                    index: index.into(),
                                    container: Storage::Struct { inner, ty }.into(),
                                });
                            }
                        }
                        (
                            Value {
                                storage: Storage::Borrow(external),
                            },
                            index,
                        ) => stack.push(external.index(index)?),
                        (container, index) => {
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
                    let value = program.pop(stack)?;
                    stack.push(Storage::Tuple(Tuple::from([(name, value)])).into())
                }
                instruction::NEST => {
                    let value = program.pop(stack)?;
                    stack.push(Storage::Tuple(Tuple::from([value])).into())
                }
                instruction::NEGATIVE => {
                    let value = program.pop(stack)?.into_i64()?;
                    stack.push((-value).into());
                }
                instruction::DEREF => {
                    let value = program.pop(stack)?.into_refcell()?;
                    stack.push(value.try_borrow()?.clone());
                }
                instruction::SET => {
                    let value = program.pop(stack)?;
                    let target = program.pop(stack)?.into_refcell()?;
                    *target.borrow_mut() = value;
                }

                _ => Err(InvalidBytecode::InvalidInstruction)?,
            }
        }
        program.pop(stack)
    }
}
