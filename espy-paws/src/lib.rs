use espy_heart::prelude::*;
use std::{mem, rc::Rc};

#[derive(Debug)]
pub enum Error {
    ExpectedNumbers(Value, Value),
    ExpectedFunction(Value),
    ExpectedEnumVariant(Value),
    ExpectedEnumType(Value),
    ExpectedStructType(Value),
    ExpectedNamedTuple(Value),
    IncomparableValues(Value, Value),
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
    /// An instruction byte had an unexpected value.
    InvalidInstruction,
    /// An instruction referred to a string id that did not exist.
    UnexpectedStringId,
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

type Tuple = [Value];
type NamedTuple = [(Rc<str>, Value)];

#[derive(Clone, Debug)]
pub struct Value {
    pub storage: Storage,
}

impl Value {
    pub fn eq(self, other: Self) -> Result<bool, Error> {
        match (self, other) {
            (
                Value {
                    storage: Storage::Unit,
                },
                Value {
                    storage: Storage::Unit,
                },
            ) => Ok(true),
            (
                Value {
                    storage: Storage::Tuple(l),
                },
                Value {
                    storage: Storage::Tuple(r),
                },
            ) if l.len() == r.len() => {
                for (l, r) in l.iter().zip(r.iter()) {
                    if !l.clone().eq(r.clone())? {
                        return Ok(false);
                    }
                }
                Ok(true)
            }
            (
                Value {
                    storage: Storage::Tuple(l),
                },
                Value {
                    storage: Storage::NamedTuple(r),
                },
            ) if l.len() == r.len() => {
                for (l, (_, r)) in l.iter().zip(r.iter()) {
                    if !l.clone().eq(r.clone())? {
                        return Ok(false);
                    }
                }
                Ok(true)
            }
            (
                Value {
                    storage: Storage::NamedTuple(l),
                },
                Value {
                    storage: Storage::Tuple(r),
                },
            ) if l.len() == r.len() => {
                for ((_, l), r) in l.iter().zip(r.iter()) {
                    if !l.clone().eq(r.clone())? {
                        return Ok(false);
                    }
                }
                Ok(true)
            }
            (
                Value {
                    storage: Storage::NamedTuple(l),
                },
                Value {
                    storage: Storage::NamedTuple(r),
                },
            ) if l.len() == r.len() => {
                for ((_, l), (_, r)) in l.iter().zip(r.iter()) {
                    if !l.clone().eq(r.clone())? {
                        return Ok(false);
                    }
                }
                Ok(true)
            }

            (
                Value {
                    storage: Storage::I64(l),
                },
                Value {
                    storage: Storage::I64(r),
                },
            ) => Ok(l == r),
            (
                Value {
                    storage: Storage::Bool(l),
                },
                Value {
                    storage: Storage::Bool(r),
                },
            ) => Ok(l == r),
            (
                Value {
                    storage: Storage::EnumVariant(l),
                },
                Value {
                    storage: Storage::EnumVariant(r),
                },
            ) => Ok(l.variant == r.variant
                && Rc::ptr_eq(&l.definition, &r.definition)
                && Rc::try_unwrap(l)
                    .map(|l| l.contents)
                    .unwrap_or_else(|l| l.contents.clone())
                    .eq(Rc::try_unwrap(r)
                        .map(|r| r.contents)
                        .unwrap_or_else(|r| r.contents.clone()))?),
            (
                Value {
                    storage: Storage::Some(l),
                },
                Value {
                    storage: Storage::Some(r),
                },
            ) => Rc::unwrap_or_clone(l).eq(Rc::unwrap_or_clone(r)),
            (
                Value {
                    storage: Storage::None,
                },
                Value {
                    storage: Storage::None,
                },
            ) => Ok(true),

            (
                Value {
                    storage: Storage::Any,
                },
                Value {
                    storage: Storage::Any,
                },
            ) => Ok(true),
            (
                Value {
                    storage: Storage::I64Type,
                },
                Value {
                    storage: Storage::I64Type,
                },
            ) => Ok(true),
            (
                Value {
                    storage: Storage::BoolType,
                },
                Value {
                    storage: Storage::BoolType,
                },
            ) => Ok(true),
            (
                Value {
                    storage: Storage::StringType,
                },
                Value {
                    storage: Storage::StringType,
                },
            ) => Ok(true),
            (
                Value {
                    storage: Storage::StructType(l),
                },
                Value {
                    storage: Storage::StructType(r),
                },
            ) => Ok(Rc::ptr_eq(&l, &r)),
            (
                Value {
                    storage: Storage::EnumType(l),
                },
                Value {
                    storage: Storage::EnumType(r),
                },
            ) => Ok(Rc::ptr_eq(&l, &r)),
            (
                Value {
                    storage: Storage::Option,
                },
                Value {
                    storage: Storage::Option,
                },
            ) => Ok(true),
            (
                Value {
                    storage: Storage::Type,
                },
                Value {
                    storage: Storage::Type,
                },
            ) => Ok(true),
            (this, other) => Err(Error::IncomparableValues(this, other)),
        }
    }

    fn concat(self, r: Value) -> Value {
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
        match (self, r) {
            (
                Value {
                    storage: Storage::Tuple(l),
                    ..
                },
                Value {
                    storage: Storage::Tuple(r),
                    ..
                },
            ) => Value {
                storage: Storage::Tuple(rc_slice_from_iter(
                    l.len() + r.len(),
                    l.iter().chain(r.iter()).cloned(),
                )),
            },
            (
                Value {
                    storage: Storage::NamedTuple(l),
                    ..
                },
                Value {
                    storage: Storage::NamedTuple(r),
                    ..
                },
            ) => Value {
                storage: Storage::NamedTuple(rc_slice_from_iter(
                    l.len() + r.len(),
                    l.iter().chain(r.iter()).cloned(),
                )),
            },
            (
                l,
                Value {
                    storage: Storage::Unit,
                    ..
                },
            ) => Value { storage: l.storage },
            (
                Value {
                    storage: Storage::Unit,
                    ..
                },
                r,
            ) => Value { storage: r.storage },
            (
                Value {
                    storage: Storage::Tuple(l),
                    ..
                },
                r,
            ) => Value {
                storage: Storage::Tuple(rc_slice_from_iter(
                    l.len() + 1,
                    l.iter().cloned().chain(Some(r)),
                )),
            },
            (
                l,
                Value {
                    storage: Storage::Tuple(r),
                    ..
                },
            ) => Value {
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

    pub fn into_named_tuple(self) -> Result<Rc<NamedTuple>, Error> {
        self.try_into()
    }

    pub fn into_named_tuple_or_unit(self) -> Result<Option<Rc<NamedTuple>>, Error> {
        match self.into_named_tuple() {
            Ok(tuple) => Ok(Some(tuple)),
            Err(Error::ExpectedNamedTuple(Value {
                storage: Storage::Unit,
            })) => Ok(None),
            Err(e) => Err(e),
        }
    }

    pub fn into_function(self) -> Result<Rc<Function>, Error> {
        self.try_into()
    }

    pub fn into_enum_variant(self) -> Result<Rc<EnumVariant>, Error> {
        self.try_into()
    }

    pub fn into_enum_type(self) -> Result<Rc<EnumType>, Error> {
        self.try_into()
    }

    pub fn into_struct_type(self) -> Result<Rc<StructType>, Error> {
        self.try_into()
    }
}

impl TryFrom<Value> for Rc<NamedTuple> {
    type Error = Error;

    fn try_from(value: Value) -> Result<Self, Self::Error> {
        if let Storage::NamedTuple(value) = value.storage {
            Ok(value)
        } else {
            Err(Error::ExpectedNamedTuple(value))
        }
    }
}

impl TryFrom<Value> for Rc<Function> {
    type Error = Error;
    fn try_from(value: Value) -> Result<Self, Self::Error> {
        if let Value {
            storage: Storage::Function(value),
        } = value
        {
            Ok(value)
        } else {
            Err(Error::ExpectedFunction(value))
        }
    }
}

impl TryFrom<Value> for Function {
    type Error = Error;

    fn try_from(value: Value) -> Result<Self, Self::Error> {
        Ok(Rc::unwrap_or_clone(Rc::<Self>::try_from(value)?))
    }
}

impl TryFrom<Value> for Rc<EnumVariant> {
    type Error = Error;
    fn try_from(value: Value) -> Result<Self, Self::Error> {
        if let Value {
            storage: Storage::EnumVariant(value),
        } = value
        {
            Ok(value)
        } else {
            Err(Error::ExpectedEnumVariant(value))
        }
    }
}

impl TryFrom<Value> for EnumVariant {
    type Error = Error;

    fn try_from(value: Value) -> Result<Self, Self::Error> {
        Ok(Rc::unwrap_or_clone(Rc::<Self>::try_from(value)?))
    }
}

impl TryFrom<Value> for Rc<EnumType> {
    type Error = Error;
    fn try_from(value: Value) -> Result<Self, Self::Error> {
        if let Value {
            storage: Storage::EnumType(value),
        } = value
        {
            Ok(value)
        } else {
            Err(Error::ExpectedEnumType(value))
        }
    }
}

impl TryFrom<Value> for EnumType {
    type Error = Error;

    fn try_from(value: Value) -> Result<Self, Self::Error> {
        Ok(Rc::unwrap_or_clone(Rc::<Self>::try_from(value)?))
    }
}

impl TryFrom<Value> for Rc<StructType> {
    type Error = Error;
    fn try_from(value: Value) -> Result<Self, Self::Error> {
        if let Value {
            storage: Storage::StructType(value),
        } = value
        {
            Ok(value)
        } else {
            Err(Error::ExpectedStructType(value))
        }
    }
}
impl TryFrom<Value> for StructType {
    type Error = Error;

    fn try_from(value: Value) -> Result<Self, Self::Error> {
        Ok(Rc::unwrap_or_clone(Rc::<Self>::try_from(value)?))
    }
}

// Cloning this type should be cheap;
// every binding usage is a clone in espyscript!
// Use Rcs over boxes and try to put allocations as far up as possible.
#[derive(Clone, Debug, Default)]
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
    StructType(Rc<StructType>),
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

#[derive(Clone, Debug)]
pub struct Function {
    action: FunctionAction,
    argument: Value,
}

impl Function {
    pub fn eval(self) -> Result<Value, Error> {
        Ok(match self.action {
            FunctionAction::Block {
                program,
                block_id,
                mut captures,
            } => {
                captures.push(self.argument);
                program.eval(block_id, captures)?
            }
            FunctionAction::Enum {
                variant,
                definition,
            } => {
                let ty = &definition
                    .variants
                    .get(variant)
                    .expect("enum variant must not be missing")
                    .1;
                if !self.argument.storage.type_cmp(&ty.storage) {
                    return Err(Error::TypeError {
                        value: self.argument,
                        ty: ty.clone(),
                    });
                }
                Storage::EnumVariant(Rc::new(EnumVariant {
                    contents: self.argument,
                    variant,
                    definition,
                }))
                .into()
            }
            FunctionAction::Some => Storage::Some(self.argument.into()).into(),
            FunctionAction::None => {
                if !self.argument.storage.type_cmp(&Storage::Unit) {
                    return Err(Error::TypeError {
                        value: self.argument,
                        ty: Storage::Unit.into(),
                    });
                }
                Storage::None.into()
            }
        })
    }

    /// Concatentes the function's argument list with `argument`.
    pub fn pipe(&mut self, argument: Value) {
        let mut arguments = Value::from(Storage::Unit);
        mem::swap(&mut arguments, &mut self.argument);
        arguments = Value::concat(arguments, argument);
        mem::swap(&mut arguments, &mut self.argument);
    }

    pub fn piped(mut self, argument: Value) -> Self {
        self.pipe(argument);
        self
    }
}

#[derive(Clone, Debug)]
pub enum FunctionAction {
    Block {
        program: Program,
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

#[derive(Clone, Debug)]
pub struct StructType {
    pub inner: Value,
}

#[derive(Clone, Debug)]
pub struct EnumVariant {
    pub contents: Value,
    pub variant: usize,
    pub definition: Rc<EnumType>,
}

impl EnumVariant {
    pub fn contents(&self) -> &Value {
        &self.contents
    }

    pub fn definition(&self) -> &Rc<EnumType> {
        &self.definition
    }

    pub fn variant(&self) -> usize {
        self.variant
    }

    pub fn unwrap(self) -> (Rc<str>, Value) {
        (
            self.definition.variants[self.variant].0.clone(),
            self.contents,
        )
    }
}

#[derive(Clone, Debug)]
pub struct EnumType {
    pub variants: Rc<NamedTuple>,
}

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

fn block(bytes: &[u8], block_id: usize) -> Result<&[u8], Error> {
    let offsets = offsets(bytes)?;
    let start = read4(offsets, 4 * block_id).ok_or(InvalidBytecode::MalformedHeader)?;
    let end = read4(offsets, 4 * block_id + 4).unwrap_or(bytes.len());
    Ok(bytes
        .get(start..end)
        .ok_or(InvalidBytecode::MalformedHeader)?)
}

fn set(bytes: &[u8], set_id: usize) -> Result<&[u8], Error> {
    let Some(set_id) = set_id.checked_sub(1) else {
        return Ok(&[]);
    };
    let i = set_id + block_count(bytes)? + string_count(bytes)?;
    let offsets = offsets(bytes)?;
    let start = read4(offsets, 4 * i).ok_or(InvalidBytecode::MalformedHeader)?;
    let end = read4(offsets, 4 * i + 4).unwrap_or(bytes.len());
    Ok(bytes
        .get(start..end)
        .ok_or(InvalidBytecode::MalformedHeader)?)
}

#[derive(Clone, Debug)]
pub struct Program {
    bytes: Rc<[u8]>,
    owned_strings: Rc<[Rc<str>]>,
}

impl TryFrom<Rc<[u8]>> for Program {
    type Error = Error;

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
    pub fn eval(&self, block_id: usize, mut stack: Vec<Value>) -> Result<Value, Error> {
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
                        .into_named_tuple_or_unit()?;
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
                        .into_named_tuple()?;
                    stack.push(Storage::EnumType(Rc::new(EnumType { variants })).into());
                }
                instruction::PUSH_STRUCT => {
                    let methods = stack
                        .pop()
                        .ok_or(InvalidBytecode::StackUnderflow)?
                        .into_named_tuple_or_unit()?;
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
                    stack.push(
                        Rc::<Function>::try_unwrap(function.try_into()?)
                            .unwrap_or_else(|function| (*function).clone())
                            .piped(argument)
                            .eval()?,
                    );
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
                            stack.push(tuple.get(i as usize).cloned().ok_or(
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
                                    })?,
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
                                    })?,
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
                    stack.push(Storage::NamedTuple(Rc::new([(name, value)])).into())
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
