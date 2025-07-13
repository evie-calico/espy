use std::{mem, rc::Rc};

mod interpreter;
pub use interpreter::*;

#[derive(Clone, Debug)]
pub struct Value {
    pub storage: Storage,
}

// Cloning this type should be cheap;
// every binding usage is a clone in espyscript!
// Use Rcs over boxes and try to put allocations as far up as possible.
#[derive(Clone, Default)]
pub enum Storage {
    /// Unit is a special case of tuple.
    /// It behaves as both an empty tuple and an empty named tuple,
    /// as well as the type of itself (typeof () == ()).
    // TODO: maybe () should be the value and `unit` the type?
    #[default]
    Unit,
    Tuple(Tuple),

    Borrow(&'static dyn Extern),
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

impl std::fmt::Debug for Storage {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Storage::Unit => write!(f, "Unit"),
            Storage::Tuple(tuple) => write!(f, "Tuple({tuple:?})"),
            Storage::Borrow(external) => {
                write!(f, "Borrow(")?;
                external.debug(f)?;
                write!(f, ")")
            }
            Storage::I64(i) => write!(f, "I64({i:?})"),
            Storage::Bool(i) => write!(f, "Bool({i:?})"),
            Storage::String(i) => write!(f, "String({i:?})"),
            Storage::Function(function) => write!(f, "Function({function:?})"),
            Storage::EnumVariant(enum_variant) => write!(f, "EnumVariant({enum_variant:?})"),
            Storage::Some(value) => write!(f, "Some({value:?})"),
            Storage::None => write!(f, "None"),
            Storage::Any => write!(f, "Any"),
            Storage::I64Type => write!(f, "I64Type"),
            Storage::BoolType => write!(f, "BoolType"),
            Storage::StringType => write!(f, "StringType"),
            Storage::StructType(struct_type) => write!(f, "StructType({struct_type:?})"),
            Storage::EnumType(enum_type) => write!(f, "EnumType({enum_type:?})"),
            Storage::Option => write!(f, "Option"),
            Storage::Type => write!(f, "Type"),
        }
    }
}

impl From<Storage> for Value {
    fn from(storage: Storage) -> Self {
        Self { storage }
    }
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
                for (l, r) in l.values().zip(r.values()) {
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

    fn type_cmp(&self, ty: &Self) -> bool {
        match (&self.storage, &ty.storage) {
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

    pub fn concat(self, r: Value) -> Value {
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
                storage: Storage::Tuple(Tuple(rc_slice_from_iter(
                    l.len() + r.len(),
                    l.0.iter().chain(r.0.iter()).cloned(),
                ))),
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
                storage: Storage::Tuple(Tuple(rc_slice_from_iter(
                    l.len() + 1,
                    l.0.iter().cloned().chain(Some((None, r))),
                ))),
            },
            (
                l,
                Value {
                    storage: Storage::Tuple(r),
                    ..
                },
            ) => Value {
                storage: Storage::Tuple(Tuple(rc_slice_from_iter(
                    1 + r.len(),
                    Some((None, l)).into_iter().chain(r.0.iter().cloned()),
                ))),
            },
            (l, r) => Value {
                storage: Storage::Tuple(Tuple(Rc::new([(None, l), (None, r)]))),
            },
        }
    }

    pub fn into_tuple(self) -> Result<Tuple, Error> {
        self.try_into()
    }

    pub fn into_tuple_or_unit(self) -> Result<Option<Tuple>, Error> {
        match self.into_tuple() {
            Ok(tuple) => Ok(Some(tuple)),
            Err(Error::ExpectedTuple(Value {
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

impl TryFrom<Value> for Tuple {
    type Error = Error;

    fn try_from(value: Value) -> Result<Self, Self::Error> {
        if let Storage::Tuple(value) = value.storage {
            Ok(value)
        } else {
            Err(Error::ExpectedTuple(value))
        }
    }
}

pub trait Extern {
    fn call(&self, _argument: Value) -> Result<Value, Error> {
        Err(ExternError::MissingFunctionImpl)?
    }

    fn debug(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{{external value}}")
    }
}

impl<F: Fn(Value) -> Result<Value, Error>> Extern for F {
    fn call(&self, argument: Value) -> Result<Value, Error> {
        self(argument)
    }

    fn debug(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{{external function}}")
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

#[derive(Clone, Debug)]
pub struct Tuple(Rc<[(Option<Rc<str>>, Value)]>);

impl Tuple {
    pub fn len(&self) -> usize {
        self.0.len()
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub fn value(&self, index: usize) -> Option<&Value> {
        self.0.get(index).map(|(_name, value)| value)
    }

    pub fn find_value(&self, name: &str) -> Option<&Value> {
        self.0.iter().find_map(|(n, v)| {
            if n.as_ref().is_some_and(|n| **n == *name) {
                Some(v)
            } else {
                None
            }
        })
    }

    pub fn values(&self) -> impl Iterator<Item = &Value> {
        self.0.iter().map(|(_name, value)| value)
    }
}

impl From<(Rc<str>, Value)> for Tuple {
    fn from((name, value): (Rc<str>, Value)) -> Self {
        Self(Rc::new([(Some(name), value)]))
    }
}

impl From<Rc<[(Option<Rc<str>>, Value)]>> for Tuple {
    fn from(value: Rc<[(Option<Rc<str>>, Value)]>) -> Self {
        Self(value)
    }
}

impl<const N: usize> From<[(Option<Rc<str>>, Value); N]> for Tuple {
    fn from(value: [(Option<Rc<str>>, Value); N]) -> Self {
        Self(Rc::from(value))
    }
}

impl AsRef<[(Option<Rc<str>>, Value)]> for Tuple {
    fn as_ref(&self) -> &[(Option<Rc<str>>, Value)] {
        &self.0
    }
}

impl std::ops::Index<usize> for Tuple {
    type Output = (Option<Rc<str>>, Value);

    fn index(&self, index: usize) -> &Self::Output {
        &self.0[index]
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
                let ty = definition
                    .variants
                    .value(variant)
                    .expect("enum variant must not be missing");
                if !self.argument.type_cmp(ty) {
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
                if !self.argument.type_cmp(&Storage::Unit.into()) {
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

    pub fn unwrap(self) -> (Option<Rc<str>>, Value) {
        (
            self.definition.variants[self.variant].0.clone(),
            self.contents,
        )
    }
}

#[derive(Clone, Debug)]
pub struct EnumType {
    pub variants: Tuple,
}

#[derive(Debug)]
pub enum Error {
    ExpectedNumbers(Value, Value),
    ExpectedFunction(Value),
    ExpectedEnumVariant(Value),
    ExpectedEnumType(Value),
    ExpectedStructType(Value),
    ExpectedTuple(Value),
    IncomparableValues(Value, Value),
    TypeError {
        value: Value,
        ty: Value,
    },
    IndexNotFound {
        index: Value,
        container: Value,
    },
    /// Errors that occur during host interop.
    ///
    /// These may carry less information than a typical espyscript error,
    /// or wrap the error type of another crate.
    ExternError(ExternError),
    /// Errors that occur due to invalid bytecode.
    ///
    /// If this is emitted due to bytecode from the espyscript compiler,
    /// it should be considered a bug in either program.
    InvalidBytecode(InvalidBytecode),
}

#[derive(Debug)]
pub enum ExternError {
    MissingFunctionImpl,
    MissingPipeImpl,
    Other(Box<dyn std::error::Error>),
}

impl From<ExternError> for Error {
    fn from(e: ExternError) -> Error {
        Error::ExternError(e)
    }
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
