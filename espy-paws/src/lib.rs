use std::{mem, rc::Rc};

mod interpreter;
pub use interpreter::*;
mod interop;
pub use interop::{Extern, ExternCell, ExternMut, function, function_mut};

fn rc_slice_try_from_iter<T, E>(
    len: usize,
    iter: impl Iterator<Item = Result<T, E>>,
) -> Result<Rc<[T]>, E> {
    let mut tuple = Rc::new_uninit_slice(len);
    // SAFETY: `get_mut` only returns `None` if the `Rc` has been cloned.
    let mutable_tuple = unsafe { Rc::get_mut(&mut tuple).unwrap_unchecked() };
    let mut count = 0;
    for (entry, value) in mutable_tuple.iter_mut().zip(iter) {
        entry.write(value?);
        count += 1;
    }
    assert!(
        count == len,
        "iter did not produce enough values ({count}) to initialize slice of length {len}"
    );
    // SAFETY: Since `count` == `len`, the slice is initialized.
    unsafe { Ok(tuple.assume_init()) }
}

fn rc_slice_from_iter<T>(len: usize, iter: impl Iterator<Item = T>) -> Rc<[T]> {
    rc_slice_try_from_iter(len, iter.map(Ok::<_, ()>)).expect("iter is always Ok")
}

#[derive(Clone, Debug)]
pub struct Value<'host> {
    pub storage: Storage<'host>,
}

// Cloning this type should be cheap;
// every binding usage is a clone in espyscript!
// Use Rcs over boxes and try to put allocations as far up as possible.
#[derive(Clone)]
pub enum Storage<'host> {
    /// Unit is the absense of a value.
    /// It can be thought of as both an empty tuple and an empty named tuple.
    Unit,
    Tuple(Tuple<Value<'host>>),

    Borrow(&'host dyn Extern),
    I64(i64),
    Bool(bool),
    String(Rc<str>),
    Function(Rc<Function<'host>>),
    EnumVariant(Rc<EnumVariant<'host>>),
    Option {
        contents: Option<Rc<Value<'host>>>,
        ty: Rc<ComplexType>,
    },

    Type(Type),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Type {
    /// Any is the type of a value with unknown capabilities.
    ///
    /// Any may be downcasted to another type or trait object to observe its properties,
    /// but a value of type Any otherwise has no functionality.
    ///
    /// Any is the type of all external values because they have arbitrary, unknowable properties.
    /// This means that external values must be casted to trait objects to interact with them.
    Any,
    I64,
    Bool,
    String,
    // TODO: FunctionType
    Struct(Rc<StructType>),
    Enum(Rc<EnumType>),
    Option(Rc<ComplexType>),

    /// The type of types.
    Type,
    Unit,
}

/// ComplexType is usually the only form of [`Type`] that the espyscript interpreter is concerned with,
/// but it cannot be represented within espyscript itself (tuples of types represent Complex, instead).
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ComplexType {
    Simple(Type),
    Complex(Tuple<ComplexType>),
}

impl From<Type> for ComplexType {
    fn from(value: Type) -> Self {
        Self::Simple(value)
    }
}

impl From<Tuple<ComplexType>> for ComplexType {
    fn from(value: Tuple<ComplexType>) -> Self {
        Self::Complex(value)
    }
}

impl std::fmt::Debug for Storage<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Storage::")?;
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
            Storage::Option { contents, ty: _ } => write!(f, "{contents:?}"),
            Storage::Type(t) => write!(f, "{t:?}"),
        }
    }
}

impl<'host> From<Storage<'host>> for Value<'host> {
    fn from(storage: Storage<'host>) -> Self {
        Self { storage }
    }
}

impl<'host> From<Type> for Value<'host> {
    fn from(t: Type) -> Self {
        Self { storage: t.into() }
    }
}

impl<'host> From<Type> for Storage<'host> {
    fn from(t: Type) -> Self {
        Self::Type(t)
    }
}

impl<'host> Value<'host> {
    pub fn eq(self, other: Self) -> Result<bool, Error<'host>> {
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
                    storage:
                        Storage::Option {
                            contents: l,
                            ty: l_type,
                        },
                },
                Value {
                    storage:
                        Storage::Option {
                            contents: r,
                            ty: r_type,
                        },
                },
            ) => {
                if l_type == r_type {
                    Ok(l.is_none() && r.is_none()
                        || l.zip(r)
                            .map(|(l, r)| Rc::unwrap_or_clone(l).eq(Rc::unwrap_or_clone(r)))
                            .unwrap_or(Ok(false))?)
                } else {
                    Err(Error::IncomparableValues(
                        Storage::Option {
                            contents: l,
                            ty: l_type,
                        }
                        .into(),
                        Storage::Option {
                            contents: r,
                            ty: r_type,
                        }
                        .into(),
                    ))
                }
            }
            (
                Value {
                    storage: Storage::Type(l),
                },
                Value {
                    storage: Storage::Type(r),
                },
            ) => Ok(l == r),
            (this, other) => Err(Error::IncomparableValues(this, other)),
        }
    }

    fn type_of(&self) -> ComplexType {
        match &self.storage {
            Storage::Unit => ComplexType::Simple(Type::Unit),
            Storage::Tuple(tuple) => {
                let complex = rc_slice_from_iter(
                    tuple.len(),
                    tuple
                        .0
                        .iter()
                        .map(|(name, value)| (name.clone(), value.type_of())),
                );
                // The type of a tuple containing only types is `type`, not `(type, type, ..)`
                if complex
                    .iter()
                    .all(|x| matches!(x.1, ComplexType::Simple(Type::Type)))
                {
                    Type::Type.into()
                } else {
                    Tuple(complex).into()
                }
            }
            Storage::Borrow(_) => ComplexType::Simple(Type::Any),
            Storage::I64(_) => ComplexType::Simple(Type::I64),
            Storage::Bool(_) => ComplexType::Simple(Type::Bool),
            Storage::String(_) => ComplexType::Simple(Type::String),
            Storage::Function(function) => todo!(),
            Storage::EnumVariant(enum_variant) => {
                ComplexType::Simple(Type::Enum(enum_variant.definition.clone()))
            }
            Storage::Option { contents: _, ty } => ComplexType::Simple(Type::Option(ty.clone())),
            Storage::Type(_) => ComplexType::Simple(Type::Type),
        }
    }

    pub fn concat(self, r: Self) -> Self {
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

    pub fn into_unit(self) -> Result<(), Error<'host>> {
        self.try_into()
    }

    pub fn into_tuple(self) -> Result<Tuple<Value<'host>>, Error<'host>> {
        self.try_into()
    }

    pub fn into_tuple_or_unit(self) -> Result<Option<Tuple<Value<'host>>>, Error<'host>> {
        match self.into_tuple() {
            Ok(tuple) => Ok(Some(tuple)),
            Err(Error::ExpectedTuple(Value {
                storage: Storage::Unit,
            })) => Ok(None),
            Err(e) => Err(e),
        }
    }

    pub fn into_i64(self) -> Result<i64, Error<'host>> {
        self.try_into()
    }

    pub fn into_str(self) -> Result<Rc<str>, Error<'host>> {
        self.try_into()
    }

    pub fn as_str(&self) -> Option<&str> {
        if let Storage::String(s) = &self.storage {
            Some(s)
        } else {
            None
        }
    }

    pub fn borrow(external: &'host dyn Extern) -> Self {
        Storage::Borrow(external).into()
    }

    pub fn into_function(self) -> Result<Rc<Function<'host>>, Error<'host>> {
        self.try_into()
    }

    pub fn into_enum_variant(self) -> Result<Rc<EnumVariant<'host>>, Error<'host>> {
        self.try_into()
    }

    pub fn into_complex_type(self) -> Result<ComplexType, Error<'host>> {
        self.try_into()
    }

    pub fn into_enum_type(self) -> Result<Rc<EnumType>, Error<'host>> {
        self.try_into()
    }

    pub fn into_struct_type(self) -> Result<Rc<StructType>, Error<'host>> {
        self.try_into()
    }
}

impl From<()> for Value<'_> {
    fn from(_: ()) -> Self {
        Storage::Unit.into()
    }
}

impl From<Option<()>> for Value<'_> {
    fn from(value: Option<()>) -> Self {
        Storage::Option {
            contents: value.map(|()| Rc::new(Storage::Unit.into())),
            ty: Rc::new(Type::Unit.into()),
        }
        .into()
    }
}

impl From<bool> for Value<'_> {
    fn from(value: bool) -> Self {
        Storage::Bool(value).into()
    }
}

impl From<Option<bool>> for Value<'_> {
    fn from(value: Option<bool>) -> Self {
        Storage::Option {
            contents: value.map(|value| Rc::new(Storage::Bool(value).into())),
            ty: Rc::new(Type::Bool.into()),
        }
        .into()
    }
}

impl From<i64> for Value<'_> {
    fn from(i: i64) -> Self {
        Storage::I64(i).into()
    }
}

impl From<Option<i64>> for Value<'_> {
    fn from(value: Option<i64>) -> Self {
        Storage::Option {
            contents: value.map(|value| Rc::new(Storage::I64(value).into())),
            ty: Rc::new(Type::I64.into()),
        }
        .into()
    }
}

impl From<Rc<str>> for Value<'_> {
    fn from(s: Rc<str>) -> Self {
        Storage::String(s).into()
    }
}

impl From<Option<Rc<str>>> for Value<'_> {
    fn from(value: Option<Rc<str>>) -> Self {
        Storage::Option {
            contents: value.map(|value| Rc::new(Storage::String(value).into())),
            ty: Rc::new(Type::String.into()),
        }
        .into()
    }
}

impl<'host> TryFrom<Value<'host>> for () {
    type Error = Error<'host>;

    fn try_from(value: Value<'host>) -> Result<Self, Self::Error> {
        if let Storage::Unit = value.storage {
            Ok(())
        } else {
            Err(Error::type_error(value, Type::I64))
        }
    }
}

impl<'host> TryFrom<Value<'host>> for Tuple<Value<'host>> {
    type Error = Error<'host>;

    fn try_from(value: Value<'host>) -> Result<Self, Self::Error> {
        if let Storage::Tuple(value) = value.storage {
            Ok(value)
        } else {
            Err(Error::ExpectedTuple(value))
        }
    }
}

impl<'host> TryFrom<Value<'host>> for i64 {
    type Error = Error<'host>;

    fn try_from(value: Value<'host>) -> Result<Self, Self::Error> {
        if let Storage::I64(value) = value.storage {
            Ok(value)
        } else {
            Err(Error::type_error(value, Type::I64))
        }
    }
}

impl<'host> TryFrom<Value<'host>> for Rc<str> {
    type Error = Error<'host>;

    fn try_from(value: Value<'host>) -> Result<Self, Self::Error> {
        if let Storage::String(value) = value.storage {
            Ok(value)
        } else {
            Err(Error::type_error(value, Type::String))
        }
    }
}

impl<'host> TryFrom<Value<'host>> for Rc<Function<'host>> {
    type Error = Error<'host>;
    fn try_from(value: Value<'host>) -> Result<Self, Self::Error> {
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

impl<'host> TryFrom<Value<'host>> for Function<'host> {
    type Error = Error<'host>;

    fn try_from(value: Value<'host>) -> Result<Self, Self::Error> {
        Ok(Rc::unwrap_or_clone(Rc::<Self>::try_from(value)?))
    }
}

impl<'host> TryFrom<Value<'host>> for Rc<EnumVariant<'host>> {
    type Error = Error<'host>;
    fn try_from(value: Value<'host>) -> Result<Self, Self::Error> {
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

impl<'host> TryFrom<Value<'host>> for EnumVariant<'host> {
    type Error = Error<'host>;

    fn try_from(value: Value<'host>) -> Result<Self, Self::Error> {
        Ok(Rc::unwrap_or_clone(Rc::<Self>::try_from(value)?))
    }
}

impl<'host> TryFrom<Value<'host>> for ComplexType {
    type Error = Error<'host>;

    fn try_from(value: Value<'host>) -> Result<Self, Self::Error> {
        match value {
            Value {
                storage: Storage::Type(t),
            } => Ok(t.into()),
            Value {
                storage: Storage::Tuple(tuple),
            } => Ok(ComplexType::Complex(tuple.try_into()?)),
            _ => Err(Error::type_error(value, Type::Type)),
        }
    }
}

impl<'host> TryFrom<Value<'host>> for Rc<EnumType> {
    type Error = Error<'host>;
    fn try_from(value: Value<'host>) -> Result<Self, Self::Error> {
        if let Value {
            storage: Storage::Type(Type::Enum(value)),
        } = value
        {
            Ok(value)
        } else {
            Err(Error::ExpectedEnumType(value))
        }
    }
}

impl<'host> TryFrom<Value<'host>> for EnumType {
    type Error = Error<'host>;

    fn try_from(value: Value<'host>) -> Result<Self, Self::Error> {
        Ok(Rc::unwrap_or_clone(Rc::<Self>::try_from(value)?))
    }
}

impl<'host> TryFrom<Value<'host>> for Rc<StructType> {
    type Error = Error<'host>;
    fn try_from(value: Value<'host>) -> Result<Self, Self::Error> {
        if let Value {
            storage: Storage::Type(Type::Struct(value)),
        } = value
        {
            Ok(value)
        } else {
            Err(Error::ExpectedStructType(value))
        }
    }
}

impl<'host> TryFrom<Value<'host>> for StructType {
    type Error = Error<'host>;

    fn try_from(value: Value<'host>) -> Result<Self, Self::Error> {
        Ok(Rc::unwrap_or_clone(Rc::<Self>::try_from(value)?))
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Tuple<T>(Rc<[(Option<Rc<str>>, T)]>);

impl<T> Tuple<T> {
    pub fn len(&self) -> usize {
        self.0.len()
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub fn value(&self, index: usize) -> Option<&T> {
        self.0.get(index).map(|(_name, value)| value)
    }

    pub fn find_value(&self, name: &str) -> Option<&T> {
        self.0.iter().find_map(|(n, v)| {
            if n.as_ref().is_some_and(|n| **n == *name) {
                Some(v)
            } else {
                None
            }
        })
    }

    pub fn values(&self) -> impl Iterator<Item = &T> {
        self.0.iter().map(|(_name, value)| value)
    }
}

impl<'host> TryFrom<Tuple<Value<'host>>> for Tuple<ComplexType> {
    type Error = Error<'host>;

    fn try_from(tuple: Tuple<Value<'host>>) -> Result<Self, Self::Error> {
        rc_slice_try_from_iter(
            tuple.len(),
            tuple
                .0
                .iter()
                .map(|(name, value)| Ok((name.clone(), value.clone().try_into()?))),
        )
        .map(Tuple)
    }
}

impl<T> From<(Rc<str>, T)> for Tuple<T> {
    fn from((name, value): (Rc<str>, T)) -> Self {
        Self(Rc::new([(Some(name), value)]))
    }
}

impl<T> From<Rc<[(Option<Rc<str>>, T)]>> for Tuple<T> {
    fn from(value: Rc<[(Option<Rc<str>>, T)]>) -> Self {
        Self(value)
    }
}

impl<T, const N: usize> From<[(Option<Rc<str>>, T); N]> for Tuple<T> {
    fn from(value: [(Option<Rc<str>>, T); N]) -> Self {
        Self(Rc::from(value))
    }
}

impl<T> AsRef<[(Option<Rc<str>>, T)]> for Tuple<T> {
    fn as_ref(&self) -> &[(Option<Rc<str>>, T)] {
        &self.0
    }
}

impl<T> std::ops::Index<usize> for Tuple<T> {
    type Output = (Option<Rc<str>>, T);

    fn index(&self, index: usize) -> &Self::Output {
        &self.0[index]
    }
}

#[derive(Clone, Debug)]
pub struct Function<'host> {
    action: FunctionAction<'host>,
    argument: Value<'host>,
}

impl<'host> Function<'host> {
    pub fn eval(self) -> Result<Value<'host>, Error<'host>> {
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
                if *ty != Type::Any.into() && self.argument.type_of() != *ty {
                    return Err(Error::type_error(self.argument, ty.clone()));
                }
                Storage::EnumVariant(Rc::new(EnumVariant {
                    contents: self.argument,
                    variant,
                    definition,
                }))
                .into()
            }
            FunctionAction::Option => {
                Type::Option(Rc::new(self.argument.into_complex_type()?)).into()
            }
            FunctionAction::Some(ty) => {
                if self.argument.type_of() != *ty {
                    return Err(Error::type_error(self.argument, (*ty).clone()));
                }
                Storage::Option {
                    contents: Some(Rc::new(self.argument)),
                    ty,
                }
                .into()
            }
            FunctionAction::None(ty) => {
                self.argument.into_unit()?;
                Storage::Option { contents: None, ty }.into()
            }
        })
    }

    /// Concatentes the function's argument list with `argument`.
    pub fn pipe(&mut self, argument: Value<'host>) {
        let mut arguments = ().into();
        mem::swap(&mut arguments, &mut self.argument);
        arguments = Value::concat(arguments, argument);
        mem::swap(&mut arguments, &mut self.argument);
    }

    pub fn piped(mut self, argument: Value<'host>) -> Self {
        self.pipe(argument);
        self
    }
}

#[derive(Clone, Debug)]
pub enum FunctionAction<'host> {
    Block {
        program: Program,
        block_id: usize,
        captures: Vec<Value<'host>>,
    },
    Enum {
        variant: usize,
        definition: Rc<EnumType>,
    },
    Option,
    Some(Rc<ComplexType>),
    None(Rc<ComplexType>),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct StructType {
    pub inner: ComplexType,
}

impl From<Rc<StructType>> for Type {
    fn from(value: Rc<StructType>) -> Self {
        Type::Struct(value)
    }
}

impl From<StructType> for Type {
    fn from(value: StructType) -> Self {
        Rc::new(value).into()
    }
}

#[derive(Clone, Debug)]
pub struct EnumVariant<'host> {
    pub contents: Value<'host>,
    pub variant: usize,
    pub definition: Rc<EnumType>,
}

impl<'host> EnumVariant<'host> {
    pub fn contents(&self) -> &Value<'host> {
        &self.contents
    }

    pub fn definition(&self) -> &Rc<EnumType> {
        &self.definition
    }

    pub fn variant(&self) -> usize {
        self.variant
    }

    pub fn unwrap(self) -> (Option<Rc<str>>, Value<'host>) {
        (
            self.definition.variants[self.variant].0.clone(),
            self.contents,
        )
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct EnumType {
    pub variants: Tuple<ComplexType>,
}

impl From<Rc<EnumType>> for Type {
    fn from(value: Rc<EnumType>) -> Self {
        Type::Enum(value)
    }
}

impl From<EnumType> for Type {
    fn from(value: EnumType) -> Self {
        Rc::new(value).into()
    }
}

#[derive(Debug)]
pub enum Error<'host> {
    ExpectedNumbers(Value<'host>, Value<'host>),
    ExpectedFunction(Value<'host>),
    ExpectedEnumVariant(Value<'host>),
    ExpectedEnumType(Value<'host>),
    ExpectedStructType(Value<'host>),
    ExpectedTuple(Value<'host>),
    IncomparableValues(Value<'host>, Value<'host>),
    TypeError {
        value: Value<'host>,
        ty: ComplexType,
    },
    IndexNotFound {
        index: Value<'host>,
        container: Value<'host>,
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

impl<'host> Error<'host> {
    pub fn type_error(value: Value<'host>, ty: impl Into<ComplexType>) -> Self {
        Self::TypeError {
            value,
            ty: ty.into(),
        }
    }
}

#[derive(Debug)]
pub enum ExternError {
    MissingFunctionImpl,
    MissingIndexImpl,
    BorrowMutError,
    Other(Box<dyn std::error::Error>),
}

impl<'host> From<ExternError> for Error<'host> {
    fn from(e: ExternError) -> Error<'host> {
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

impl<'host> From<InvalidBytecode> for Error<'host> {
    fn from(e: InvalidBytecode) -> Error<'host> {
        Error::InvalidBytecode(e)
    }
}
