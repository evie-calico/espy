use std::cell::RefCell;
use std::mem;
use std::rc::{Rc, Weak};

mod interpreter;
pub use interpreter::*;
mod interop;
pub use interop::{Extern, ExternFn, FunctionWrapper, FunctionWrapperMut, wrap_fn, wrap_fn_mut};

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
    Struct {
        inner: Rc<Value<'host>>,
        ty: Rc<StructType<'host>>,
    },
    EnumVariant(Rc<EnumVariant<'host>>),
    Option {
        contents: Option<Rc<Value<'host>>>,
        ty: Rc<ComplexType<'host>>,
    },
    Mut(Mut<'host>),

    Type(Type<'host>),
}

#[derive(Clone, Debug)]
pub enum Type<'host> {
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
    Function(Rc<FunctionType<'host>>),
    Struct(Rc<StructType<'host>>),
    Enum(Rc<EnumType<'host>>),
    Option(Rc<ComplexType<'host>>),
    Mut(Rc<ComplexType<'host>>),

    /// The type of types.
    Type,
    Unit,
}

impl Type<'_> {
    pub fn compare(&self, r: &Self) -> bool {
        match (self, r) {
            (Self::Any, _) | (_, Self::Any) => true,
            _ => self == r,
        }
    }
}

impl PartialEq for Type<'_> {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Struct(l), Self::Struct(r)) => Rc::as_ptr(l) == Rc::as_ptr(r),
            (Self::Enum(l), Self::Enum(r)) => l == r,
            (Self::Option(l), Self::Option(r)) => l == r,
            _ => mem::discriminant(self) == mem::discriminant(other),
        }
    }
}

impl Eq for Type<'_> {}

/// ComplexType is usually the only form of [`Type`] that the espyscript interpreter is concerned with,
/// but it cannot be represented within espyscript itself (tuples of types represent Complex, instead).
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ComplexType<'host> {
    Simple(Type<'host>),
    Complex(Tuple<ComplexType<'host>>),
}

impl ComplexType<'_> {
    pub fn compare(&self, r: &Self) -> bool {
        match (self, r) {
            (ComplexType::Simple(Type::Any), _) | (_, ComplexType::Simple(Type::Any)) => true,
            (ComplexType::Simple(l), ComplexType::Simple(r)) => l == r,
            (ComplexType::Complex(l), ComplexType::Complex(r)) => {
                l.values().zip(r.values()).all(|(l, r)| l.compare(r))
            }
            _ => false,
        }
    }
}

impl<'host> From<Type<'host>> for ComplexType<'host> {
    fn from(value: Type<'host>) -> Self {
        Self::Simple(value)
    }
}

impl<'host> From<Tuple<ComplexType<'host>>> for ComplexType<'host> {
    fn from(value: Tuple<ComplexType<'host>>) -> Self {
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
            Storage::Struct { inner, ty } => f
                .debug_struct("Struct")
                .field("inner", inner)
                .field("ty", ty)
                .finish(),
            Storage::EnumVariant(enum_variant) => write!(f, "EnumVariant({enum_variant:?})"),
            Storage::Option { contents, ty: _ } => write!(f, "{contents:?}"),
            Storage::Mut(inner) => write!(f, "Mut({inner:?})"),
            Storage::Type(t) => write!(f, "{t:?}"),
        }
    }
}

impl<'host> From<Storage<'host>> for Value<'host> {
    fn from(storage: Storage<'host>) -> Self {
        Self { storage }
    }
}

impl<'host> From<Type<'host>> for Value<'host> {
    fn from(t: Type<'host>) -> Self {
        Self { storage: t.into() }
    }
}

impl<'host> From<Type<'host>> for Storage<'host> {
    fn from(t: Type<'host>) -> Self {
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

    /// # Errors
    ///
    /// Returns an error if a mutable reference is being mutably borrowed.
    ///
    /// This can be safely ignored by the host if it has not deliberately borrowed an espyscript value.
    fn type_of(&self) -> Result<ComplexType<'host>, Error<'host>> {
        Ok(match &self.storage {
            Storage::Unit => Type::Unit.into(),
            Storage::Tuple(tuple) => {
                let complex = match &tuple.0 {
                    TupleStorage::Numeric(items) => Tuple(TupleStorage::Numeric(
                        rc_slice_try_from_iter(items.len(), items.iter().map(Value::type_of))?,
                    )),
                    TupleStorage::Named(items) => {
                        Tuple(TupleStorage::Named(rc_slice_try_from_iter(
                            items.len(),
                            items.iter().map(|(name, value)| {
                                value.type_of().map(|value| (name.clone(), value))
                            }),
                        )?))
                    }
                };
                // The type of a tuple containing only types is `type`, not `(type, type, ..)`
                if complex
                    .values()
                    .all(|x| matches!(x, ComplexType::Simple(Type::Type)))
                {
                    Type::Type.into()
                } else {
                    complex.into()
                }
            }
            Storage::Borrow(_) => Type::Any.into(),
            Storage::I64(_) => Type::I64.into(),
            Storage::Bool(_) => Type::Bool.into(),
            Storage::String(_) => Type::String.into(),
            Storage::Function(function) => match &function.action {
                FunctionAction::With { signature, .. } => {
                    Type::Function(Rc::new(signature.clone())).into()
                }
                _ => Type::Function(Rc::new(FunctionType {
                    input: Type::Any.into(),
                    output: Type::Any.into(),
                }))
                .into(),
            },
            Storage::Struct { inner: _, ty } => Type::Struct(ty.clone()).into(),
            Storage::EnumVariant(enum_variant) => {
                Type::Enum(enum_variant.definition.clone()).into()
            }
            Storage::Option { contents: _, ty } => Type::Option(ty.clone()).into(),
            Storage::Mut(inner) => Type::Mut(
                inner
                    .upgrade()
                    .ok_or(Error::UpgradeError)?
                    .try_borrow()?
                    .type_of()?
                    .into(),
            )
            .into(),
            Storage::Type(_) => Type::Type.into(),
        })
    }

    pub fn concat(self, r: Self) -> Self {
        match (self, r) {
            (
                Value {
                    storage: Storage::Tuple(Tuple(TupleStorage::Numeric(l))),
                    ..
                },
                Value {
                    storage: Storage::Tuple(Tuple(TupleStorage::Numeric(r))),
                    ..
                },
            ) => Value {
                storage: Storage::Tuple(Tuple(TupleStorage::Numeric(rc_slice_from_iter(
                    l.len() + r.len(),
                    l.iter().chain(r.iter()).cloned(),
                )))),
            },
            (
                Value {
                    storage: Storage::Tuple(Tuple(TupleStorage::Named(l))),
                    ..
                },
                Value {
                    storage: Storage::Tuple(Tuple(TupleStorage::Named(r))),
                    ..
                },
            ) => Value {
                storage: Storage::Tuple(Tuple(TupleStorage::Named(rc_slice_from_iter(
                    l.len() + r.len(),
                    l.iter().chain(r.iter()).cloned(),
                )))),
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
                    storage: Storage::Tuple(Tuple(TupleStorage::Numeric(l))),
                    ..
                },
                r,
            ) => Value {
                storage: Storage::Tuple(Tuple(TupleStorage::Numeric(rc_slice_from_iter(
                    l.len() + 1,
                    l.iter().cloned().chain([r]),
                )))),
            },
            (
                l,
                Value {
                    storage: Storage::Tuple(Tuple(TupleStorage::Numeric(r))),
                    ..
                },
            ) => Value {
                storage: Storage::Tuple(Tuple(TupleStorage::Numeric(rc_slice_from_iter(
                    1 + r.len(),
                    [l].into_iter().chain(r.iter().cloned()),
                )))),
            },
            (l, r) => Value {
                storage: Storage::Tuple(Tuple(TupleStorage::Numeric(Rc::new([l, r])))),
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

    pub fn into_refcell(self) -> Result<Rc<RefCell<Value<'host>>>, Error<'host>> {
        match &self.storage {
            Storage::Mut(inner) => Ok(inner.upgrade().ok_or(Error::UpgradeError)?),
            _ => Err(Error::ExpectedReference(self)),
        }
    }

    pub fn into_complex_type(self) -> Result<ComplexType<'host>, Error<'host>> {
        self.try_into()
    }

    pub fn into_enum_type(self) -> Result<Rc<EnumType<'host>>, Error<'host>> {
        self.try_into()
    }

    pub fn into_struct_type(self) -> Result<Rc<StructType<'host>>, Error<'host>> {
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

impl<'host> From<Rc<Function<'host>>> for Value<'host> {
    fn from(f: Rc<Function<'host>>) -> Self {
        Storage::Function(f).into()
    }
}

impl<'host> From<Function<'host>> for Value<'host> {
    fn from(f: Function<'host>) -> Self {
        Rc::new(f).into()
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

impl<'host> TryFrom<Value<'host>> for ComplexType<'host> {
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

impl<'host> TryFrom<Value<'host>> for Rc<EnumType<'host>> {
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

impl<'host> TryFrom<Value<'host>> for EnumType<'host> {
    type Error = Error<'host>;

    fn try_from(value: Value<'host>) -> Result<Self, Self::Error> {
        Ok(Rc::unwrap_or_clone(Rc::<Self>::try_from(value)?))
    }
}

impl<'host> TryFrom<Value<'host>> for Rc<StructType<'host>> {
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

impl<'host> TryFrom<Value<'host>> for StructType<'host> {
    type Error = Error<'host>;

    fn try_from(value: Value<'host>) -> Result<Self, Self::Error> {
        Ok(Rc::unwrap_or_clone(Rc::<Self>::try_from(value)?))
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
enum TupleStorage<T> {
    Numeric(Rc<[T]>),
    Named(Rc<[(Rc<str>, T)]>),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Tuple<T>(TupleStorage<T>);

impl<T> Tuple<T> {
    pub fn len(&self) -> usize {
        match &self.0 {
            TupleStorage::Numeric(items) => items.len(),
            TupleStorage::Named(items) => items.len(),
        }
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub fn value(&self, index: usize) -> Option<&T> {
        match &self.0 {
            TupleStorage::Numeric(items) => items.get(index),
            TupleStorage::Named(items) => items.get(index).map(|(_name, value)| value),
        }
    }

    pub fn find_value(&self, name: &str) -> Option<&T> {
        let TupleStorage::Named(items) = &self.0 else {
            return None;
        };
        items
            .iter()
            .find_map(|(n, v)| if **n == *name { Some(v) } else { None })
    }

    pub fn values(&self) -> impl Iterator<Item = &T> {
        (0..self.len()).map(|i| match &self.0 {
            TupleStorage::Numeric(items) => &items[i],
            TupleStorage::Named(items) => &items[i].1,
        })
    }
}

impl<'host> TryFrom<Tuple<Value<'host>>> for Tuple<ComplexType<'host>> {
    type Error = Error<'host>;

    fn try_from(tuple: Tuple<Value<'host>>) -> Result<Self, Self::Error> {
        match &tuple.0 {
            TupleStorage::Numeric(items) => rc_slice_try_from_iter(
                items.len(),
                items.iter().map(|value| value.clone().try_into()),
            )
            .map(TupleStorage::Numeric)
            .map(Tuple),
            TupleStorage::Named(items) => rc_slice_try_from_iter(
                items.len(),
                items.iter().map(|(name, value)| {
                    value.clone().try_into().map(|value| (name.clone(), value))
                }),
            )
            .map(TupleStorage::Named)
            .map(Tuple),
        }
    }
}

impl<'host> TryFrom<Tuple<Value<'host>>> for Tuple<Function<'host>> {
    type Error = Error<'host>;

    fn try_from(tuple: Tuple<Value<'host>>) -> Result<Self, Self::Error> {
        match &tuple.0 {
            TupleStorage::Numeric(items) => rc_slice_try_from_iter(
                items.len(),
                items.iter().map(|value| value.clone().try_into()),
            )
            .map(TupleStorage::Numeric)
            .map(Tuple),
            TupleStorage::Named(items) => rc_slice_try_from_iter(
                items.len(),
                items.iter().map(|(name, value)| {
                    value.clone().try_into().map(|value| (name.clone(), value))
                }),
            )
            .map(TupleStorage::Named)
            .map(Tuple),
        }
    }
}

impl<T> From<Rc<[T]>> for Tuple<T> {
    fn from(value: Rc<[T]>) -> Self {
        Self(TupleStorage::Numeric(value))
    }
}

impl<const N: usize, T> From<[T; N]> for Tuple<T> {
    fn from(value: [T; N]) -> Self {
        Self(TupleStorage::Numeric(Rc::from(value)))
    }
}

impl<T> From<Rc<[(Rc<str>, T)]>> for Tuple<T> {
    fn from(value: Rc<[(Rc<str>, T)]>) -> Self {
        Self(TupleStorage::Named(value))
    }
}

impl<const N: usize, T> From<[(Rc<str>, T); N]> for Tuple<T> {
    fn from(value: [(Rc<str>, T); N]) -> Self {
        Self(TupleStorage::Named(Rc::from(value)))
    }
}

#[derive(Clone, Debug)]
pub struct Function<'host> {
    action: FunctionAction<'host>,
    argument: Value<'host>,
    // If specified, the return type of this function must be casted into the provided structure.
    constructor: Option<Rc<StructType<'host>>>,
}

impl<'host> Function<'host> {
    pub fn borrow(external: &'host dyn ExternFn) -> Self {
        Function {
            action: FunctionAction::Borrow(external),
            argument: ().into(),
            constructor: None,
        }
    }

    pub fn as_constructor(mut self, structure: Rc<StructType<'host>>) -> Self {
        self.constructor = Some(structure);
        self
    }

    pub fn eval(self) -> Result<Value<'host>, Error<'host>> {
        let result = match self.action {
            FunctionAction::With {
                program,
                block_id,
                signature: FunctionType { input, output },
                mut captures,
            } => {
                if !self.argument.type_of()?.compare(&input) {
                    return Err(Error::type_error(self.argument, input));
                }
                captures.push(self.argument);
                let result = program.eval(block_id, captures)?;
                if !result.type_of()?.compare(&output) {
                    return Err(Error::type_error(result, output));
                }
                result
            }
            FunctionAction::Enum {
                variant,
                definition,
            } => {
                let ty = &definition.variants[variant].1;
                if *ty != Type::Any.into() && self.argument.type_of()? != *ty {
                    return Err(Error::type_error(self.argument, ty.clone()));
                }
                Storage::EnumVariant(Rc::new(EnumVariant {
                    contents: self.argument,
                    variant,
                    definition,
                }))
                .into()
            }
            FunctionAction::Mut => {
                if let Ok(ty) = ComplexType::try_from(self.argument.clone()) {
                    Type::Mut(Rc::new(ty)).into()
                } else {
                    Storage::Mut(Mut::new(Rc::new(RefCell::new(self.argument)))).into()
                }
            }
            FunctionAction::Option => {
                Type::Option(Rc::new(self.argument.into_complex_type()?)).into()
            }
            FunctionAction::Some(ty) => {
                if self.argument.type_of()? != *ty {
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
            FunctionAction::Borrow(external) => external.call(self.argument)?,
        };
        if let Some(structure) = self.constructor {
            if structure.inner != Type::Any.into() && structure.inner != result.type_of()? {
                Err(Error::type_error(result, structure.inner.clone()))
            } else {
                Ok(Storage::Struct {
                    inner: Rc::new(result),
                    ty: structure,
                }
                .into())
            }
        } else {
            Ok(result)
        }
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

#[derive(Clone)]
enum FunctionAction<'host> {
    With {
        program: Program,
        block_id: usize,
        signature: FunctionType<'host>,
        captures: Vec<Value<'host>>,
    },
    Enum {
        variant: usize,
        definition: Rc<EnumType<'host>>,
    },
    Mut,
    Option,
    Some(Rc<ComplexType<'host>>),
    None(Rc<ComplexType<'host>>),
    Borrow(&'host dyn ExternFn),
}

impl<'host> From<FunctionAction<'host>> for Function<'host> {
    fn from(action: FunctionAction<'host>) -> Self {
        Self {
            action,
            argument: ().into(),
            constructor: None,
        }
    }
}

impl std::fmt::Debug for FunctionAction<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::With {
                program,
                block_id,
                signature,
                captures,
            } => f
                .debug_struct("Block")
                .field("program", &Rc::as_ptr(&program.bytes))
                .field("block_id", block_id)
                .field("signature", signature)
                .field("captures", captures)
                .finish(),
            Self::Enum {
                variant,
                definition,
            } => f
                .debug_struct("Enum")
                .field("variant", variant)
                .field("definition", definition)
                .finish(),
            Self::Mut => write!(f, "Mut"),
            Self::Option => write!(f, "Option"),
            Self::Some(arg0) => f.debug_tuple("Some").field(arg0).finish(),
            Self::None(arg0) => f.debug_tuple("None").field(arg0).finish(),
            Self::Borrow(arg0) => arg0.debug(f),
        }
    }
}

#[derive(Debug)]
enum MutRefSource<'host> {
    Origin(Rc<RefCell<Value<'host>>>),
    Child(Weak<RefCell<Value<'host>>>),
}

#[derive(Debug)]
pub struct Mut<'host> {
    source: MutRefSource<'host>,
}

impl<'host> Mut<'host> {
    pub fn new(rc: Rc<RefCell<Value<'host>>>) -> Self {
        Self {
            source: MutRefSource::Origin(rc),
        }
    }
    /// Creates a strong reference to the mutable reference's origin if it still exists.
    ///
    /// This can be used to create reference cycles and leak memory,
    /// so it should only be exposed to trusted espyscript programs.
    pub fn upgrade(&self) -> Option<Rc<RefCell<Value<'host>>>> {
        match &self.source {
            MutRefSource::Origin(rc) => Some(rc.clone()),
            MutRefSource::Child(weak) => weak.upgrade(),
        }
    }
}

impl<'host> Clone for Mut<'host> {
    fn clone(&self) -> Self {
        Self {
            source: MutRefSource::Child(match &self.source {
                MutRefSource::Origin(rc) => Rc::downgrade(rc),
                MutRefSource::Child(weak) => weak.clone(),
            }),
        }
    }
}

#[derive(Clone, Debug)]
pub struct FunctionType<'host> {
    pub input: ComplexType<'host>,
    pub output: ComplexType<'host>,
}

#[derive(Clone, Debug)]
pub struct StructType<'host> {
    pub inner: ComplexType<'host>,
    pub methods: Option<Tuple<Function<'host>>>,
    pub constructors: Rc<[(Rc<str>, Function<'host>)]>,
}

impl<'host> From<Rc<StructType<'host>>> for Type<'host> {
    fn from(value: Rc<StructType<'host>>) -> Self {
        Type::Struct(value)
    }
}

impl<'host> From<StructType<'host>> for Type<'host> {
    fn from(value: StructType<'host>) -> Self {
        Rc::new(value).into()
    }
}

#[derive(Clone, Debug)]
pub struct EnumVariant<'host> {
    pub contents: Value<'host>,
    pub variant: usize,
    pub definition: Rc<EnumType<'host>>,
}

impl<'host> EnumVariant<'host> {
    pub fn contents(&self) -> &Value<'host> {
        &self.contents
    }

    pub fn definition(&self) -> &Rc<EnumType<'host>> {
        &self.definition
    }

    pub fn variant(&self) -> usize {
        self.variant
    }

    pub fn unwrap(self) -> (Rc<str>, Value<'host>) {
        (
            self.definition.variants[self.variant].0.clone(),
            self.contents,
        )
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct EnumType<'host> {
    pub variants: Rc<[(Rc<str>, ComplexType<'host>)]>,
}

impl<'host> From<Rc<EnumType<'host>>> for Type<'host> {
    fn from(value: Rc<EnumType<'host>>) -> Self {
        Type::Enum(value)
    }
}

impl<'host> From<EnumType<'host>> for Type<'host> {
    fn from(value: EnumType<'host>) -> Self {
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
    ExpectedNamedTuple(Value<'host>),
    ExpectedReference(Value<'host>),

    IncomparableValues(Value<'host>, Value<'host>),
    TypeError {
        value: Value<'host>,
        ty: ComplexType<'host>,
    },
    IndexNotFound {
        index: Value<'host>,
        container: Value<'host>,
    },
    UpgradeError,
    BorrowError(std::cell::BorrowError),
    BorrowMutError(std::cell::BorrowMutError),
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
    pub fn type_error(value: Value<'host>, ty: impl Into<ComplexType<'host>>) -> Self {
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
    Other(Box<dyn std::error::Error>),
}

impl<'host> From<ExternError> for Error<'host> {
    fn from(e: ExternError) -> Error<'host> {
        Error::ExternError(e)
    }
}

impl<'host> From<std::cell::BorrowError> for Error<'host> {
    fn from(e: std::cell::BorrowError) -> Error<'host> {
        Error::BorrowError(e)
    }
}

impl<'host> From<std::cell::BorrowMutError> for Error<'host> {
    fn from(e: std::cell::BorrowMutError) -> Error<'host> {
        Error::BorrowMutError(e)
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
