use espy_heart::prelude::*;
use std::cell::RefCell;
use std::mem;
use std::rc::{Rc, Weak};

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

// Cloning this type should be cheap;
// every binding usage is a clone in espy!
// Use Rcs over boxes and try to put allocations as far up as possible.
#[derive(Clone)]
pub enum Value<'host> {
    /// Unit is the absense of a value.
    /// It can be thought of as both an empty tuple and an empty named tuple.
    Unit,
    Tuple(Tuple<Value<'host>>),

    Borrow(&'host dyn Extern),
    Owned(Rc<dyn ExternOwned>),
    I64(i64),
    Bool(bool),
    String(Rc<str>),
    Function(Rc<Function<'host>>),
    EnumVariant(Rc<EnumVariant<'host>>),
    Option {
        contents: Option<Rc<Value<'host>>>,
        ty: Rc<ComplexType>,
    },
    Mut(Mut<'host>),

    Type(Type),
}

impl<'host> Value<'host> {
    pub fn get(&self, index: i64) -> Result<Value<'host>, Error<'host>> {
        match self {
            Value::Tuple(tuple) => {
                tuple
                    .value(index as usize)
                    .cloned()
                    .ok_or(Error::IndexNotFound {
                        index: index.into(),
                        container: self.clone(),
                    })
            }
            Value::Type(Type::Enum(ty)) => {
                if (index as usize) < ty.variants.len() {
                    Ok(Value::Function(Rc::new(
                        FunctionAction::Enum {
                            variant: index as usize,
                            definition: ty.clone(),
                        }
                        .into(),
                    )))
                } else {
                    Err(Error::IndexNotFound {
                        index: index.into(),
                        container: self.clone(),
                    })
                }
            }
            Value::Type(Type::Option(ty)) => match index {
                0 => Ok(Value::Function(Rc::new(
                    FunctionAction::Some(ty.clone()).into(),
                ))),
                1 => Ok(Value::Function(Rc::new(
                    FunctionAction::None(ty.clone()).into(),
                ))),
                _ => Err(Error::IndexNotFound {
                    index: index.into(),
                    container: self.clone(),
                }),
            },
            _ => Err(Error::IndexNotFound {
                index: index.into(),
                container: self.clone(),
            }),
        }
    }

    pub fn find(&self, index: Rc<str>) -> Result<Value<'host>, Error<'host>> {
        match self {
            Value::Tuple(tuple) => tuple
                .find_value(&index)
                .cloned()
                .ok_or(Error::IndexNotFound {
                    index: index.into(),
                    container: self.clone(),
                }),
            Value::Type(Type::Enum(ty)) => {
                if let Some(variant_id) = ty
                    .variants
                    .as_ref()
                    .iter()
                    .enumerate()
                    .find(|(_, (variant, _))| *variant == index)
                    .map(|(i, _)| i)
                {
                    Ok(Value::Function(Rc::new(
                        FunctionAction::Enum {
                            variant: variant_id,
                            definition: ty.clone(),
                        }
                        .into(),
                    )))
                } else {
                    Err(Error::IndexNotFound {
                        index: index.into(),
                        container: self.clone(),
                    })
                }
            }
            Value::Type(Type::Option(ty)) => match &*index {
                "Some" => Ok(Value::Function(Rc::new(
                    FunctionAction::Some(ty.clone()).into(),
                ))),
                "None" => Ok(Value::Function(Rc::new(
                    FunctionAction::None(ty.clone()).into(),
                ))),
                _ => Err(Error::IndexNotFound {
                    container: self.clone(),
                    index: index.into(),
                }),
            },
            _ => Err(Error::IndexNotFound {
                index: index.into(),
                container: self.clone(),
            }),
        }
    }

    pub fn index(&self, index: impl Into<Value<'host>>) -> Result<Value<'host>, Error<'host>> {
        match (self, index.into()) {
            (Value::Borrow(external), index) => external.index(index),
            (Value::Owned(external), index) => external.index(index),
            (_, Value::I64(index)) => self.get(index),
            (_, Value::String(index)) => self.find(index),
            (_, index) => Err(Error::IndexNotFound {
                index,
                container: self.clone(),
            }),
        }
    }
}

#[derive(Clone, Debug)]
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
    Function(Rc<FunctionType>),
    Enum(Rc<EnumType>),
    Option(Rc<ComplexType>),
    Mut(Rc<ComplexType>),

    /// The type of types.
    Type,
    Unit,
}

impl Type {
    pub fn compare(&self, r: &Self) -> bool {
        match (self, r) {
            (Self::Any, _) | (_, Self::Any) => true,
            _ => self == r,
        }
    }
}

impl PartialEq for Type {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Enum(l), Self::Enum(r)) => l == r,
            (Self::Option(l), Self::Option(r)) => l == r,
            _ => mem::discriminant(self) == mem::discriminant(other),
        }
    }
}

impl Eq for Type {}

/// ComplexType is usually the only form of [`Type`] that the espy interpreter is concerned with,
/// but it cannot be represented within espy itself (tuples of types represent Complex, instead).
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ComplexType {
    Simple(Type),
    Complex(Tuple<ComplexType>),
}

impl ComplexType {
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

impl std::fmt::Debug for Value<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Storage::")?;
        match self {
            Value::Unit => write!(f, "Unit"),
            Value::Tuple(tuple) => write!(f, "Tuple({tuple:?})"),
            Value::Borrow(external) => {
                write!(f, "Borrow(")?;
                external.debug(f)?;
                write!(f, ")")
            }
            Value::Owned(external) => {
                write!(f, "Owned(")?;
                external.debug(f)?;
                write!(f, ")")
            }
            Value::I64(i) => write!(f, "I64({i:?})"),
            Value::Bool(i) => write!(f, "Bool({i:?})"),
            Value::String(i) => write!(f, "String({i:?})"),
            Value::Function(function) => write!(f, "Function({function:?})"),
            Value::EnumVariant(enum_variant) => write!(f, "EnumVariant({enum_variant:?})"),
            Value::Option { contents, ty: _ } => write!(f, "{contents:?}"),
            Value::Mut(inner) => write!(f, "Mut({inner:?})"),
            Value::Type(t) => write!(f, "{t:?}"),
        }
    }
}

impl<'host> From<Type> for Value<'host> {
    fn from(t: Type) -> Self {
        Self::Type(t)
    }
}

impl<'host> Value<'host> {
    pub fn eq(self, other: Self) -> Result<bool, Error<'host>> {
        match (self, other) {
            (Value::Unit, Value::Unit) => Ok(true),
            (Value::Tuple(l), Value::Tuple(r)) if l.len() == r.len() => {
                for (l, r) in l.values().zip(r.values()) {
                    if !l.clone().eq(r.clone())? {
                        return Ok(false);
                    }
                }
                Ok(true)
            }
            (Value::I64(l), Value::I64(r)) => Ok(l == r),
            (Value::Bool(l), Value::Bool(r)) => Ok(l == r),
            (Value::EnumVariant(l), Value::EnumVariant(r)) => Ok(l.variant == r.variant
                && Rc::ptr_eq(&l.definition, &r.definition)
                && Rc::try_unwrap(l)
                    .map(|l| l.contents)
                    .unwrap_or_else(|l| l.contents.clone())
                    .eq(Rc::try_unwrap(r)
                        .map(|r| r.contents)
                        .unwrap_or_else(|r| r.contents.clone()))?),
            (
                Value::Option {
                    contents: l,
                    ty: l_type,
                },
                Value::Option {
                    contents: r,
                    ty: r_type,
                },
            ) => {
                if l_type == r_type {
                    Ok(l.is_none() && r.is_none()
                        || l.zip(r)
                            .map(|(l, r)| Rc::unwrap_or_clone(l).eq(Rc::unwrap_or_clone(r)))
                            .unwrap_or(Ok(false))?)
                } else {
                    Err(Error::IncomparableValues(
                        Value::Option {
                            contents: l,
                            ty: l_type,
                        },
                        Value::Option {
                            contents: r,
                            ty: r_type,
                        },
                    ))
                }
            }
            (Value::Type(l), Value::Type(r)) => Ok(l == r),
            (this, other) => Err(Error::IncomparableValues(this, other)),
        }
    }

    /// # Errors
    ///
    /// Returns an error if a mutable reference is being mutably borrowed.
    ///
    /// This can be safely ignored by the host if it has not deliberately borrowed an espy value.
    fn type_of(&self) -> Result<ComplexType, Error<'host>> {
        Ok(match &self {
            Value::Unit => Type::Unit.into(),
            Value::Tuple(tuple) => {
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
            Value::Borrow(_) | Value::Owned(_) => Type::Any.into(),
            Value::I64(_) => Type::I64.into(),
            Value::Bool(_) => Type::Bool.into(),
            Value::String(_) => Type::String.into(),
            Value::Function(function) => match &function.action {
                FunctionAction::With { signature, .. } => {
                    Type::Function(Rc::new(signature.clone())).into()
                }
                _ => Type::Function(Rc::new(FunctionType {
                    input: Type::Any.into(),
                    output: Type::Any.into(),
                }))
                .into(),
            },
            Value::EnumVariant(enum_variant) => Type::Enum(enum_variant.definition.clone()).into(),
            Value::Option { contents: _, ty } => Type::Option(ty.clone()).into(),
            Value::Mut(inner) => Type::Mut(
                inner
                    .upgrade()
                    .ok_or(Error::UpgradeError)?
                    .try_borrow()?
                    .type_of()?
                    .into(),
            )
            .into(),
            Value::Type(_) => Type::Type.into(),
        })
    }

    pub fn concat(self, r: Self) -> Self {
        match (self, r) {
            (
                Value::Tuple(Tuple(TupleStorage::Numeric(l))),
                Value::Tuple(Tuple(TupleStorage::Numeric(r))),
            ) => Value::Tuple(Tuple(TupleStorage::Numeric(rc_slice_from_iter(
                l.len() + r.len(),
                l.iter().chain(r.iter()).cloned(),
            )))),
            (
                Value::Tuple(Tuple(TupleStorage::Named(l))),
                Value::Tuple(Tuple(TupleStorage::Named(r))),
            ) => Value::Tuple(Tuple(TupleStorage::Named(rc_slice_from_iter(
                l.len() + r.len(),
                l.iter().chain(r.iter()).cloned(),
            )))),
            (l, Value::Unit) => l,
            (Value::Unit, r) => r,
            (Value::Tuple(Tuple(TupleStorage::Numeric(l))), r) => {
                Value::Tuple(Tuple(TupleStorage::Numeric(rc_slice_from_iter(
                    l.len() + 1,
                    l.iter().cloned().chain([r]),
                ))))
            }
            (l, Value::Tuple(Tuple(TupleStorage::Numeric(r)))) => {
                Value::Tuple(Tuple(TupleStorage::Numeric(rc_slice_from_iter(
                    1 + r.len(),
                    [l].into_iter().chain(r.iter().cloned()),
                ))))
            }
            (l, r) => Value::Tuple(Tuple(TupleStorage::Numeric(Rc::new([l, r])))),
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
            Err(Error::ExpectedTuple(Value::Unit)) => Ok(None),
            Err(e) => Err(e),
        }
    }

    pub fn into_bool(self) -> Result<bool, Error<'host>> {
        self.try_into()
    }

    pub fn into_i64(self) -> Result<i64, Error<'host>> {
        self.try_into()
    }

    pub fn into_str(self) -> Result<Rc<str>, Error<'host>> {
        self.try_into()
    }

    pub fn as_str(&self) -> Option<&str> {
        if let Value::String(s) = &self {
            Some(s)
        } else {
            None
        }
    }

    pub fn borrow(external: &'host dyn Extern) -> Self {
        Value::Borrow(external)
    }

    pub fn owned(external: Rc<dyn ExternOwned>) -> Self {
        Value::Owned(external)
    }

    pub fn into_function(self) -> Result<Function<'host>, Error<'host>> {
        self.try_into()
    }

    pub fn into_enum_variant(self) -> Result<Rc<EnumVariant<'host>>, Error<'host>> {
        self.try_into()
    }

    pub fn into_option(self) -> Result<Option<Value<'host>>, Error<'host>> {
        match self {
            Value::Option { contents, ty: _ } => Ok(contents.map(|x| (*x).clone())),
            _ => Err(Error::ExpectedOption(self)),
        }
    }

    pub fn into_refcell(self) -> Result<Rc<RefCell<Value<'host>>>, Error<'host>> {
        match &self {
            Value::Mut(inner) => Ok(inner.upgrade().ok_or(Error::UpgradeError)?),
            _ => Err(Error::ExpectedReference(self)),
        }
    }

    pub fn into_complex_type(self) -> Result<ComplexType, Error<'host>> {
        self.try_into()
    }

    pub fn into_enum_type(self) -> Result<Rc<EnumType>, Error<'host>> {
        self.try_into()
    }
}

impl From<()> for Value<'_> {
    fn from(_: ()) -> Self {
        Value::Unit
    }
}

impl From<Option<()>> for Value<'_> {
    fn from(value: Option<()>) -> Self {
        Value::Option {
            contents: value.map(|()| Rc::new(Value::Unit)),
            ty: Rc::new(Type::Unit.into()),
        }
    }
}

impl From<bool> for Value<'_> {
    fn from(value: bool) -> Self {
        Value::Bool(value)
    }
}

impl From<Option<bool>> for Value<'_> {
    fn from(value: Option<bool>) -> Self {
        Value::Option {
            contents: value.map(|value| Rc::new(Value::Bool(value))),
            ty: Rc::new(Type::Bool.into()),
        }
    }
}

impl From<i64> for Value<'_> {
    fn from(i: i64) -> Self {
        Value::I64(i)
    }
}

impl From<Option<i64>> for Value<'_> {
    fn from(value: Option<i64>) -> Self {
        Value::Option {
            contents: value.map(|value| Rc::new(Value::I64(value))),
            ty: Rc::new(Type::I64.into()),
        }
    }
}

impl From<Rc<str>> for Value<'_> {
    fn from(s: Rc<str>) -> Self {
        Value::String(s)
    }
}

impl From<Option<Rc<str>>> for Value<'_> {
    fn from(value: Option<Rc<str>>) -> Self {
        Value::Option {
            contents: value.map(|value| Rc::new(Value::String(value))),
            ty: Rc::new(Type::String.into()),
        }
    }
}

impl<'host> From<Rc<Function<'host>>> for Value<'host> {
    fn from(f: Rc<Function<'host>>) -> Self {
        Value::Function(f)
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
        if let Value::Unit = value {
            Ok(())
        } else {
            Err(Error::type_error(value, Type::Unit))
        }
    }
}

impl<'host> TryFrom<Value<'host>> for Tuple<Value<'host>> {
    type Error = Error<'host>;

    fn try_from(value: Value<'host>) -> Result<Self, Self::Error> {
        if let Value::Tuple(value) = value {
            Ok(value)
        } else {
            Err(Error::ExpectedTuple(value))
        }
    }
}

impl<'host> TryFrom<Value<'host>> for bool {
    type Error = Error<'host>;

    fn try_from(value: Value<'host>) -> Result<Self, Self::Error> {
        if let Value::Bool(bool) = value {
            Ok(bool)
        } else {
            Err(Error::type_error(value, Type::Unit))
        }
    }
}

impl<'host> TryFrom<Value<'host>> for i64 {
    type Error = Error<'host>;

    fn try_from(value: Value<'host>) -> Result<Self, Self::Error> {
        if let Value::I64(value) = value {
            Ok(value)
        } else {
            Err(Error::type_error(value, Type::I64))
        }
    }
}

impl<'host> TryFrom<Value<'host>> for Rc<str> {
    type Error = Error<'host>;

    fn try_from(value: Value<'host>) -> Result<Self, Self::Error> {
        if let Value::String(value) = value {
            Ok(value)
        } else {
            Err(Error::type_error(value, Type::String))
        }
    }
}

impl<'host> TryFrom<Value<'host>> for Rc<Function<'host>> {
    type Error = Error<'host>;
    fn try_from(value: Value<'host>) -> Result<Self, Self::Error> {
        if let Value::Function(value) = value {
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
        if let Value::EnumVariant(value) = value {
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
            Value::Type(t) => Ok(t.into()),
            Value::Tuple(tuple) => Ok(ComplexType::Complex(tuple.try_into()?)),
            _ => Err(Error::type_error(value, Type::Type)),
        }
    }
}

impl<'host> TryFrom<Value<'host>> for Rc<EnumType> {
    type Error = Error<'host>;
    fn try_from(value: Value<'host>) -> Result<Self, Self::Error> {
        if let Value::Type(Type::Enum(value)) = value {
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

impl<'host> TryFrom<Tuple<Value<'host>>> for Tuple<ComplexType> {
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
}

impl<'host> Function<'host> {
    pub fn borrow(external: &'host dyn ExternFn) -> Self {
        Function {
            action: FunctionAction::Borrow(external),
            argument: ().into(),
        }
    }

    pub fn owned(external: Rc<dyn ExternFnOwned>) -> Self {
        Function {
            action: FunctionAction::Owned(external),
            argument: ().into(),
        }
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
                let result = program.eval(block_id, &mut captures)?;
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
                Value::EnumVariant(Rc::new(EnumVariant {
                    contents: self.argument,
                    variant,
                    definition,
                }))
            }
            FunctionAction::Mut => {
                if let Ok(ty) = ComplexType::try_from(self.argument.clone()) {
                    Type::Mut(Rc::new(ty)).into()
                } else {
                    Value::Mut(Mut::new(Rc::new(RefCell::new(self.argument))))
                }
            }
            FunctionAction::Option => {
                Type::Option(Rc::new(self.argument.into_complex_type()?)).into()
            }
            FunctionAction::Some(ty) => {
                if self.argument.type_of()? != *ty {
                    return Err(Error::type_error(self.argument, (*ty).clone()));
                }
                Value::Option {
                    contents: Some(Rc::new(self.argument)),
                    ty,
                }
            }
            FunctionAction::None(ty) => {
                self.argument.into_unit()?;
                Value::Option { contents: None, ty }
            }
            FunctionAction::Borrow(external) => external.call(self.argument)?,
            FunctionAction::Owned(external) => external.call(self.argument)?,
        };
        Ok(result)
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
        signature: FunctionType,
        captures: Vec<Value<'host>>,
    },
    Enum {
        variant: usize,
        definition: Rc<EnumType>,
    },
    Mut,
    Option,
    Some(Rc<ComplexType>),
    None(Rc<ComplexType>),
    Borrow(&'host dyn ExternFn),
    Owned(Rc<dyn ExternFnOwned>),
}

impl<'host> From<FunctionAction<'host>> for Function<'host> {
    fn from(action: FunctionAction<'host>) -> Self {
        Self {
            action,
            argument: ().into(),
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
            Self::Owned(arg0) => arg0.debug(f),
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
    /// so it should only be exposed to trusted espy programs.
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
pub struct FunctionType {
    pub input: ComplexType,
    pub output: ComplexType,
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

    pub fn unwrap(self) -> (Rc<str>, Value<'host>) {
        (
            self.definition.variants[self.variant].0.clone(),
            self.contents,
        )
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct EnumType {
    pub variants: Rc<[(Rc<str>, ComplexType)]>,
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
    ExpectedOption(Value<'host>),
    ExpectedEnumType(Value<'host>),
    ExpectedStructType(Value<'host>),
    ExpectedTuple(Value<'host>),
    ExpectedNamedTuple(Value<'host>),
    ExpectedReference(Value<'host>),

    IncomparableValues(Value<'host>, Value<'host>),
    TypeError {
        value: Value<'host>,
        ty: ComplexType,
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
    /// These may carry less information than a typical espy error,
    /// or wrap the error type of another crate.
    ExternError(ExternError),
    /// Errors that occur due to invalid bytecode.
    ///
    /// If this is emitted due to bytecode from the espy compiler,
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
    /// A clone referred to a builtin value that does not exist.
    InvalidBuiltin,
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

/// Reads a u32 and casts it to usize for convenience.
fn read_header(bytes: &[u8], at: usize) -> Result<usize, InvalidBytecode> {
    let (((a, b), c), d) = bytes
        .get(at)
        .zip(bytes.get(at + 1))
        .zip(bytes.get(at + 2))
        .zip(bytes.get(at + 3))
        .ok_or(InvalidBytecode::MalformedHeader)?;
    Ok(u32::from_le_bytes([*a, *b, *c, *d]) as usize)
}

fn block_count(bytes: &[u8]) -> Result<usize, InvalidBytecode> {
    read_header(bytes, 0)
}

fn string_count(bytes: &[u8]) -> Result<usize, InvalidBytecode> {
    read_header(bytes, size_of::<u32>())
}

fn offsets(bytes: &[u8]) -> Result<&[u8], InvalidBytecode> {
    let offset_count = block_count(bytes)? + string_count(bytes)?;
    let first_offset = size_of::<u32>() * 2;
    let last_offset = first_offset + size_of::<u32>() * offset_count;
    bytes
        .get(first_offset..last_offset)
        .ok_or(InvalidBytecode::MalformedHeader)
}

fn block(bytes: &[u8], block_id: usize) -> Result<&[u8], InvalidBytecode> {
    let offsets = offsets(bytes)?;
    let block_position = size_of::<u32>() * block_id;
    let start = read_header(offsets, block_position)?;
    let end = read_header(offsets, size_of::<u32>() + block_position).unwrap_or(bytes.len());
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
            .map(|string| {
                let string = size_of::<u32>() * (string + block_count(&bytes)?);
                let offsets = offsets(&bytes)?;
                let start = read_header(offsets, string)?;
                let end = read_header(offsets, size_of::<u32>() + string).unwrap_or(bytes.len());
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
                    match (&$l, &$r) {
                        (Value::$type($l), Value::$type($r)) => {
                            stack.push(Value::$expr_type($expr))
                        }
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
                            stack.push(Value::Function(Rc::new(FunctionAction::Option.into())));
                        }
                        builtins::MUT => {
                            stack.push(Value::Function(Rc::new(FunctionAction::Mut.into())));
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
                    if let Value::Bool(false) = program.pop(stack)? {
                        program.pc = target;
                    }
                }

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
                    stack.push(Value::Function(Rc::new(
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
                    )));
                }
                instruction::PUSH_ENUM => {
                    let variants = program.pop(stack)?;
                    let Value::Tuple(Tuple(TupleStorage::Named(variants))) = variants else {
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
                    let mut function = Rc::<Function>::try_from(program.pop(stack)?)?;
                    let argument = program.pop(stack)?;
                    let function_mut = Rc::make_mut(&mut function);
                    let mut arguments = ().into();
                    mem::swap(&mut arguments, &mut function_mut.argument);
                    arguments = Value::concat(arguments, argument);
                    mem::swap(&mut arguments, &mut function_mut.argument);
                    stack.push(Value::Function(function));
                }

                instruction::CALL => {
                    let argument = program.pop(stack)?;
                    let function = program.pop(stack)?;
                    let result = match function {
                        Value::Function(function) => Rc::<Function>::try_unwrap(function)
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
                    stack.push(container.index(index)?);
                }
                instruction::NAME => {
                    let name_id = program.next4()?;
                    let name = self
                        .owned_strings
                        .get(name_id)
                        .ok_or(InvalidBytecode::UnexpectedStringId)?
                        .clone();
                    let value = program.pop(stack)?;
                    stack.push(Value::Tuple(Tuple::from([(name, value)])))
                }
                instruction::NEST => {
                    let value = program.pop(stack)?;
                    stack.push(Value::Tuple(Tuple::from([value])))
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

pub trait Extern {
    fn index<'host>(&'host self, _index: Value<'host>) -> Result<Value<'host>, Error<'host>> {
        Err(ExternError::MissingIndexImpl)?
    }

    fn debug(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{{external value}}")
    }
}

pub trait ExternOwned {
    fn index<'host>(&self, _index: Value<'host>) -> Result<Value<'host>, Error<'host>> {
        Err(ExternError::MissingIndexImpl)?
    }

    fn debug(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{{external value}}")
    }
}

pub trait ExternFn {
    fn call<'host>(&'host self, _argument: Value<'host>) -> Result<Value<'host>, Error<'host>> {
        Err(ExternError::MissingFunctionImpl)?
    }

    fn debug(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{{external function}}")
    }
}

pub trait ExternFnOwned {
    fn call<'host>(&self, _argument: Value<'host>) -> Result<Value<'host>, Error<'host>> {
        Err(ExternError::MissingFunctionImpl)?
    }

    fn debug(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{{external function}}")
    }
}
