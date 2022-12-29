use std::{
    borrow::Borrow,
    cmp::Ordering,
    collections::HashMap,
    fmt::{Debug, Display, Formatter},
    hash::{Hash, Hasher},
    iter::FromIterator,
    ops::Deref,
    rc::Rc,
    slice::SliceIndex,
};

use derive_more::{DebugCustom, Display, Index, IndexMut, IntoIterator, IsVariant, Unwrap};
use itertools::Itertools;
use num::Float;
use serde::{
    de::{Error, MapAccess, SeqAccess, Visitor},
    ser::{SerializeMap, SerializeSeq},
    serde_if_integer128, Deserialize, Deserializer, Serialize, Serializer,
};

use crate::Number;

type Vector = Vec<Value>;
type Map = HashMap<RcString, Value>;
pub type RcString = Rc<String>;

#[derive(
    Clone, Eq, PartialEq, Hash, Default, DebugCustom, Display, IntoIterator, Index, IndexMut,
)]
#[debug(fmt = "{_0:?}")]
#[display(fmt = "{_0:?}")]
pub struct Array(#[into_iterator(owned, ref, ref_mut)] Vector);

#[derive(Clone, Eq, PartialEq, Default, DebugCustom, Display, IntoIterator, Index, IndexMut)]
#[debug(fmt = "{_0:?}")]
#[display(fmt = "{_0:?}")]
pub struct Object(#[into_iterator(owned, ref, ref_mut)] Map);

#[allow(clippy::derive_hash_xor_eq)] // HashMap::eq is implemented properly.
impl Hash for Object {
    fn hash<H: Hasher>(&self, state: &mut H) {
        for (key, value) in self.0.iter().sorted_unstable_by_key(|e| e.0) {
            key.hash(state);
            value.hash(state);
        }
    }
}

impl Array {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn with_capacity(capacity: usize) -> Self {
        Self(Vector::with_capacity(capacity))
    }

    pub fn from_vec(v: Vector) -> Self {
        Self(v)
    }

    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    pub fn len(&self) -> usize {
        self.0.len()
    }

    pub fn push<V>(&mut self, value: V)
    where
        V: Into<Value>,
    {
        self.0.push(value.into())
    }

    pub fn extend<I: IntoIterator<Item = Value>>(&mut self, iter: I) {
        self.0.extend(iter);
    }

    pub fn get<I: SliceIndex<[Value]>>(&self, index: I) -> Option<&I::Output> {
        self.0.get(index)
    }

    pub fn remove(&mut self, index: usize) -> Value {
        self.0.remove(index)
    }

    pub fn split_off(&mut self, index: usize) -> Self {
        Self(self.0.split_off(index))
    }
}

impl Object {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn with_capacity(capacity: usize) -> Self {
        Self(Map::with_capacity(capacity))
    }

    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    pub fn len(&self) -> usize {
        self.0.len()
    }

    pub fn iter(&self) -> impl Iterator<Item = (&RcString, &Value)> {
        self.0.iter()
    }

    pub fn insert<K, V>(&mut self, key: K, value: V) -> Option<Value>
    where
        K: Into<RcString>,
        V: Into<Value>,
    {
        self.0.insert(key.into(), value.into())
    }

    pub fn extend<T: IntoIterator<Item = (RcString, Value)>>(&mut self, iter: T) {
        self.0.extend(iter)
    }

    pub fn keys(&self) -> impl Iterator<Item = &RcString> {
        self.0.keys()
    }

    pub fn get<Q: ?Sized>(&self, key: &Q) -> Option<&Value>
    where
        RcString: Borrow<Q>,
        Q: Hash + Eq,
    {
        self.0.get(key)
    }

    pub fn get_mut<Q: ?Sized>(&mut self, key: &Q) -> Option<&mut Value>
    where
        RcString: Borrow<Q>,
        Q: Hash + Eq,
    {
        self.0.get_mut(key)
    }

    pub fn remove<Q: ?Sized>(&mut self, key: &Q) -> Option<Value>
    where
        RcString: Borrow<Q>,
        Q: Hash + Eq,
    {
        self.0.remove(key)
    }

    pub fn entry(&mut self, key: RcString) -> std::collections::hash_map::Entry<RcString, Value> {
        self.0.entry(key)
    }
}

impl FromIterator<Value> for Array {
    fn from_iter<T: IntoIterator<Item = Value>>(iter: T) -> Self {
        Array(Vector::from_iter(iter))
    }
}

impl FromIterator<(RcString, Value)> for Object {
    fn from_iter<T: IntoIterator<Item = (RcString, Value)>>(iter: T) -> Self {
        Object(Map::from_iter(iter))
    }
}

impl Deref for Array {
    type Target = [Value];

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

#[derive(
    Clone, Eq, PartialEq, Hash, derive_more::From, derive_more::TryInto, IsVariant, Unwrap,
)]
pub enum Value {
    Null,
    Boolean(bool),
    Number(Number),
    String(RcString),
    Array(Rc<Array>),
    Object(Rc<Object>),
}

impl From<String> for Value {
    fn from(s: String) -> Self {
        Self::String(RcString::new(s))
    }
}

impl From<Array> for Value {
    fn from(arr: Array) -> Self {
        Self::Array(Rc::new(arr))
    }
}

impl From<Object> for Value {
    fn from(obj: Object) -> Self {
        Self::Object(Rc::new(obj))
    }
}

impl Debug for Value {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Null => f.write_str("null"),
            Value::Boolean(v) => f.write_fmt(format_args!("{v:?}")),
            Value::Number(v) => f.write_fmt(format_args!("{v:?}")),
            Value::String(v) => f.write_fmt(format_args!("{v:?}")),
            Value::Array(v) => f.write_fmt(format_args!("{:?}", v.as_ref())),
            Value::Object(v) => f.write_fmt(format_args!("{:?}", v.as_ref())),
        }
    }
}
impl Display for Value {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Null => f.write_str("null"),
            Value::Boolean(v) => f.write_fmt(format_args!("{v:?}")),
            Value::Number(v) => f.write_fmt(format_args!("{v:?}")),
            Value::String(v) => f.write_fmt(format_args!("{v:?}")),
            Value::Array(arr) => {
                f.write_str("[")?;
                let mut first = true;
                for v in arr.as_ref() {
                    if first {
                        first = false;
                    } else {
                        f.write_str(", ")?;
                    }
                    Display::fmt(&v, f)?;
                }
                f.write_str("]")
            }
            Value::Object(obj) => {
                f.write_str("{")?;
                let mut first = true;
                for (k, v) in obj.as_ref() {
                    if first {
                        first = false;
                    } else {
                        f.write_str(", ")?;
                    }
                    f.write_fmt(format_args!("{k}: {v}"))?;
                }
                f.write_str("}")
            }
        }
    }
}

impl Value {
    pub fn number<T: Into<Number>>(n: T) -> Self {
        // TODO: Replace the caller with into
        n.into().into()
    }
    pub fn string(s: String) -> Self {
        // TODO: Replace the caller with into
        s.into()
    }
}

impl Serialize for Value {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        match self {
            Value::Null => serializer.serialize_none(),
            Value::Boolean(v) => serializer.serialize_bool(*v),
            Value::Number(v) => {
                if v.is_finite() {
                    v.serialize(serializer)
                } else if v.is_nan() {
                    serializer.serialize_none()
                } else if v.is_sign_negative() {
                    serializer.serialize_f64(f64::MIN)
                } else {
                    serializer.serialize_f64(f64::MAX)
                }
            }
            Value::String(s) => serializer.serialize_str(s),
            Value::Array(arr) => {
                let mut seq = serializer.serialize_seq(Some(arr.len()))?;
                for e in arr.as_ref() {
                    seq.serialize_element::<Value>(e)?;
                }
                seq.end()
            }
            Value::Object(obj) => {
                let mut map = serializer.serialize_map(Some(obj.len()))?;
                for (k, v) in obj.as_ref() {
                    map.serialize_entry::<String, Value>(k, v)?;
                }
                map.end()
            }
        }
    }
}

impl<'de> Deserialize<'de> for Value {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        struct V;
        impl<'de> Visitor<'de> for V {
            type Value = Value;

            fn expecting(&self, formatter: &mut Formatter) -> std::fmt::Result {
                write!(
                    formatter,
                    "null, boolean, number, string, array, or map keyed with string"
                )
            }

            fn visit_bool<E>(self, v: bool) -> Result<Self::Value, E>
            where
                E: serde::de::Error,
            {
                Ok(v.into())
            }

            fn visit_i64<E>(self, v: i64) -> Result<Self::Value, E>
            where
                E: serde::de::Error,
            {
                Ok(Value::number(v))
            }

            fn visit_u64<E>(self, v: u64) -> Result<Self::Value, E>
            where
                E: serde::de::Error,
            {
                Ok(Value::number(v))
            }

            serde_if_integer128! {
                fn visit_i128<E>(self, v: i128) -> Result<Self::Value, E>
                where
                    E: serde::de::Error,
                {
                    Ok(Value::number(v))
                }

                fn visit_u128<E>(self, v: u128) -> Result<Self::Value, E>
                where
                    E: serde::de::Error,
                {
                    Ok(Value::number(v))
                }
            }

            fn visit_f64<E>(self, v: f64) -> Result<Self::Value, E>
            where
                E: serde::de::Error,
            {
                Ok(Value::number(v))
            }

            fn visit_str<E>(self, v: &str) -> Result<Self::Value, E>
            where
                E: serde::de::Error,
            {
                Ok(v.to_string().into())
            }

            fn visit_string<E>(self, v: String) -> Result<Self::Value, E>
            where
                E: serde::de::Error,
            {
                Ok(v.into())
            }

            fn visit_none<E>(self) -> Result<Self::Value, E>
            where
                E: serde::de::Error,
            {
                Ok(Value::Null)
            }

            fn visit_unit<E>(self) -> Result<Self::Value, E>
            where
                E: Error,
            {
                Ok(Value::Null)
            }

            fn visit_seq<A>(self, mut seq: A) -> Result<Self::Value, A::Error>
            where
                A: SeqAccess<'de>,
            {
                let mut arr = if let Some(n) = seq.size_hint() {
                    Array::with_capacity(n)
                } else {
                    Array::new()
                };
                while let Some(elem) = seq.next_element::<Value>()? {
                    arr.push(elem);
                }
                Ok(arr.into())
            }

            fn visit_map<A>(self, mut map: A) -> Result<Self::Value, A::Error>
            where
                A: MapAccess<'de>,
            {
                let mut obj = if let Some(n) = map.size_hint() {
                    Object::with_capacity(n)
                } else {
                    Object::new()
                };
                while let Some((key, value)) = map.next_entry::<String, Value>()? {
                    obj.insert(key, value);
                }
                Ok(obj.into())
            }
        }
        deserializer.deserialize_any(V)
    }
}

impl PartialOrd for Value {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(Self::cmp(self, other))
    }
}

impl Ord for Value {
    fn cmp(&self, other: &Self) -> Ordering {
        use Ordering::*;
        use Value::*;
        fn type_ord(value: &Value) -> u8 {
            match value {
                Null => 0,
                Boolean(_) => 1,
                Number(_) => 2,
                String(_) => 3,
                Array(_) => 4,
                Object(_) => 5,
            }
        }
        if let res @ (Less | Greater) = type_ord(self).cmp(&type_ord(other)) {
            return res;
        }
        return match (&self, &other) {
            (Null, Null) => Equal,
            (Boolean(lhs), Boolean(rhs)) => Ord::cmp(lhs, rhs),
            (Number(lhs), Number(rhs)) => Ord::cmp(&lhs, &rhs),
            (String(lhs), String(rhs)) => Ord::cmp(&lhs, &rhs),
            (Array(lhs), Array(rhs)) => return Iterator::cmp(lhs.iter(), rhs.iter()),
            (Object(lhs), Object(rhs)) => {
                let lhs_keys = lhs.keys().sorted_unstable().collect_vec();
                let rhs_keys = rhs.keys().sorted_unstable().collect_vec();
                if let res @ (Less | Greater) = Iterator::cmp(lhs_keys.iter(), rhs_keys.iter()) {
                    return res;
                }
                for key in lhs_keys {
                    if let res @ (Less | Greater) = Ord::cmp(&lhs.get(key), &rhs.get(key)) {
                        return res;
                    }
                }
                Equal
            }
            (Null | Boolean(_) | Number(_) | String(_) | Array(_) | Object(_), _) => {
                unreachable!()
            }
        };
    }
}
