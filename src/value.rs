use crate::{
    data_structure::{PHashMap, PVector},
    number::IntOrReal,
    Number,
};
use num::ToPrimitive;
use serde::{
    de::{Error, MapAccess, SeqAccess, Visitor},
    ser::{SerializeMap, SerializeSeq},
    Deserialize, Deserializer, Serialize, Serializer,
};
use std::{borrow::Borrow, collections::HashMap, fmt::Formatter, rc::Rc};

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Value {
    Null,
    True,
    False,
    Number(Rc<Number>),
    String(Rc<String>),
    Array(PVector<Value>),
    Object(PHashMap<Rc<String>, Value>),
}

impl Value {
    pub fn number(n: Number) -> Self {
        Self::Number(Rc::new(n))
    }
    pub fn string(s: String) -> Self {
        Self::String(Rc::new(s))
    }
}

impl Serialize for Value {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        match self {
            Value::Null => serializer.serialize_none(),
            Value::True => serializer.serialize_bool(true),
            Value::False => serializer.serialize_bool(false),
            Value::Number(v) => {
                match v.as_int_or_real() {
                    IntOrReal::Integer(v) => serializer.serialize_i64(v.to_i64().unwrap()), // TODO
                    IntOrReal::Real(v) => serializer.serialize_f64(*v),
                }
            }
            Value::String(s) => serializer.serialize_str(s),
            Value::Array(v) => {
                let mut seq = serializer.serialize_seq(Some(v.len()))?;
                for e in v {
                    seq.serialize_element::<Value>(e.borrow())?;
                }
                seq.end()
            }
            Value::Object(values) => {
                let mut map = serializer.serialize_map(Some(values.len()))?;
                for (k, v) in values {
                    map.serialize_entry::<String, Value>(k.borrow(), v.borrow())?;
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
                if v {
                    Ok(Value::True)
                } else {
                    Ok(Value::False)
                }
            }

            fn visit_i64<E>(self, v: i64) -> Result<Self::Value, E>
            where
                E: serde::de::Error,
            {
                Ok(Value::number(Number::from_integer(v.into())))
            }

            fn visit_u64<E>(self, v: u64) -> Result<Self::Value, E>
            where
                E: serde::de::Error,
            {
                Ok(Value::number(Number::from_integer(v.into())))
            }

            fn visit_f64<E>(self, v: f64) -> Result<Self::Value, E>
            where
                E: serde::de::Error,
            {
                Ok(Value::number(Number::from_real(v)))
            }

            fn visit_str<E>(self, v: &str) -> Result<Self::Value, E>
            where
                E: serde::de::Error,
            {
                Ok(Value::string(v.to_string()))
            }

            fn visit_string<E>(self, v: String) -> Result<Self::Value, E>
            where
                E: serde::de::Error,
            {
                Ok(Value::string(v))
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
                let mut v = if let Some(n) = seq.size_hint() {
                    Vec::with_capacity(n)
                } else {
                    vec![]
                };
                while let Some(elem) = seq.next_element::<Value>()? {
                    v.push(elem);
                }
                Ok(Value::Array(PVector::from(v)))
            }

            fn visit_map<A>(self, mut map: A) -> Result<Self::Value, A::Error>
            where
                A: MapAccess<'de>,
            {
                let mut m = if let Some(n) = map.size_hint() {
                    HashMap::with_capacity(n)
                } else {
                    HashMap::new()
                };
                while let Some((key, value)) = map.next_entry::<String, Value>()? {
                    m.insert(Rc::new(key), value);
                }
                Ok(Value::Object(PHashMap::from(m)))
            }
        }
        deserializer.deserialize_any(V)
    }
}
