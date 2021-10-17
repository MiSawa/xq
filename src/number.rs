use cast::i32;
use derive_more::{DebugCustom, Display};
use ordered_float::OrderedFloat;
use serde::{serde_if_integer128, Deserialize, Deserializer, Serialize, Serializer};
use std::{fmt::Formatter, ops::Neg, str::FromStr};

pub(crate) type PrimitiveReal = f64;

#[derive(
    Copy,
    Clone,
    Eq,
    PartialEq,
    Ord,
    PartialOrd,
    Hash,
    DebugCustom,
    Display,
    num_derive::FromPrimitive,
    num_derive::ToPrimitive,
    num_derive::NumCast,
    num_derive::NumOps,
    num_derive::Zero,
    num_derive::One,
    num_derive::Num,
    num_derive::Float,
)]
#[debug(fmt = "{}", _0)]
#[display(fmt = "{}", _0)]
pub struct Number(OrderedFloat<PrimitiveReal>);

impl Number {
    pub(crate) fn to_primitive_real(self) -> PrimitiveReal {
        self.0 .0
    }
}

impl FromStr for Number {
    type Err = <PrimitiveReal as FromStr>::Err;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        PrimitiveReal::from_str(s).map(|n| Number(OrderedFloat(n)))
    }
}

impl Neg for Number {
    type Output = Self;
    fn neg(self) -> Self::Output {
        Self(-self.0)
    }
}

impl<T> From<T> for Number
where
    PrimitiveReal: cast::From<T, Output = PrimitiveReal>,
{
    fn from(v: T) -> Self {
        use cast::From;
        Self(OrderedFloat(PrimitiveReal::cast(v)))
    }
}

impl Serialize for Number {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        // There's no `is_integral()`.
        // `.fract().is_zero()` also works but `.fract()` is implemented by `self - self.trunc()`.
        #[allow(clippy::float_cmp)]
        if self.0 .0.trunc() == self.0 .0 {
            if let Ok(v) = i32(self.0 .0) {
                return serializer.serialize_i32(v);
            }
        }
        serializer.serialize_f64(self.0 .0)
    }
}

impl<'de> Deserialize<'de> for Number {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        struct V;
        impl<'de> serde::de::Visitor<'de> for V {
            type Value = Number;

            fn expecting(&self, formatter: &mut Formatter) -> std::fmt::Result {
                write!(formatter, "null, number")
            }

            fn visit_i64<E>(self, v: i64) -> Result<Self::Value, E>
            where
                E: serde::de::Error,
            {
                Ok(v.into())
            }

            fn visit_u64<E>(self, v: u64) -> Result<Self::Value, E>
            where
                E: serde::de::Error,
            {
                Ok(v.into())
            }

            serde_if_integer128! {
                fn visit_i128<E>(self, v: i128) -> Result<Self::Value, E>
                where
                    E: serde::de::Error,
                {
                    Ok(v.into())
                }

                fn visit_u128<E>(self, v: u128) -> Result<Self::Value, E>
                where
                    E: serde::de::Error,
                {
                    Ok(v.into())
                }
            }

            fn visit_f64<E>(self, v: f64) -> Result<Self::Value, E>
            where
                E: serde::de::Error,
            {
                Ok(v.into())
            }
        }
        deserializer.deserialize_any(V)
    }
}
