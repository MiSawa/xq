use num::{bigint::BigInt, FromPrimitive, Num, One, ToPrimitive, Zero};
use std::ops::{
    Add, AddAssign, Div, DivAssign, Mul, MulAssign, Neg, Rem, RemAssign, Sub, SubAssign,
};

#[derive(Debug, Clone, PartialEq)]
pub enum IntOrReal {
    Integer(BigInt),
    Real(f64),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Number(IntOrReal);

// TODO
impl Eq for Number {}

impl Number {
    pub fn from_integer(v: BigInt) -> Self {
        Self(IntOrReal::Integer(v))
    }

    pub fn from_real(v: f64) -> Self {
        let mut value = Self(IntOrReal::Real(v));
        value.normalize();
        value
    }

    pub fn as_int_or_real(&self) -> &IntOrReal {
        &self.0
    }

    fn normalize(&mut self) {
        if let Self(IntOrReal::Real(n)) = self {
            #[allow(clippy::float_cmp)]
            if *n == n.trunc() {
                if let Some(n) = BigInt::from_f64(*n) {
                    *self = Self(IntOrReal::Integer(n))
                }
            }
        }
    }

    fn as_f64(&self) -> f64 {
        match self {
            Self(IntOrReal::Integer(n)) => n.to_f64().unwrap(),
            Self(IntOrReal::Real(n)) => *n,
        }
    }
}

impl Num for Number {
    type FromStrRadixErr = <f64 as Num>::FromStrRadixErr;

    fn from_str_radix(str: &str, radix: u32) -> Result<Self, Self::FromStrRadixErr> {
        BigInt::from_str_radix(str, radix)
            .map(Self::from_integer)
            .or_else(|_| f64::from_str_radix(str, radix).map(Self::from_real))
    }
}

impl From<i64> for Number {
    fn from(n: i64) -> Self {
        Self::from_integer(BigInt::from(n))
    }
}

impl From<f64> for Number {
    fn from(n: f64) -> Self {
        Self::from_real(n)
    }
}

impl FromPrimitive for Number {
    fn from_isize(n: isize) -> Option<Self> {
        Some(Self::from_integer(n.into()))
    }

    fn from_i64(n: i64) -> Option<Self> {
        Some(Self::from_integer(n.into()))
    }

    fn from_i128(n: i128) -> Option<Self> {
        Some(Self::from_integer(n.into()))
    }

    fn from_usize(n: usize) -> Option<Self> {
        Some(Self::from_integer(n.into()))
    }

    fn from_u64(n: u64) -> Option<Self> {
        Some(Self::from_integer(n.into()))
    }

    fn from_u128(n: u128) -> Option<Self> {
        Some(Self::from_integer(n.into()))
    }

    fn from_f64(n: f64) -> Option<Self> {
        Some(Self::from_real(n))
    }
}

impl ToPrimitive for Number {
    fn to_isize(&self) -> Option<isize> {
        match &self.0 {
            IntOrReal::Integer(n) => n.to_isize(),
            IntOrReal::Real(n) => n.to_isize(),
        }
    }

    fn to_i64(&self) -> Option<i64> {
        match &self.0 {
            IntOrReal::Integer(n) => n.to_i64(),
            IntOrReal::Real(n) => n.to_i64(),
        }
    }

    fn to_i128(&self) -> Option<i128> {
        match &self.0 {
            IntOrReal::Integer(n) => n.to_i128(),
            IntOrReal::Real(n) => n.to_i128(),
        }
    }

    fn to_usize(&self) -> Option<usize> {
        match &self.0 {
            IntOrReal::Integer(n) => n.to_usize(),
            IntOrReal::Real(n) => n.to_usize(),
        }
    }

    fn to_u64(&self) -> Option<u64> {
        match &self.0 {
            IntOrReal::Integer(n) => n.to_u64(),
            IntOrReal::Real(n) => n.to_u64(),
        }
    }

    fn to_u128(&self) -> Option<u128> {
        match &self.0 {
            IntOrReal::Integer(n) => n.to_u128(),
            IntOrReal::Real(n) => n.to_u128(),
        }
    }

    fn to_f64(&self) -> Option<f64> {
        match &self.0 {
            IntOrReal::Integer(n) => n.to_f64(),
            IntOrReal::Real(n) => n.to_f64(),
        }
    }
}

impl Zero for Number {
    fn zero() -> Self {
        Self::from_integer(BigInt::zero())
    }

    fn is_zero(&self) -> bool {
        matches!(self, Self(IntOrReal::Integer(v)) if v.is_zero())
    }
}

impl One for Number {
    fn one() -> Self {
        Self::from_integer(BigInt::one())
    }
}

impl Neg for Number {
    type Output = Self;

    fn neg(self) -> Self::Output {
        match self {
            Self(IntOrReal::Integer(v)) => Self(IntOrReal::Integer(-v)),
            Self(IntOrReal::Real(v)) => Self(IntOrReal::Real(-v)),
        }
    }
}

impl Add for Number {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Self(IntOrReal::Integer(lhs)), Self(IntOrReal::Integer(rhs))) => {
                Self::from_integer(lhs + rhs)
            }
            (lhs, rhs) => Self::from_real(lhs.as_f64() + rhs.as_f64()),
        }
    }
}

impl Sub for Number {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Self(IntOrReal::Integer(lhs)), Self(IntOrReal::Integer(rhs))) => {
                Self::from_integer(lhs - rhs)
            }
            (lhs, rhs) => Self::from_real(lhs.as_f64() - rhs.as_f64()),
        }
    }
}

impl Mul for Number {
    type Output = Self;

    fn mul(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Self(IntOrReal::Integer(lhs)), Self(IntOrReal::Integer(rhs))) => {
                Self::from_integer(lhs * rhs)
            }
            (lhs, rhs) => Self::from_real(lhs.as_f64() * rhs.as_f64()),
        }
    }
}

impl Div for Number {
    type Output = Self;

    fn div(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Self(IntOrReal::Integer(lhs)), Self(IntOrReal::Integer(rhs)))
                if (&lhs % &rhs).is_zero() =>
            {
                Self::from_integer(lhs / rhs)
            }
            (lhs, rhs) => Self::from_real(lhs.as_f64() / rhs.as_f64()),
        }
    }
}

impl Rem for Number {
    type Output = Self;

    fn rem(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Self(IntOrReal::Integer(lhs)), Self(IntOrReal::Integer(rhs))) => {
                Self::from_integer(lhs % rhs)
            }
            (lhs, rhs) => Self::from_real(lhs.as_f64() % rhs.as_f64()),
        }
    }
}

impl AddAssign for Number {
    fn add_assign(&mut self, rhs: Self) {
        match (self, rhs) {
            (Self(IntOrReal::Integer(lhs)), Self(IntOrReal::Integer(rhs))) => {
                *lhs += rhs;
            }
            (lhs, rhs) => *lhs = Self::from_real(lhs.as_f64() + rhs.as_f64()),
        }
    }
}

impl SubAssign for Number {
    fn sub_assign(&mut self, rhs: Self) {
        match (self, rhs) {
            (Self(IntOrReal::Integer(lhs)), Self(IntOrReal::Integer(rhs))) => {
                *lhs -= rhs;
            }
            (lhs, rhs) => *lhs = Self::from_real(lhs.as_f64() - rhs.as_f64()),
        }
    }
}

impl MulAssign for Number {
    fn mul_assign(&mut self, rhs: Self) {
        match (self, rhs) {
            (Self(IntOrReal::Integer(lhs)), Self(IntOrReal::Integer(rhs))) => {
                *lhs *= rhs;
            }
            (lhs, rhs) => *lhs = Self::from_real(lhs.as_f64() * rhs.as_f64()),
        }
    }
}

impl DivAssign for Number {
    fn div_assign(&mut self, rhs: Self) {
        match (self, rhs) {
            (Self(IntOrReal::Integer(lhs)), Self(IntOrReal::Integer(rhs))) => {
                *lhs /= rhs;
            }
            (lhs, rhs) => *lhs = Self::from_real(lhs.as_f64() / rhs.as_f64()),
        }
    }
}

impl RemAssign for Number {
    fn rem_assign(&mut self, rhs: Self) {
        match (self, rhs) {
            (Self(IntOrReal::Integer(lhs)), Self(IntOrReal::Integer(rhs))) => {
                *lhs %= rhs;
            }
            (lhs, rhs) => *lhs = Self::from_real(lhs.as_f64() % rhs.as_f64()),
        }
    }
}
