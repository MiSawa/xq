pub mod ast;
pub mod parser;
pub mod runner;

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum Number {
    Integer(i64),
    Real(f64),
}

// FIXME: Eq on real...
impl Eq for Number {}

impl From<i64> for Number {
    fn from(value: i64) -> Self {
        Self::Integer(value)
    }
}

impl From<f64> for Number {
    fn from(value: f64) -> Self {
        Self::Real(value)
    }
}
