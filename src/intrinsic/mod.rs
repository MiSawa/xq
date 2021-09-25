use crate::vm::Value;

mod binary;
mod comparator;
mod index;
mod unary;

pub(crate) use self::{
    binary::binary,
    comparator::comparator,
    index::{index, slice},
    unary::unary,
};

pub(crate) fn truthy(value: Value) -> bool {
    !matches!(value, Value::Null | Value::False)
}
