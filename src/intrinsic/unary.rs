use crate::{
    Value,
    vm::{bytecode::NamedFn0, QueryExecutionError},
};
use crate::lang::ast::UnaryOp;

pub(crate) fn unary(operator: &UnaryOp) -> NamedFn0 {
    match operator {
        UnaryOp::Plus => NamedFn0 {
            name: "UnaryPlus",
            func: unary_plus,
        },
        UnaryOp::Minus => NamedFn0 {
            name: "UnaryMinus",
            func: unary_minus,
        },
    }
}

fn unary_plus(value: Value) -> Result<Value, QueryExecutionError> {
    match value {
        Value::Number(_) => Ok(value),
        _ => Err(QueryExecutionError::UnaryOnNonNumeric("plus", value)),
    }
}

fn unary_minus(value: Value) -> Result<Value, QueryExecutionError> {
    match value {
        Value::Number(n) => Ok(Value::number(-(*n).clone())),
        _ => Err(QueryExecutionError::UnaryOnNonNumeric("minus", value)),
    }
}
