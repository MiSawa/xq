use crate::{
    ast::BinaryArithmeticOp,
    vm::{bytecode::NamedFn2, error::QueryExecutionError::DivModByZero, QueryExecutionError},
    Value,
};
use num::{ToPrimitive, Zero};
use QueryExecutionError::{IncompatibleBinaryOperator, StringRepeatByNonUSize};

pub(crate) fn binary(operator: &BinaryArithmeticOp) -> NamedFn2 {
    match operator {
        BinaryArithmeticOp::Add => NamedFn2 {
            name: "Add",
            func: Box::new(add),
        },
        BinaryArithmeticOp::Subtract => NamedFn2 {
            name: "Subtract",
            func: Box::new(subtract),
        },
        BinaryArithmeticOp::Multiply => NamedFn2 {
            name: "Multiply",
            func: Box::new(multiply),
        },
        BinaryArithmeticOp::Divide => NamedFn2 {
            name: "Divide",
            func: Box::new(divide),
        },
        BinaryArithmeticOp::Modulo => NamedFn2 {
            name: "Modulo",
            func: Box::new(modulo),
        },
    }
}

fn add(lhs: Value, rhs: Value) -> Result<Value, QueryExecutionError> {
    use Value::*;
    Ok(match (lhs, rhs) {
        (Null, rhs) => rhs,
        (lhs, Null) => lhs,
        (Number(lhs), Number(rhs)) => Value::number((*lhs).clone() + (*rhs).clone()), // TODO: Fix rhs.clone()
        (String(lhs), String(rhs)) => Value::string((*lhs).clone() + &*rhs),
        (Array(lhs), Array(rhs)) => Value::Array(lhs + rhs),
        (Object(lhs), Object(rhs)) => Value::Object(rhs.union(lhs)),
        (lhs @ (True | False | Number(_) | String(_) | Array(_) | Object(_)), rhs) => {
            return Err(IncompatibleBinaryOperator("add", lhs, rhs));
        }
    })
}

fn subtract(lhs: Value, rhs: Value) -> Result<Value, QueryExecutionError> {
    use Value::*;
    Ok(match (lhs, rhs) {
        (Number(lhs), Number(rhs)) => Value::number((*lhs).clone() - (*rhs).clone()), // TODO: Fix rhs.clone()
        (Array(lhs), Array(rhs)) => {
            let iter = lhs.into_iter().filter(|v| !rhs.contains(v)); // TODO: O(n log n) or expected O(n) instead of O(n^2). NaN though...
            Array(iter.collect())
        }
        (lhs @ (Null | True | False | Number(_) | String(_) | Array(_) | Object(_)), rhs) => {
            return Err(IncompatibleBinaryOperator("add", lhs, rhs));
        }
    })
}

fn multiply(lhs: Value, rhs: Value) -> Result<Value, QueryExecutionError> {
    use Value::*;
    fn merge(lhs: Value, rhs: Value) -> Value {
        match (lhs, rhs) {
            (Object(lhs), Object(rhs)) => Object(lhs.union_with(rhs, merge)),
            (_, rhs) => rhs,
        }
    }
    Ok(match (lhs, rhs) {
        (Number(lhs), Number(rhs)) => Value::number((*lhs).clone() * (*rhs).clone()), // TODO: Fix rhs.clone()
        (String(lhs), Number(rhs)) => {
            let repeat = rhs
                .to_usize()
                .ok_or_else(|| StringRepeatByNonUSize((*rhs).clone()))?;
            if repeat == 0 {
                Value::Null // Why not ""....
            } else {
                Value::string((*lhs).clone().repeat(repeat))
            }
        }
        (lhs @ Object(_), rhs @ Object(_)) => merge(lhs, rhs),
        (lhs @ (Null | True | False | Number(_) | String(_) | Array(_) | Object(_)), rhs) => {
            return Err(IncompatibleBinaryOperator("add", lhs, rhs));
        }
    })
}

fn divide(lhs: Value, rhs: Value) -> Result<Value, QueryExecutionError> {
    use Value::*;
    Ok(match (lhs, rhs) {
        (Number(lhs), Number(rhs)) => {
            if rhs.is_zero() {
                return Err(DivModByZero);
            }
            Value::number((*lhs).clone() / (*rhs).clone()) // TODO: Fix rhs.clone()
        }
        (String(lhs), String(rhs)) => Array(
            lhs.split(&*rhs)
                .into_iter()
                .map(|s| Value::string(s.to_string()))
                .collect(),
        ),
        (lhs @ (Null | True | False | Number(_) | String(_) | Array(_) | Object(_)), rhs) => {
            return Err(IncompatibleBinaryOperator("add", lhs, rhs));
        }
    })
}

fn modulo(lhs: Value, rhs: Value) -> Result<Value, QueryExecutionError> {
    use Value::*;
    Ok(match (lhs, rhs) {
        (Number(lhs), Number(rhs)) => {
            if rhs.is_zero() {
                return Err(DivModByZero);
            }
            Value::number((*lhs).clone() % (*rhs).clone()) // TODO: Fix rhs.clone()
        }
        (lhs @ (Null | True | False | Number(_) | String(_) | Array(_) | Object(_)), rhs) => {
            return Err(IncompatibleBinaryOperator("add", lhs, rhs));
        }
    })
}