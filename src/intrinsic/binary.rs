use std::collections::HashSet;

use num::{Float, ToPrimitive, Zero};
use xq_lang::ast::BinaryArithmeticOp;

use crate::{
    intrinsic::string,
    vm::{bytecode::NamedFn1, QueryExecutionError},
    Number, Value,
};

pub(crate) fn binary(operator: &BinaryArithmeticOp) -> NamedFn1 {
    // NOTE: Because of the evaluation order, lhs and rhs are flipped here.
    match operator {
        BinaryArithmeticOp::Add => NamedFn1 {
            name: "Add",
            func: |rhs, lhs| add(lhs, rhs),
        },
        BinaryArithmeticOp::Subtract => NamedFn1 {
            name: "Subtract",
            func: |rhs, lhs| subtract(lhs, rhs),
        },
        BinaryArithmeticOp::Multiply => NamedFn1 {
            name: "Multiply",
            func: |rhs, lhs| multiply(lhs, rhs),
        },
        BinaryArithmeticOp::Divide => NamedFn1 {
            name: "Divide",
            func: |rhs, lhs| divide(lhs, rhs),
        },
        BinaryArithmeticOp::Modulo => NamedFn1 {
            name: "Modulo",
            func: |rhs, lhs| modulo(lhs, rhs),
        },
    }
}

fn add(lhs: Value, rhs: Value) -> Result<Value, QueryExecutionError> {
    use Value::*;
    Ok(match (lhs, rhs) {
        (Null, rhs) => rhs,
        (lhs, Null) => lhs,
        (Number(lhs), Number(rhs)) => (lhs + rhs).into(),
        (String(lhs), String(rhs)) => ((*lhs).clone() + &*rhs).into(),
        (Array(lhs), Array(rhs)) => lhs
            .iter()
            .chain(rhs.iter())
            .cloned()
            .collect::<crate::Array>()
            .into(),
        (Object(lhs), Object(rhs)) => lhs
            .iter()
            .chain(rhs.iter())
            .map(|(k, v)| (k.clone(), v.clone())) // TODO: Better way?
            .collect::<crate::Object>()
            .into(),
        (lhs @ (Boolean(_) | Number(_) | String(_) | Array(_) | Object(_)), rhs) => {
            return Err(QueryExecutionError::IncompatibleBinaryOperator(
                "add", lhs, rhs,
            ));
        }
    })
}

fn subtract(lhs: Value, rhs: Value) -> Result<Value, QueryExecutionError> {
    use Value::*;
    Ok(match (lhs, rhs) {
        (Number(lhs), Number(rhs)) => Value::number(lhs - rhs),
        (Array(lhs), Array(rhs)) => {
            let set: HashSet<_> = rhs.iter().collect();
            let iter = lhs.iter().filter(|v| !set.contains(v));
            iter.cloned().collect::<crate::Array>().into()
        }
        (lhs @ (Null | Boolean(_) | Number(_) | String(_) | Array(_) | Object(_)), rhs) => {
            return Err(QueryExecutionError::IncompatibleBinaryOperator(
                "subtract", lhs, rhs,
            ));
        }
    })
}

fn multiply(lhs: Value, rhs: Value) -> Result<Value, QueryExecutionError> {
    use Value::*;
    fn merge(lhs: Value, rhs: Value) -> Value {
        match (lhs, rhs) {
            (Object(lhs), Object(rhs)) => {
                let mut lhs = (*lhs).clone();
                for (k, r) in rhs.iter() {
                    lhs.entry(k.clone())
                        .and_modify(|l| {
                            let tmp = std::mem::replace(l, Value::Null);
                            *l = merge(tmp, r.clone())
                        })
                        .or_insert_with(|| r.clone());
                }
                lhs.into()
            }
            (_, rhs) => rhs,
        }
    }
    Ok(match (lhs, rhs) {
        (Number(lhs), Number(rhs)) => Value::number(lhs * rhs),
        (String(lhs), Number(rhs)) => {
            if rhs <= Zero::zero() || rhs.is_nan() {
                Value::Null
            } else {
                let repeat = rhs
                    .to_usize()
                    .ok_or(QueryExecutionError::StringRepeatByNonUSize(rhs))?;
                Value::string((*lhs).clone().repeat(repeat))
            }
        }
        (lhs @ Number(_), rhs @ String(_)) => multiply(rhs, lhs)?,
        (lhs @ Object(_), rhs @ Object(_)) => merge(lhs, rhs),
        (lhs @ (Null | Boolean(_) | Number(_) | String(_) | Array(_) | Object(_)), rhs) => {
            return Err(QueryExecutionError::IncompatibleBinaryOperator(
                "multiply", lhs, rhs,
            ));
        }
    })
}

fn divide(lhs: Value, rhs: Value) -> Result<Value, QueryExecutionError> {
    use Value::*;
    Ok(match (lhs, rhs) {
        (Number(lhs), Number(rhs)) => {
            if rhs.is_zero() {
                return Err(QueryExecutionError::DivModByZero);
            }
            Value::number(lhs / rhs)
        }
        (String(lhs), String(rhs)) => string::split(lhs.as_ref(), rhs.as_ref()),
        (lhs @ (Null | Boolean(_) | Number(_) | String(_) | Array(_) | Object(_)), rhs) => {
            return Err(QueryExecutionError::IncompatibleBinaryOperator(
                "divide", lhs, rhs,
            ));
        }
    })
}

fn modulo(lhs: Value, rhs: Value) -> Result<Value, QueryExecutionError> {
    use Value::*;
    Ok(match (lhs, rhs) {
        (Number(lhs), Number(rhs)) => {
            let rhs = number_to_i64(rhs);
            if rhs == 0 {
                return Err(QueryExecutionError::DivModByZero);
            }
            Value::number(number_to_i64(lhs) % rhs)
        }
        (lhs @ (Null | Boolean(_) | Number(_) | String(_) | Array(_) | Object(_)), rhs) => {
            return Err(QueryExecutionError::IncompatibleBinaryOperator(
                "modulo", lhs, rhs,
            ));
        }
    })
}

fn number_to_i64(n: Number) -> i64 {
    n.to_i64()
        .unwrap_or(if n > Zero::zero() { i64::MAX } else { i64::MIN })
}
