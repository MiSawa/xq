use crate::{
    ast::BinaryArithmeticOp,
    vm::{bytecode::NamedFn2, QueryExecutionError},
    Value,
};

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
pub(crate) fn add(_lhs: Value, _rhs: Value) -> Result<Value, QueryExecutionError> {
    todo!()
}

pub(crate) fn subtract(_lhs: Value, _rhs: Value) -> Result<Value, QueryExecutionError> {
    todo!()
}

pub(crate) fn multiply(_lhs: Value, _rhs: Value) -> Result<Value, QueryExecutionError> {
    todo!()
}

pub(crate) fn divide(_lhs: Value, _rhs: Value) -> Result<Value, QueryExecutionError> {
    todo!()
}

pub(crate) fn modulo(_lhs: Value, _rhs: Value) -> Result<Value, QueryExecutionError> {
    todo!()
}
