use crate::{
    ast::BinaryOp,
    vm::{bytecode::NamedFn2, QueryExecutionError, Value},
};

pub(crate) fn binary(operator: &BinaryOp) -> NamedFn2 {
    match operator {
        BinaryOp::Add => NamedFn2 {
            name: "Add",
            func: Box::new(add),
        },
        BinaryOp::Subtract => NamedFn2 {
            name: "Subtract",
            func: Box::new(subtract),
        },
        BinaryOp::Multiply => NamedFn2 {
            name: "Multiply",
            func: Box::new(multiply),
        },
        BinaryOp::Divide => NamedFn2 {
            name: "Divide",
            func: Box::new(divide),
        },
        BinaryOp::Modulo => NamedFn2 {
            name: "Modulo",
            func: Box::new(modulo),
        },
        BinaryOp::Alt => NamedFn2 {
            name: "Alt",
            func: Box::new(alt),
        },
        BinaryOp::And => NamedFn2 {
            name: "And",
            func: Box::new(and),
        },
        BinaryOp::Or => NamedFn2 {
            name: "Or",
            func: Box::new(or),
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

pub(crate) fn alt(_lhs: Value, _rhs: Value) -> Result<Value, QueryExecutionError> {
    todo!()
}

pub(crate) fn and(_lhs: Value, _rhs: Value) -> Result<Value, QueryExecutionError> {
    todo!()
}

pub(crate) fn or(_lhs: Value, _rhs: Value) -> Result<Value, QueryExecutionError> {
    todo!()
}
