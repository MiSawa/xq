use thiserror::Error;

use crate::vm::{
    compiler::{CompileError, Compiler},
    machine::Machine,
    QueryExecutionError,
};
pub use number::{IntOrReal, Number};
pub use value::Value;

pub mod ast;
mod data_structure;
mod intrinsic;
mod number;
pub mod parser;
pub mod value;
pub mod vm;

#[derive(Debug, Error)]
pub enum XQError {
    #[error(transparent)]
    ParseError(#[from] nom::error::Error<String>),
    #[error(transparent)]
    CompileError(#[from] CompileError),
    #[error(transparent)]
    QueryExecutionError(#[from] QueryExecutionError),
}

pub fn run_query<I>(query: &str, input: I) -> Result<OutputIterator<I>, XQError>
where
    I: Iterator<Item = Value>,
{
    let parsed = parser::parse_query(query)?;
    log::info!("Parsed query = {:?}", parsed);

    let mut compiler = Compiler::new();
    let program = compiler.compile(&parsed)?;
    log::info!("Compiled program = {:?}", program);

    let vm = Machine::new(program);
    Ok(OutputIterator::new(vm, input))
}

pub struct OutputIterator<I: Iterator<Item = Value>> {
    finished: bool,
    machine: Machine,
    results: Option<vm::machine::ResultIterator>,
    input: I,
}

impl<I: Iterator<Item = Value>> OutputIterator<I> {
    fn new(machine: Machine, input: I) -> Self {
        Self {
            finished: false,
            machine,
            results: None,
            input,
        }
    }
}

impl<I: Iterator<Item = Value>> Iterator for OutputIterator<I> {
    type Item = Result<Value, QueryExecutionError>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.finished {
            return None;
        }
        loop {
            if let Some(it) = &mut self.results {
                if let Some(value) = it.next() {
                    return Some(value);
                } else {
                    self.results = None;
                }
            }
            if let Some(value) = self.input.next() {
                self.results = Some(self.machine.run(value))
            } else {
                self.finished = true;
                return None;
            }
        }
    }
}
