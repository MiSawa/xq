mod compile;
mod data_structure;
mod intrinsic;
pub mod lang;
pub mod module_loader;
mod number;
mod util;
mod value;
pub mod vm;
pub mod lang2;

pub use crate::{
    number::Number,
    value::{Array, Object, Value},
};

use crate::{
    compile::compiler::{CompileError, Compiler},
    lang::parser,
    module_loader::ModuleLoader,
    vm::{machine::Machine, QueryExecutionError},
};
use thiserror::Error;

#[derive(Debug, Error)]
pub enum XQError {
    #[error(transparent)]
    ParseError(#[from] nom::error::Error<String>),
    #[error(transparent)]
    CompileError(#[from] CompileError),
    #[error(transparent)]
    QueryExecutionError(#[from] QueryExecutionError),
}

pub fn run_query<I, M>(
    query: &str,
    input: I,
    module_loader: &M,
) -> Result<OutputIterator<I>, XQError>
where
    I: Iterator<Item = Value>,
    M: ModuleLoader,
{
    // let now = std::time::Instant::now();
    let parsed = parser::parse_query(query)?;
    log::info!("Parsed query = {:?}", parsed);
    // eprintln!("Parse: {:?}", now.elapsed());
    // let now = std::time::Instant::now();

    let mut compiler = Compiler::new();
    let program = compiler.compile(&parsed, module_loader)?;
    log::info!("Compiled program = {:?}", program);
    // eprintln!("Compile: {:?}", now.elapsed());

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
