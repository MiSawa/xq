mod compile;
mod data_structure;
mod intrinsic;
pub mod module_loader;
mod number;
pub mod util;
mod value;
pub mod vm;

use thiserror::Error;
use vm::machine::ResultIterator;
use xq_lang::ParseError;

use crate::{
    compile::compiler::{CompileError, Compiler},
    module_loader::ModuleLoader,
    vm::{machine::Machine, QueryExecutionError},
};
pub use crate::{
    number::Number,
    value::{Array, Object, Value},
};

pub type InputError = vm::error::InputError;

#[derive(Debug, Error)]
pub enum XQError {
    #[error(transparent)]
    ParseError(#[from] ParseError),
    #[error(transparent)]
    CompileError(#[from] CompileError),
    #[error(transparent)]
    QueryExecutionError(#[from] QueryExecutionError),
}

pub fn run_query<C, I, M>(
    query: &str,
    context: C,
    input: I,
    module_loader: &M,
) -> Result<ResultIterator<C, I>, XQError>
where
    C: Iterator<Item = Result<Value, InputError>>,
    I: Iterator<Item = Result<Value, InputError>>,
    M: ModuleLoader,
{
    // let now = std::time::Instant::now();
    let parsed = xq_lang::parse_program(query)?;
    log::info!("Parsed query = {:?}", parsed);
    // eprintln!("Parse: {:?}", now.elapsed());
    // let now = std::time::Instant::now();

    let mut compiler = Compiler::new();
    let program = compiler.compile(&parsed, module_loader)?;
    log::info!("Compiled program = {:?}", program);
    // eprintln!("Compile: {:?}", now.elapsed());

    let mut vm = Machine::new(program);
    Ok(vm.start(context, input))
}
