use crate::{
    lang::{ast::Program, parse_program, ParseError},
    module_loader::ModuleLoadError::NotFoundError,
    Value,
};
use thiserror::Error;

#[derive(Debug, Error)]
pub enum ModuleLoadError {
    #[error("Module `{0:?}` not found")]
    NotFoundError(String),
    #[error(transparent)]
    ParseError(#[from] ParseError),
    #[error(transparent)]
    IOError(#[from] std::io::Error),
}

type Result<T> = std::result::Result<T, ModuleLoadError>;

pub trait ModuleLoader {
    fn prelude(&self) -> Result<Vec<Program>>;
    fn load_values(&self, path: &str, search: Option<Vec<String>>) -> Result<Vec<Value>>;
    fn load_program(&self, path: &str, search: Option<Vec<String>>) -> Result<Program>;
}

pub struct PreludeLoader();
impl ModuleLoader for PreludeLoader {
    fn prelude(&self) -> Result<Vec<Program>> {
        let prelude = include_str!("../prelude.jq");
        let parsed = parse_program(prelude)?;
        Ok(vec![parsed])
    }

    fn load_values(&self, path: &str, _search: Option<Vec<String>>) -> Result<Vec<Value>> {
        Err(NotFoundError(path.to_string()))
    }

    fn load_program(&self, path: &str, _search: Option<Vec<String>>) -> Result<Program> {
        Err(NotFoundError(path.to_string()))
    }
}
