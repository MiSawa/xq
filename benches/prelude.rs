use std::time::Duration;

use criterion::{criterion_group, criterion_main, BatchSize, Criterion};

const PRELUDE: &str = include_str!("../prelude.jq");

struct NullModuleLoader;
impl xq::module_loader::ModuleLoader for NullModuleLoader {
    fn prelude(&self) -> xq::module_loader::Result<Vec<xq_lang::ast::Program>> {
        Ok(vec![])
    }

    fn load_values(
        &self,
        path: &str,
        _search: Option<Vec<String>>,
    ) -> xq::module_loader::Result<Vec<xq::Value>> {
        Err(xq::module_loader::ModuleLoadError::NotFoundError(
            path.to_string(),
        ))
    }

    fn load_program(
        &self,
        path: &str,
        _search: Option<Vec<String>>,
    ) -> xq::module_loader::Result<xq_lang::ast::Program> {
        Err(xq::module_loader::ModuleLoadError::NotFoundError(
            path.to_string(),
        ))
    }
}

fn tokenize(c: &mut Criterion) {
    c.bench_function("tokenize", |b| {
        b.iter(|| {
            xq_lang::lexer::Lexer::new(PRELUDE)
                .into_iter()
                .collect::<Vec<_>>()
        })
    });
}

fn parse(c: &mut Criterion) {
    let tokens: Vec<_> = xq_lang::lexer::Lexer::new(PRELUDE).into_iter().collect();

    c.bench_function("parse", |b| {
        b.iter_batched(
            || tokens.clone(),
            |tokens| xq_lang::parser::ProgramParser::new().parse(PRELUDE, tokens.into_iter()),
            BatchSize::SmallInput,
        )
    });
}

fn tokenize_and_parse(c: &mut Criterion) {
    c.bench_function("tokenize_and_parse", |b| {
        b.iter(|| xq_lang::parse_program(PRELUDE).unwrap())
    });
}

fn compile(c: &mut Criterion) {
    let program = xq_lang::parse_program(PRELUDE).unwrap();

    c.bench_function("compile", |b| {
        b.iter(|| {
            let mut compiler = xq::compile::compiler::Compiler::new();
            compiler.compile(&program, &NullModuleLoader).unwrap()
        })
    });
}

fn full(c: &mut Criterion) {
    c.bench_function("full", |b| {
        b.iter(|| {
            let program = xq_lang::parse_program(PRELUDE).unwrap();
            let mut compiler = xq::compile::compiler::Compiler::new();
            compiler.compile(&program, &NullModuleLoader).unwrap()
        })
    });
}

criterion_group! {
    name = benches;
    config = Criterion::default()
        .warm_up_time(Duration::from_secs(10))
        .measurement_time(Duration::from_secs(10));
    targets = tokenize, parse, tokenize_and_parse, compile, full
}
criterion_main!(benches);
