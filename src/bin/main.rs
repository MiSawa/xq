use anyhow::{Context, Result};
use clap::{AppSettings, Clap, ValueHint};
use std::path::PathBuf;

#[derive(Clap)]
#[clap(author, about, version)]
#[clap(long_version(option_env!("LONG_VERSION").unwrap_or(env!("CARGO_PKG_VERSION"))))]
#[clap(setting(AppSettings::ColoredHelp))]
struct Opts {
    #[clap(default_value("."))]
    query: String,
    #[clap(short('f'), long("from-file"), parse(from_os_str), conflicts_with("query"), value_hint(ValueHint::FilePath))]
    query_file: Option<PathBuf>,
}

fn main() -> Result<()> {
    let opts: Opts = Opts::parse();
    let query = if let Some(path) = opts.query_file {
        std::fs::read_to_string(path)?
    } else {
        opts.query
    };
    let ast = xq::parser::parse_query(&query).with_context(|| "Parse query")?;
    println!("{:?}", ast);
    Ok(())
}
