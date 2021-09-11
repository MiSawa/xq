use anyhow::{Context, Result};
use clap::{AppSettings, Clap, ValueHint};
use std::path::PathBuf;

#[derive(Clap)]
#[clap(author, about, version)]
#[clap(long_version(option_env!("LONG_VERSION").unwrap_or(env!("CARGO_PKG_VERSION"))))]
#[clap(setting(AppSettings::ColoredHelp))]
struct Opts {
    query: Option<String>,
    #[clap(short, long, parse(from_os_str), conflicts_with("query"), value_hint = ValueHint::FilePath)]
    from_file: Option<PathBuf>,
}

fn main() -> Result<()> {
    let opts: Opts = Opts::parse();
    let query = if let Some(query) = opts.query {
        query
    } else {
        std::fs::read_to_string(opts.from_file.expect("Query should be given either as an argument or via -f"))?
    };
    let ast = xq::parser::parse_query(&query).with_context(|| "Parse query")?;
    println!("{:?}", ast);
    Ok(())
}
