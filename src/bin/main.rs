use anyhow::{Context, Result};
use clap::{AppSettings, Clap, ValueHint};
use std::path::PathBuf;

#[derive(Clap, Debug)]
#[clap(author, about, version)]
#[clap(long_version(option_env!("LONG_VERSION").unwrap_or(env!("CARGO_PKG_VERSION"))))]
#[clap(setting(AppSettings::ColoredHelp))]
struct Args {
    /// The query to run
    #[clap(default_value("."))]
    query: String,

    /// Read query from a file instead of arg
    #[clap(
        name("file"),
        short('f'),
        long("from-file"),
        parse(from_os_str),
        conflicts_with("query"),
        value_hint(ValueHint::FilePath)
    )]
    query_file: Option<PathBuf>,

    /// Verbose mode (-v, -vv, -vvv, etc.)
    #[clap(short('v'), long("verbose"), parse(from_occurrences))]
    verbosity: u8,
}

fn main() -> Result<()> {
    let args: Args = Args::parse();
    if args.verbosity > 2 {
        eprintln!("{:?}", args);
    }
    let query = if let Some(path) = args.query_file {
        std::fs::read_to_string(path)?
    } else {
        args.query
    };
    let ast = xq::parser::parse_query(&query).with_context(|| "Parse query")?;
    println!("{:?}", ast);
    Ok(())
}
