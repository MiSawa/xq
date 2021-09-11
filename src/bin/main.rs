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

fn init_log(verbosity: u8) -> Result<()> {
    // TODO: Use https://github.com/rust-cli/clap-verbosity-flag if it supports clap_derive at some day, or find an alternative.
    use log::LevelFilter::*;
    use simplelog::{ColorChoice, CombinedLogger, Config, TermLogger, TerminalMode};
    let levels = [Off, Error, Warn, Info, Debug, Trace];
    let level = levels[(verbosity as usize).clamp(0, levels.len() - 1)];
    CombinedLogger::init(vec![TermLogger::new(
        level,
        Config::default(),
        TerminalMode::Mixed,
        ColorChoice::Auto,
    )])
    .with_context(|| "Unable to initialize logger")
}

fn main() -> Result<()> {
    let args: Args = Args::parse();
    init_log(args.verbosity)?;
    log::debug!("Parsed argument: {:?}", args);
    let query = if let Some(path) = args.query_file {
        log::trace!("Read query from file {:?}", path);
        std::fs::read_to_string(path)?
    } else {
        log::trace!(
            "Read from query in arg (if it wasn't the default value): `{}`",
            args.query
        );
        args.query
    };
    let ast = xq::parser::parse_query(&query).with_context(|| "Parse query")?;
    log::info!("Parsed query = {:?}", ast);
    Ok(())
}
