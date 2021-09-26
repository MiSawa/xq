use anyhow::{anyhow, Context, Result};
use clap::{AppSettings, Clap, ValueHint};
use std::{
    io::{stdin, stdout, Write},
    path::PathBuf,
};
use xq::{module_loader::PreludeLoader, run_query, Value};

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

    /// Use null as an input value
    #[clap(short, long)]
    null_input: bool,

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
    let output = |value| -> Result<()> {
        match value {
            Ok(value) => serde_json::ser::to_writer_pretty::<_, Value>(stdout(), &value)
                .with_context(|| "Write to output")
                .and_then(|()| writeln!(stdout()).with_context(|| "Write ln"))?,
            Err(e) => {
                log::error!("Error: {:?}", e)
            }
        }
        Ok(())
    };

    let module_loader = PreludeLoader();
    if args.null_input {
        let results = run_query(&query, vec![Value::Null].into_iter(), &module_loader)
            .map_err(|e| anyhow!("{:?}", e))
            .with_context(|| "compile query")?;
        for value in results {
            output(value)?;
        }
    } else {
        let stdin = stdin();
        let locked = stdin.lock();
        let input: Vec<_> = serde_json::de::Deserializer::from_reader(locked)
            .into_iter::<Value>()
            .collect::<Result<_, serde_json::Error>>()?;
        let results = run_query(&query, input.into_iter(), &module_loader)
            .map_err(|e| anyhow!("{:?}", e))
            .with_context(|| "compile query")?;
        for value in results {
            output(value)?;
        }
    }
    Ok(())
}
