use anyhow::{anyhow, Context, Result};
use clap::{ArgEnum, Args, Parser, ValueHint};
use clap_verbosity_flag::Verbosity;
use std::{
    io::{stdin, stdout, Write},
    path::PathBuf,
};
use xq::{module_loader::PreludeLoader, run_query, Value};

#[derive(Parser, Debug)]
#[clap(author, about, version)]
#[clap(long_version(option_env!("LONG_VERSION").unwrap_or(env!("CARGO_PKG_VERSION"))))]
struct MainArgs {
    /// The query to run
    #[clap(default_value = ".")]
    query: String,

    /// Read query from a file instead of arg
    #[clap(
        name = "file",
        short = 'f',
        long = "from-file",
        parse(from_os_str),
        conflicts_with = "query",
        value_hint = ValueHint::FilePath
    )]
    query_file: Option<PathBuf>,

    #[clap(flatten)]
    input_format: InputFormatArg,

    #[clap(flatten)]
    output_format: OutputFormatArg,

    #[clap(flatten)]
    verbosity: Verbosity,
}

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Debug, ArgEnum)]
enum InputFormat {
    Null,
    Json,
    Yaml,
}

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Debug, Args)]
struct InputFormatArg {
    /// Specify input format
    #[clap(group = "input", long, arg_enum, default_value_t = InputFormat::Json)]
    input_format: InputFormat,
    /// Use null as an input value
    #[clap(group = "input", short = 'n', long)]
    null_input: bool,
    /// Read input as json values
    #[clap(group = "input", long)]
    json_input: bool,
    /// Read input as yaml values
    #[clap(group = "input", long)]
    yaml_input: bool,
}

impl InputFormatArg {
    fn get(&self) -> InputFormat {
        if self.null_input {
            InputFormat::Null
        } else if self.json_input {
            InputFormat::Json
        } else if self.yaml_input {
            InputFormat::Yaml
        } else {
            self.input_format
        }
    }
}

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Debug, ArgEnum)]
enum OutputFormat {
    Json,
    Yaml,
}

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Debug, Args)]
struct OutputFormatArg {
    /// Specify output format
    #[clap(group = "output", long, arg_enum, default_value_t = OutputFormat::Json)]
    output_format: OutputFormat,
    /// Read output as json values
    #[clap(group = "output", long)]
    json_output: bool,
    /// Read output as yaml values
    #[clap(group = "output", long)]
    yaml_output: bool,

    /// Compact output
    #[clap(short, long, conflicts_with = "yaml-output")]
    compact_output: bool,
    /// Raw string output
    #[clap(short, long, conflicts_with = "yaml-output")]
    raw_string_output: bool,
}

impl OutputFormatArg {
    fn get(&self) -> OutputFormat {
        if self.json_output {
            OutputFormat::Json
        } else if self.yaml_output {
            OutputFormat::Yaml
        } else {
            self.output_format
        }
    }
}

fn init_log(verbosity: &Verbosity) -> Result<()> {
    use simplelog::{ColorChoice, CombinedLogger, Config, TermLogger, TerminalMode};
    let filter = match verbosity.log_level() {
        Some(l) => l.to_level_filter(),
        None => log::LevelFilter::Off,
    };
    CombinedLogger::init(vec![TermLogger::new(
        filter,
        Config::default(),
        TerminalMode::Mixed,
        ColorChoice::Auto,
    )])
    .with_context(|| "Unable to initialize logger")
}

fn main() -> Result<()> {
    let args: MainArgs = MainArgs::parse();
    init_log(&args.verbosity)?;
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
    let output_format = args.output_format.get();
    let output = |value| -> Result<()> {
        match value {
            Ok(value) => match output_format {
                OutputFormat::Json => if args.output_format.compact_output {
                    serde_json::ser::to_writer::<_, Value>(stdout(), &value)
                } else {
                    if args.output_format.raw_string_output && value.is_string() {
                        stdout().write_all(value.unwrap_string().as_bytes()).or(Ok(()))
                    } else {
                        serde_json::ser::to_writer_pretty::<_, Value>(stdout(), &value)
                    }
                }
                .with_context(|| "Write to output"),
                OutputFormat::Yaml => serde_yaml::to_writer::<_, Value>(stdout(), &value)
                    .with_context(|| "Write to output"),
            }
            .and_then(|()| writeln!(stdout()).with_context(|| "Write ln"))?,
            Err(e) => {
                eprintln!("Error: {:?}", e);
            }
        }
        Ok(())
    };

    let module_loader = PreludeLoader();
    match args.input_format.get() {
        InputFormat::Null => {
            let results = run_query(&query, vec![Value::Null].into_iter(), &module_loader)
                .map_err(|e| anyhow!("{:?}", e))
                .with_context(|| "compile query")?;
            for value in results {
                output(value)?;
            }
        }
        InputFormat::Json => {
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
        InputFormat::Yaml => {
            use serde::Deserialize;
            let stdin = stdin();
            let locked = stdin.lock();
            let input: Vec<_> = serde_yaml::Deserializer::from_reader(locked)
                .into_iter()
                .map(Value::deserialize)
                .collect::<Result<_, serde_yaml::Error>>()?;
            let results = run_query(&query, input.into_iter(), &module_loader)
                .map_err(|e| anyhow!("{:?}", e))
                .with_context(|| "compile query")?;
            for value in results {
                output(value)?;
            }
        }
    }
    Ok(())
}
