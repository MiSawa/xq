use std::{
    io::{stdin, stdout, BufRead, Read, Write},
    path::PathBuf,
};

use anyhow::{anyhow, Context, Result};
use clap::{ArgEnum, Args, Parser, ValueHint};
use clap_verbosity_flag::Verbosity;
use cli::input::Input;
use xq::{module_loader::PreludeLoader, run_query, InputError, Value};

use crate::cli::input::Tied;

mod cli;

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
enum SerializationFormat {
    Json,
    Yaml,
}

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Debug, Args)]
struct InputFormatArg {
    /// Specify input format
    #[clap(group = "input", long, arg_enum, default_value_t = SerializationFormat::Json)]
    input_format: SerializationFormat,
    /// Read input as json values
    #[clap(group = "input", long)]
    json_input: bool,
    /// Read input as yaml values
    #[clap(group = "input", long)]
    yaml_input: bool,

    /// Single null is supplied to the program.
    /// The original input can still be read via input/0 and inputs/0.
    #[clap(short, long)]
    null_input: bool,
    /// Read input values into an array
    #[clap(short, long)]
    slurp: bool,
    /// Treat each line of input will be supplied to the filter as a string.
    /// When used with --slurp, the whole input text will be supplied to the filter as a single
    /// string.
    #[clap(
        short = 'R',
        long,
        conflicts_with = "input",
        conflicts_with = "json-input",
        conflicts_with = "yaml-input"
    )]
    raw_input: bool,
}

impl InputFormatArg {
    fn get(self) -> SerializationFormat {
        if self.json_input {
            SerializationFormat::Json
        } else if self.yaml_input {
            SerializationFormat::Yaml
        } else {
            self.input_format
        }
    }
}

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Debug, Args)]
struct OutputFormatArg {
    /// Specify output format
    #[clap(group = "output", long, arg_enum, default_value_t = SerializationFormat::Json)]
    output_format: SerializationFormat,
    /// Write output as json values
    #[clap(group = "output", long)]
    json_output: bool,
    /// Write output as yaml values
    #[clap(group = "output", long)]
    yaml_output: bool,

    /// Compact output
    #[clap(
        short,
        long,
        conflicts_with = "output",
        conflicts_with = "json-output",
        conflicts_with = "yaml-output"
    )]
    compact_output: bool,
    /// Output raw string if the output value was a string
    #[clap(
        short,
        long,
        conflicts_with = "output",
        conflicts_with = "json-output",
        conflicts_with = "yaml-output"
    )]
    raw_output: bool,
}

impl OutputFormatArg {
    fn get(self) -> SerializationFormat {
        if self.json_output {
            SerializationFormat::Json
        } else if self.yaml_output {
            SerializationFormat::Yaml
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

fn run_with_input(args: MainArgs, input: impl Input) -> Result<()> {
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
    let module_loader = PreludeLoader();

    let (context, input) = input.into_iterators();
    let result_iterator = run_query(&query, context, input, &module_loader)
        .map_err(|e| anyhow!("{:?}", e))
        .with_context(|| "compile query")?;

    let output_format = args.output_format.get();
    match output_format {
        SerializationFormat::Json => {
            for value in result_iterator {
                match value {
                    Ok(Value::String(s)) if args.output_format.raw_output => {
                        stdout().write_all(s.as_bytes())?;
                        println!();
                    }
                    Ok(value) => {
                        if args.output_format.compact_output {
                            serde_json::ser::to_writer::<_, Value>(stdout().lock(), &value)?;
                            println!();
                        } else {
                            serde_json::ser::to_writer_pretty::<_, Value>(stdout().lock(), &value)?;
                            println!();
                        }
                    }
                    Err(e) => eprintln!("Error: {e:?}"),
                }
            }
        }
        SerializationFormat::Yaml => {
            for value in result_iterator {
                match value {
                    Ok(value) => serde_yaml::to_writer::<_, Value>(stdout().lock(), &value)
                        .with_context(|| "Write to output")?,
                    Err(e) => eprintln!("Error: {e:?}"),
                }
            }
        }
    }
    Ok(())
}

fn run_with_maybe_null_input(args: MainArgs, input: impl Input) -> Result<()> {
    if args.input_format.null_input {
        run_with_input(args, input.null_input())
    } else {
        run_with_input(args, input)
    }
}

fn run_with_maybe_slurp_null_input<I: Iterator<Item = Result<Value, InputError>>>(
    args: MainArgs,
    input: Tied<I>,
) -> Result<()> {
    if args.input_format.slurp {
        run_with_maybe_null_input(args, input.slurp())
    } else {
        run_with_maybe_null_input(args, input)
    }
}

fn main() -> Result<()> {
    let args: MainArgs = MainArgs::parse();
    init_log(&args.verbosity)?;
    log::debug!("Parsed argument: {:?}", args);

    let stdin = stdin();
    let mut locked = stdin.lock();

    if args.input_format.raw_input {
        if args.input_format.slurp {
            let mut input = String::new();
            locked.read_to_string(&mut input)?;
            run_with_maybe_null_input(args, Tied::new(std::iter::once(Ok(Value::from(input)))))
        } else {
            let input = locked
                .lines()
                .map(|l| l.map(Value::from).map_err(InputError::new));
            run_with_maybe_null_input(args, Tied::new(input))
        }
    } else {
        match args.input_format.get() {
            SerializationFormat::Json => {
                let input = serde_json::de::Deserializer::from_reader(locked)
                    .into_iter::<Value>()
                    .map(|r| r.map_err(InputError::new));
                run_with_maybe_slurp_null_input(args, Tied::new(input))
            }
            SerializationFormat::Yaml => {
                use serde::Deserialize;
                let input = serde_yaml::Deserializer::from_reader(locked)
                    .into_iter()
                    .map(Value::deserialize)
                    .map(|r| r.map_err(InputError::new));
                run_with_maybe_slurp_null_input(args, Tied::new(input))
            }
        }
    }
}
