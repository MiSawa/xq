use std::{
    io::{stdin, stdout, BufRead, Read, Write},
    path::PathBuf,
};

use anyhow::{anyhow, Context, Result};
use clap::Parser;
use tracing::*;
use xq::{module_loader::PreludeLoader, run_query, InputError, Value};

use crate::cli::input::{Input, Tied};

mod cli;

#[derive(Parser, Debug)]
#[clap(author, about, version)]
#[clap(long_version(option_env!("LONG_VERSION").unwrap_or(env!("CARGO_PKG_VERSION"))))]
struct Cli {
    /// The query to run
    #[clap(default_value = ".")]
    query: String,

    /// Read query from a file instead of arg
    #[clap(
        name = "file",
        short = 'f',
        long = "from-file",
        conflicts_with = "query",
        value_hint = clap::ValueHint::FilePath
    )]
    query_file: Option<PathBuf>,

    #[clap(flatten)]
    input_format: InputFormatArg,

    #[clap(flatten)]
    output_format: OutputFormatArg,

    #[clap(flatten)]
    verbosity: cli::Verbosity,
}

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Debug, clap::ValueEnum)]
enum SerializationFormat {
    Json,
    Yaml,
}

impl Default for SerializationFormat {
    fn default() -> Self {
        SerializationFormat::Json
    }
}

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Debug, clap::Args)]
struct InputFormatArg {
    /// Specify input format
    #[arg(long, value_enum, default_value_t, group = "input-format")]
    input_format: SerializationFormat,

    /// Read input as json values
    #[arg(long, group = "input-format")]
    json_input: bool,

    /// Read input as yaml values
    #[arg(long, group = "input-format")]
    yaml_input: bool,

    /// Treat each line of input will be supplied to the filter as a string.
    /// When used with --slurp, the whole input text will be supplied to the filter as a single
    /// string.
    #[arg(short = 'R', long, group = "input-format")]
    raw_input: bool,

    /// Single null is supplied to the program.
    /// The original input can still be read via input/0 and inputs/0.
    #[arg(short, long)]
    null_input: bool,

    /// Read input values into an array
    #[arg(short, long)]
    slurp: bool,
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

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Debug, clap::Args)]
struct OutputFormatArg {
    /// Specify output format
    #[arg(long, value_enum, default_value_t, group = "output-format")]
    output_format: SerializationFormat,

    /// Write output as json values
    #[arg(long, group = "output-format")]
    json_output: bool,

    /// Write output as yaml values
    #[arg(long, group = "output-format")]
    yaml_output: bool,

    /// Output raw string if the output value was a string
    #[clap(short, long, conflicts_with = "output-format")]
    raw_output: bool,

    /// Compact output
    #[clap(short, long, conflicts_with = "output-format")]
    compact_output: bool,
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

fn run_with_input(cli: Cli, input: impl Input) -> Result<()> {
    let query = if let Some(path) = cli.query_file {
        trace!("Read query from file {:?}", path);
        std::fs::read_to_string(path)?
    } else {
        trace!(
            "Read from query in arg (if it wasn't the default value): `{}`",
            cli.query
        );
        cli.query
    };
    let module_loader = PreludeLoader();

    let (context, input) = input.into_iterators();
    let result_iterator = run_query(&query, context, input, &module_loader)
        .map_err(|e| anyhow!("{:?}", e))
        .with_context(|| "compile query")?;

    let output_format = cli.output_format.get();
    match output_format {
        SerializationFormat::Json => {
            for value in result_iterator {
                match value {
                    Ok(Value::String(s)) if cli.output_format.raw_output => {
                        stdout().write_all(s.as_bytes())?;
                        println!();
                    }
                    Ok(value) => {
                        if cli.output_format.compact_output {
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

fn run_with_maybe_null_input(cli: Cli, input: impl Input) -> Result<()> {
    if cli.input_format.null_input {
        run_with_input(cli, input.null_input())
    } else {
        run_with_input(cli, input)
    }
}

fn run_with_maybe_slurp_null_input<I: Iterator<Item = Result<Value, InputError>>>(
    args: Cli,
    input: Tied<I>,
) -> Result<()> {
    if args.input_format.slurp {
        run_with_maybe_null_input(args, input.slurp())
    } else {
        run_with_maybe_null_input(args, input)
    }
}

fn main() -> Result<()> {
    let cli: Cli = Cli::parse();
    cli.verbosity.configure();
    debug!(
        query = cli.query,
        input_format = ?cli.input_format,
        output_format = ?cli.output_format,
        "Parsed CLI arguments"
    );

    let stdin = stdin();
    let mut locked = stdin.lock();

    if cli.input_format.raw_input {
        if cli.input_format.slurp {
            let mut input = String::new();
            locked.read_to_string(&mut input)?;
            run_with_maybe_null_input(cli, Tied::new(std::iter::once(Ok(Value::from(input)))))
        } else {
            let input = locked
                .lines()
                .map(|l| l.map(Value::from).map_err(InputError::new));
            run_with_maybe_null_input(cli, Tied::new(input))
        }
    } else {
        match cli.input_format.get() {
            SerializationFormat::Json => {
                let input = serde_json::de::Deserializer::from_reader(locked)
                    .into_iter::<Value>()
                    .map(|r| r.map_err(InputError::new));
                run_with_maybe_slurp_null_input(cli, Tied::new(input))
            }
            SerializationFormat::Yaml => {
                use serde::Deserialize;
                let input = serde_yaml::Deserializer::from_reader(locked)
                    .into_iter()
                    .map(Value::deserialize)
                    .map(|r| r.map_err(InputError::new));
                run_with_maybe_slurp_null_input(cli, Tied::new(input))
            }
        }
    }
}
