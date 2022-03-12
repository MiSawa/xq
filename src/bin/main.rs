use anyhow::{anyhow, Context, Result};
use clap::{ArgEnum, Args, Parser, ValueHint};
use clap_verbosity_flag::Verbosity;
use std::{
    io::{stdin, stdout, Write},
    path::PathBuf,
};
use xq::{module_loader::PreludeLoader, run_query, Array, InputError, Value};

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

    /// Read input values into an array
    #[clap(short, long)]
    slurp: bool,
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
    /// Output raw string if the output value was a string
    #[clap(short, long, conflicts_with = "yaml-output")]
    raw_output: bool,
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

fn run_with_input(
    args: MainArgs,
    input: impl Iterator<Item = Result<Value, InputError>>,
) -> Result<()> {
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

    let result_iterator = run_query(&query, input, &module_loader)
        .map_err(|e| anyhow!("{:?}", e))
        .with_context(|| "compile query")?;

    let output_format = args.output_format.get();
    match output_format {
        OutputFormat::Json => {
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
        OutputFormat::Yaml => {
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

fn main() -> Result<()> {
    let args: MainArgs = MainArgs::parse();
    init_log(&args.verbosity)?;
    log::debug!("Parsed argument: {:?}", args);

    match args.input_format.get() {
        InputFormat::Null => {
            run_with_input(args, [Ok(Value::Null)].into_iter())?;
        }
        InputFormat::Json => {
            let stdin = stdin();
            let locked = stdin.lock();
            let input = serde_json::de::Deserializer::from_reader(locked)
                .into_iter::<Value>()
                .map(|r| r.map_err(InputError::new));
            if args.input_format.slurp {
                let v: Array = input.collect::<Result<_, InputError>>()?;
                run_with_input(args, [Ok(v.into())].into_iter())?;
            } else {
                run_with_input(args, input)?;
            }
        }
        InputFormat::Yaml => {
            use serde::Deserialize;
            let stdin = stdin();
            let locked = stdin.lock();
            let input = serde_yaml::Deserializer::from_reader(locked)
                .into_iter()
                .map(Value::deserialize)
                .map(|r| r.map_err(InputError::new));
            if args.input_format.slurp {
                let v: Array = input.collect::<Result<_, InputError>>()?;
                run_with_input(args, [Ok(v.into())].into_iter())?;
            } else {
                run_with_input(args, input)?;
            }
        }
    }
    Ok(())
}
