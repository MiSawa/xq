use std::{
    io::{stdin, stdout, BufRead, Read, Write},
    path::PathBuf,
};

use anyhow::{anyhow, Context, Result};
use clap::Parser;
use clap_verbosity_flag::Verbosity;
use cli::input::Input;
use is_terminal::IsTerminal;
use xq::{module_loader::PreludeLoader, run_query, InputError, Value};

use crate::cli::input::Tied;

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
    verbosity: Verbosity,
}

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Debug, Default, clap::ValueEnum)]
enum SerializationFormat {
    #[default]
    Json,
    Yaml,
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

    /// Colorize output where possible (currently only JSON is supported)
    #[clap(short = 'C', long, group = "output-color")]
    color_output: bool,

    /// Do not colorize output
    #[clap(short = 'M', long, group = "output-color")]
    monochrome_output: bool,
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

fn get_json_style() -> colored_json::Styler {
    fn set_env_colors(styler: &mut colored_json::Styler) -> Result<()> {
        if let Ok(env_colors) = std::env::var("XQ_COLORS") {
            let env_colors = env_colors.split(':');
            for (i, env_color) in env_colors.enumerate().take(7) {
                let styles = match i {
                    0 => vec![&mut styler.nil_value],
                    1 => vec![&mut styler.bool_value],
                    2 => continue, // cannot easily separate true/false values, so apply false style to both
                    3 => vec![&mut styler.integer_value, &mut styler.float_value],
                    4 => vec![&mut styler.string_value],
                    5 => vec![&mut styler.array_brackets],
                    6 => vec![&mut styler.object_brackets],
                    _ => continue,
                };

                let (modifier, color) = env_color.split_once(';').unwrap_or(("0", env_color));
                let modifier: i32 = modifier.parse()?;
                let color: i32 = color.parse()?;

                let color = match color {
                    30 => colored_json::Color::Black,
                    31 => colored_json::Color::Red,
                    32 => colored_json::Color::Green,
                    33 => colored_json::Color::Yellow,
                    34 => colored_json::Color::Blue,
                    35 => colored_json::Color::Magenta,
                    36 => colored_json::Color::Cyan,
                    37 => colored_json::Color::White,
                    _ => colored_json::Color::Default,
                };

                for style in styles {
                    *style = colored_json::Style::new(color);

                    match modifier {
                        1 => {}
                        2 => *style = style.dimmed(),
                        4 => *style = style.underline(),
                        5 => *style = style.blink(),
                        7 => *style = style.invert(),
                        8 => *style = style.hidden(),
                        _ => {}
                    }
                }
            }
        }

        Ok(())
    }

    let mut styler = colored_json::Styler {
        nil_value: colored_json::Style::new(colored_json::Color::Default).dimmed(),
        ..Default::default()
    };
    set_env_colors(&mut styler)
        .unwrap_or_else(|_| eprintln!("Failed to set colors from $XQ_COLORS"));
    styler
}

fn run_with_input(cli: Cli, input: impl Input) -> Result<()> {
    let query = if let Some(path) = cli.query_file {
        log::trace!("Read query from file {path:?}");
        std::fs::read_to_string(path)?
    } else {
        log::trace!(
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
            let is_stdout_terminal = stdout().is_terminal();
            let no_color_from_env = || std::env::var("NO_COLOR").is_ok_and(|s| !s.is_empty());
            let should_colorize_output = cli.output_format.color_output
                || (is_stdout_terminal
                    && !cli.output_format.monochrome_output
                    && !no_color_from_env());
            let color_styler = should_colorize_output.then(get_json_style);

            for value in result_iterator {
                match value {
                    Ok(Value::String(s)) if cli.output_format.raw_output => {
                        stdout().write_all(s.as_bytes())?;
                        println!();
                    }
                    Ok(value) => {
                        if cli.output_format.compact_output {
                            if let Some(styler) = color_styler {
                                let formatter = colored_json::ColoredFormatter::with_styler(
                                    colored_json::CompactFormatter,
                                    styler,
                                );
                                formatter.write_colored_json(
                                    &serde_json::to_value(&value)?,
                                    &mut stdout().lock(),
                                    colored_json::ColorMode::On,
                                )?;
                                println!();
                            } else {
                                serde_json::ser::to_writer::<_, Value>(stdout().lock(), &value)?;
                                println!();
                            }
                        } else if let Some(styler) = color_styler {
                            let formatter = colored_json::ColoredFormatter::with_styler(
                                colored_json::PrettyFormatter::default(),
                                styler,
                            );
                            formatter.write_colored_json(
                                &serde_json::to_value(&value)?,
                                &mut stdout().lock(),
                                colored_json::ColorMode::On,
                            )?;
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
    init_log(&cli.verbosity)?;
    log::debug!("Parsed argument: {cli:?}");

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
                    .map(Value::deserialize)
                    .map(|r| r.map_err(InputError::new));
                run_with_maybe_slurp_null_input(cli, Tied::new(input))
            }
        }
    }
}
