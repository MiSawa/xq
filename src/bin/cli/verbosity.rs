use tracing::Level;
use tracing_subscriber::{filter::Targets, prelude::*};

#[derive(clap::Args, Debug)]
pub struct Verbosity {
    /// Increase verbosity or the output
    ///
    /// The verbosity only apply to the logging of the crate,
    /// logging from other crates are filtered to INFO.
    #[clap(short, long, help_heading = "Verbosity", action = clap::ArgAction::Count)]
    verbose: u8,

    /// Reduce verbosity of the output
    #[clap(short, long, help_heading = "Verbosity", action = clap::ArgAction::Count, conflicts_with = "verbose")]
    quiet: u8,
}

impl Verbosity {
    pub(crate) fn configure(&self) {
        #[cfg(debug_assertions)]
        let show_details = true;
        #[cfg(not(debug_assertions))]
        let show_details = false;

        tracing_subscriber::registry()
            .with(
                tracing_subscriber::fmt::layer()
                    .pretty()
                    .with_file(show_details)
                    .with_target(show_details),
            )
            .with::<Targets>(self.into())
            .init();
    }
}

impl From<&Verbosity> for tracing_subscriber::filter::Targets {
    fn from(val: &Verbosity) -> Self {
        tracing_subscriber::filter::Targets::new()
            // Enable the `WARN` level for anything not `xq`
            .with_default(Level::WARN)
            // Use CLI verbosity to determine the level of logging asked
            .with_target(
                "xq",
                match val.verbose as i16 - val.quiet as i16 {
                    ..=-1 => Level::ERROR,
                    0 => Level::WARN,
                    1 => Level::INFO,
                    2 => Level::DEBUG,
                    3.. => Level::TRACE,
                },
            )
    }
}
