use clap::builder::styling::{AnsiColor, Effects, Style, Styles};

#[derive(clap::Parser, Debug, Clone, Default)]
#[clap(styles = CARGO_STYLING)]
pub struct Cli {
    pub file: Option<String>,

    /// Do not print log messages
    #[arg(short, long, default_value_t = false)]
    pub quiet: bool,

    /// Read input from stdin
    #[arg(long, default_value_t = false)]
    pub stdin: bool,

    /// Maximum amount of errors to report
    #[arg(long, value_name = "max-errors", default_value_t = 4)]
    pub max_errors: usize,
}

const HEADER: Style = AnsiColor::Green.on_default().effects(Effects::BOLD);
const USAGE: Style = AnsiColor::Green.on_default().effects(Effects::BOLD);
const LITERAL: Style = AnsiColor::Cyan.on_default().effects(Effects::BOLD);
const PLACEHOLDER: Style = AnsiColor::Cyan.on_default();
const ERROR: Style = AnsiColor::Red.on_default().effects(Effects::BOLD);
const VALID: Style = AnsiColor::Cyan.on_default().effects(Effects::BOLD);
const INVALID: Style = AnsiColor::Yellow.on_default().effects(Effects::BOLD);

/// Cargo's color style
/// [source](https://github.com/crate-ci/clap-cargo/blob/master/src/style.rs)
const CARGO_STYLING: Styles = Styles::styled()
    .header(HEADER)
    .usage(USAGE)
    .literal(LITERAL)
    .placeholder(PLACEHOLDER)
    .error(ERROR)
    .valid(VALID)
    .invalid(INVALID);
