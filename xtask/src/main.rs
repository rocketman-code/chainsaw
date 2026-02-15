mod perf_judge;
mod perf_validate;
mod registry;

use clap::{Parser, Subcommand};

#[derive(Parser)]
#[command(name = "xtask", about = "Development tasks for chainsaw")]
struct Cli {
    #[command(subcommand)]
    command: Command,
}

#[derive(Subcommand)]
enum Command {
    /// Validate criterion benchmark results with Welch's t-test
    PerfJudge {
        /// Criterion benchmark directories to validate
        #[arg(required = true)]
        dirs: Vec<String>,
    },
    /// Check perf-sensitive changes have passing benchmarks
    PerfValidate,
}

fn main() {
    let cli = Cli::parse();
    match cli.command {
        Command::PerfJudge { dirs } => {
            std::process::exit(perf_judge::run(&dirs));
        }
        Command::PerfValidate => {
            std::process::exit(perf_validate::run());
        }
    }
}
