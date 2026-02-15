mod perf_judge;
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
}

fn main() {
    let cli = Cli::parse();
    match cli.command {
        Command::PerfJudge { dirs } => {
            std::process::exit(perf_judge::run(&dirs));
        }
    }
}
