mod check;
mod hooks;
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
    /// Validate benchmark performance (gate for pre-push, or ad-hoc comparison)
    PerfValidate {
        /// Compare against a named criterion baseline instead of checking changed files.
        /// Runs all benchmarks (or those specified) with confirmation runs.
        #[arg(long)]
        baseline: Option<String>,

        /// Specific benchmark names to check (only with --baseline)
        benchmarks: Vec<String>,
    },
    /// Pre-commit hook: run checks on feature branches, perf attestation gate on main
    PreCommit,
    /// Pre-push hook: block pushes without perf attestation
    PrePush {
        /// Remote name (passed by git, ignored)
        #[arg(hide = true)]
        remote: Option<String>,
        /// Remote URL (passed by git, ignored)
        #[arg(hide = true)]
        url: Option<String>,
    },
    /// Install git hooks
    InstallHooks,
    /// Run all local CI checks (fmt, clippy, test)
    Check,
}

fn main() {
    let cli = Cli::parse();
    match cli.command {
        Command::PerfValidate {
            baseline,
            benchmarks,
        } => {
            std::process::exit(perf_validate::run(baseline.as_deref(), &benchmarks));
        }
        Command::PreCommit => {
            std::process::exit(hooks::pre_commit());
        }
        Command::PrePush { .. } => {
            std::process::exit(hooks::pre_push());
        }
        Command::InstallHooks => {
            std::process::exit(hooks::install_hooks());
        }
        Command::Check => {
            std::process::exit(check::run());
        }
    }
}
