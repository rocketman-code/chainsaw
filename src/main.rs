use std::path::{Path, PathBuf};
use std::time::Instant;

use clap::{CommandFactory, Parser, Subcommand};
use clap_complete::Shell;

use chainsaw::{error::Error, loader, query, report};

#[derive(Parser)]
#[command(
    name = "chainsaw",
    version,
    about = "TypeScript/JavaScript and Python dependency graph analyzer",
    after_help = "Repository: https://github.com/RocketMan234/chainsaw"
)]
struct Cli {
    /// Disable colored output
    #[arg(long, global = true)]
    no_color: bool,

    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand)]
enum Commands {
    /// Trace the transitive import weight from an entry point
    Trace {
        /// Entry point file to trace from
        entry: PathBuf,

        /// Compare against another entry point
        #[arg(long)]
        diff: Option<PathBuf>,

        /// Save a trace snapshot to a file for later comparison
        #[arg(long)]
        save: Option<PathBuf>,

        /// Compare current trace against a previously saved snapshot
        #[arg(long)]
        diff_from: Option<PathBuf>,

        /// Also traverse dynamic imports
        #[arg(long)]
        include_dynamic: bool,

        /// Show top N heaviest dependencies (0 to hide, -1 for all)
        #[arg(long, default_value_t = 10, allow_hyphen_values = true)]
        top: i32,

        /// Show top N modules by exclusive weight (0 to hide, -1 for all)
        #[arg(long, default_value_t = 20, allow_hyphen_values = true)]
        top_modules: i32,

        /// Show all shortest import chains to a package or file
        #[arg(long)]
        chain: Option<String>,

        /// Show where to cut to sever all import chains to a package or file
        #[arg(long)]
        cut: Option<String>,

        /// Output machine-readable JSON
        #[arg(long)]
        json: bool,

        /// Force full re-parse, ignoring cache
        #[arg(long)]
        no_cache: bool,

        /// Exclude packages from the heavy dependencies list
        #[arg(long, num_args = 1..)]
        ignore: Vec<String>,

        /// Suppress informational output (timing, warnings)
        #[arg(long, short)]
        quiet: bool,

        /// Max packages to show in diff output (-1 for all)
        #[arg(long, default_value_t = 10, allow_hyphen_values = true)]
        limit: i32,

        /// Exit with error if static weight exceeds this threshold (e.g. 5MB, 500KB)
        #[arg(long, value_parser = parse_size)]
        max_weight: Option<u64>,
    },

    /// Compare two saved trace snapshots
    Diff {
        /// First snapshot file (the "before" or "baseline")
        a: PathBuf,

        /// Second snapshot file (the "after" or "current")
        b: PathBuf,

        /// Max packages to show in diff output (-1 for all)
        #[arg(long, default_value_t = 10, allow_hyphen_values = true)]
        limit: i32,
    },

    /// List all third-party packages in the dependency graph
    Packages {
        /// Entry point file (used to detect project root)
        entry: PathBuf,

        /// Output machine-readable JSON
        #[arg(long)]
        json: bool,

        /// Force full re-parse, ignoring cache
        #[arg(long)]
        no_cache: bool,

        /// Show top N packages by size (0 to hide, -1 for all)
        #[arg(long, default_value_t = 10, allow_hyphen_values = true)]
        top: i32,

        /// Suppress informational output (timing, warnings)
        #[arg(long, short)]
        quiet: bool,
    },

    /// Generate shell completions
    Completions {
        /// Shell to generate completions for
        shell: Shell,
    },
}

#[allow(clippy::cast_possible_truncation, clippy::cast_sign_loss)]
fn parse_size(s: &str) -> Result<u64, String> {
    let s = s.trim();
    let (num_str, multiplier) = if let Some(n) = s.strip_suffix("MB") {
        (n.trim(), 1_000_000.0)
    } else if let Some(n) = s.strip_suffix("KB") {
        (n.trim(), 1_000.0)
    } else if let Some(n) = s.strip_suffix("B") {
        (n.trim(), 1.0)
    } else {
        // bare number = bytes
        (s, 1.0)
    };
    let value: f64 = num_str.parse().map_err(|_| format!("invalid size: {s}"))?;
    Ok((value * multiplier) as u64)
}

struct ResolvedTarget {
    target: query::ChainTarget,
    label: String,
    exists: bool,
}

fn main() {
    let cli = Cli::parse();
    let no_color = cli.no_color;
    let sc = report::StderrColor::new(no_color);

    if let Err(e) = run(cli.command, no_color, &sc) {
        eprintln!("{} {e}", sc.error("error:"));
        if let Some(hint) = e.hint() {
            eprintln!("hint: {hint}");
        }
        std::process::exit(1);
    }
}

#[allow(clippy::too_many_lines)]
fn run(command: Commands, no_color: bool, sc: &report::StderrColor) -> Result<(), Error> {
    match command {
        Commands::Trace {
            entry,
            diff,
            save,
            diff_from,
            include_dynamic,
            top,
            top_modules,
            chain,
            cut,
            json,
            no_cache,
            ignore,
            quiet,
            limit,
            max_weight,
        } => {
            let start = Instant::now();

            // Validate mutually exclusive flags before loading graph
            let query_flags: Vec<&str> = [
                chain.as_ref().map(|_| "--chain"),
                cut.as_ref().map(|_| "--cut"),
                diff.as_ref().map(|_| "--diff"),
                diff_from.as_ref().map(|_| "--diff-from"),
            ]
            .into_iter()
            .flatten()
            .collect();
            if query_flags.len() > 1 {
                eprintln!(
                    "{} {} cannot be used together",
                    sc.error("error:"),
                    query_flags.join(" and ")
                );
                std::process::exit(1);
            }

            // Load or build graph
            let (loaded, _cache_write) = loader::load_graph(&entry, no_cache)?;
            if !quiet {
                eprintln!(
                    "{} ({} modules) in {:.1}ms",
                    sc.status(if loaded.from_cache {
                        "Loaded cached graph"
                    } else {
                        "Built graph"
                    }),
                    loaded.graph.module_count(),
                    start.elapsed().as_secs_f64() * 1000.0
                );
                if loaded.unresolvable_dynamic_count > 0 && !loaded.from_cache {
                    let count = loaded.unresolvable_dynamic_count;
                    eprintln!(
                        "{} {count} dynamic import{} with non-literal argument{} could not be traced:",
                        sc.warning("warning:"),
                        if count == 1 { "" } else { "s" },
                        if count == 1 { "" } else { "s" },
                    );
                    for (path, file_count) in &loaded.unresolvable_dynamic_files {
                        let rel = report::relative_path(path, &loaded.root);
                        eprintln!("  {rel} ({file_count})");
                    }
                }
            }

            // Resolve entry module ID
            let Some(&entry_id) = loaded.graph.path_to_id.get(&loaded.entry) else {
                return Err(Error::EntryNotInGraph(loaded.entry));
            };

            let opts = query::TraceOptions {
                include_dynamic,
                top_n: top,
                ignore,
            };
            let result = query::trace(&loaded.graph, entry_id, &opts);
            let entry_rel = loaded
                .entry
                .strip_prefix(&loaded.root)
                .unwrap_or(&loaded.entry)
                .to_string_lossy()
                .into_owned();

            // Save snapshot if requested (works with any mode)
            if let Some(ref save_path) = save {
                let snapshot = result.to_snapshot(&entry_rel);
                let data = serde_json::to_string_pretty(&snapshot).unwrap();
                std::fs::write(save_path, data).unwrap_or_else(|e| {
                    eprintln!(
                        "{} cannot write snapshot '{}': {e}",
                        sc.error("error:"),
                        save_path.display()
                    );
                    std::process::exit(1);
                });
                if !quiet {
                    eprintln!(
                        "{} to {}",
                        sc.status("Snapshot saved"),
                        save_path.display()
                    );
                }
            }

            // Resolve --chain/--cut argument: file path or package name
            let resolve_target = |arg: &str| -> ResolvedTarget {
                let is_path = looks_like_path(arg, loaded.valid_extensions);
                if is_path {
                    let target_path =
                        loaded.root.join(arg).canonicalize().unwrap_or_else(|e| {
                            eprintln!("{} cannot find file '{arg}': {e}", sc.error("error:"));
                            std::process::exit(1);
                        });
                    let Some(&id) = loaded.graph.path_to_id.get(&target_path) else {
                        eprintln!(
                            "{} '{arg}' is not in the dependency graph",
                            sc.error("error:")
                        );
                        eprintln!("hint: is it reachable from the entry point?");
                        std::process::exit(1);
                    };
                    let p = &loaded.graph.module(id).path;
                    let label = p
                        .strip_prefix(&loaded.root)
                        .unwrap_or(p)
                        .to_string_lossy()
                        .into_owned();
                    ResolvedTarget {
                        target: query::ChainTarget::Module(id),
                        label,
                        exists: true,
                    }
                } else {
                    ResolvedTarget {
                        target: query::ChainTarget::Package(arg.to_string()),
                        label: arg.to_string(),
                        exists: loaded.graph.package_map.contains_key(arg),
                    }
                }
            };

            // Handle --chain mode
            if let Some(ref chain_arg) = chain {
                let resolved = resolve_target(chain_arg);
                if resolved.target == query::ChainTarget::Module(entry_id) {
                    eprintln!(
                        "{} --chain target is the entry point itself",
                        sc.error("error:")
                    );
                    eprintln!(
                        "hint: --chain finds import chains from the entry to a dependency"
                    );
                    std::process::exit(1);
                }
                let chains = query::find_all_chains(
                    &loaded.graph,
                    entry_id,
                    &resolved.target,
                    include_dynamic,
                );
                if json {
                    report::print_chains_json(
                        &loaded.graph,
                        &chains,
                        &resolved.label,
                        &loaded.root,
                        resolved.exists,
                    );
                } else {
                    report::print_chains(
                        &loaded.graph,
                        &chains,
                        &resolved.label,
                        &loaded.root,
                        resolved.exists,
                        no_color,
                    );
                }
                if chains.is_empty() {
                    std::process::exit(1);
                }
                return Ok(());
            }

            // Handle --cut mode
            if let Some(ref cut_arg) = cut {
                let resolved = resolve_target(cut_arg);
                if resolved.target == query::ChainTarget::Module(entry_id) {
                    eprintln!(
                        "{} --cut target is the entry point itself",
                        sc.error("error:")
                    );
                    eprintln!(
                        "hint: --cut finds where to sever import chains to a dependency"
                    );
                    std::process::exit(1);
                }
                let chains = query::find_all_chains(
                    &loaded.graph,
                    entry_id,
                    &resolved.target,
                    include_dynamic,
                );
                let cuts = query::find_cut_modules(
                    &loaded.graph,
                    &chains,
                    entry_id,
                    &resolved.target,
                    top,
                    include_dynamic,
                );
                if json {
                    report::print_cut_json(
                        &loaded.graph,
                        &cuts,
                        &chains,
                        &resolved.label,
                        &loaded.root,
                        resolved.exists,
                    );
                } else {
                    report::print_cut(
                        &loaded.graph,
                        &cuts,
                        &chains,
                        &resolved.label,
                        &loaded.root,
                        resolved.exists,
                        no_color,
                    );
                }
                if chains.is_empty() {
                    std::process::exit(1);
                }
                return Ok(());
            }

            // Handle --diff-from mode (compare against saved snapshot)
            if let Some(ref snapshot_path) = diff_from {
                let data = std::fs::read_to_string(snapshot_path).unwrap_or_else(|e| {
                    eprintln!(
                        "{} cannot read snapshot '{}': {e}",
                        sc.error("error:"),
                        snapshot_path.display()
                    );
                    std::process::exit(1);
                });
                let saved: query::TraceSnapshot =
                    serde_json::from_str(&data).unwrap_or_else(|e| {
                        eprintln!(
                            "{} invalid snapshot '{}': {e}",
                            sc.error("error:"),
                            snapshot_path.display()
                        );
                        std::process::exit(1);
                    });
                let diff_output =
                    query::diff_snapshots(&saved, &result.to_snapshot(&entry_rel));
                report::print_diff(
                    &diff_output,
                    &saved.entry,
                    &entry_rel,
                    limit,
                    no_color,
                );
                return Ok(());
            }

            // Handle --diff mode
            if let Some(diff_path) = diff {
                let diff_entry = diff_path
                    .canonicalize()
                    .map_err(|e| Error::EntryNotFound(diff_path.clone(), e))?;
                if diff_entry == loaded.entry {
                    eprintln!(
                        "{} both entry points are the same file, diff will be empty",
                        sc.warning("warning:")
                    );
                }

                let diff_snapshot =
                    if let Some(&diff_id) = loaded.graph.path_to_id.get(&diff_entry) {
                        // Same graph — trace directly
                        let diff_rel = diff_entry
                            .strip_prefix(&loaded.root)
                            .unwrap_or(&diff_entry)
                            .to_string_lossy()
                            .into_owned();
                        query::trace(&loaded.graph, diff_id, &opts).to_snapshot(&diff_rel)
                    } else {
                        // Different project — build second graph
                        let (diff_loaded, _diff_cache_write) =
                            loader::load_graph(&diff_entry, no_cache)?;
                        let Some(&diff_id) =
                            diff_loaded.graph.path_to_id.get(&diff_loaded.entry)
                        else {
                            return Err(Error::EntryNotInGraph(diff_loaded.entry));
                        };
                        let diff_rel = diff_loaded
                            .entry
                            .strip_prefix(&diff_loaded.root)
                            .unwrap_or(&diff_loaded.entry)
                            .to_string_lossy()
                            .into_owned();
                        query::trace(&diff_loaded.graph, diff_id, &opts)
                            .to_snapshot(&diff_rel)
                    };

                let diff_output =
                    query::diff_snapshots(&result.to_snapshot(&entry_rel), &diff_snapshot);
                report::print_diff(
                    &diff_output,
                    &entry_rel,
                    &diff_snapshot.entry,
                    limit,
                    no_color,
                );
                return Ok(());
            }

            // Normal trace output
            if json {
                report::print_trace_json(
                    &loaded.graph,
                    &result,
                    &loaded.entry,
                    &loaded.root,
                    top_modules,
                );
            } else {
                report::print_trace(
                    &loaded.graph,
                    &result,
                    &loaded.entry,
                    &loaded.root,
                    top,
                    top_modules,
                    include_dynamic,
                    no_color,
                );
            }

            if let Some(threshold) = max_weight
                && result.static_weight > threshold
            {
                eprintln!(
                    "{} static weight {} exceeds threshold {}",
                    sc.error("error:"),
                    report::format_size(result.static_weight),
                    report::format_size(threshold),
                );
                std::process::exit(1);
            }

            if !quiet {
                eprintln!(
                    "\n{} in {:.1}ms",
                    sc.status("Completed"),
                    start.elapsed().as_secs_f64() * 1000.0
                );
            }
        }

        Commands::Diff { a, b, limit } => {
            let load_snapshot = |path: &Path| -> query::TraceSnapshot {
                let data = std::fs::read_to_string(path).unwrap_or_else(|e| {
                    eprintln!(
                        "{} cannot read snapshot '{}': {e}",
                        sc.error("error:"),
                        path.display()
                    );
                    std::process::exit(1);
                });
                serde_json::from_str(&data).unwrap_or_else(|e| {
                    eprintln!(
                        "{} invalid snapshot '{}': {e}",
                        sc.error("error:"),
                        path.display()
                    );
                    std::process::exit(1);
                })
            };

            let snap_a = load_snapshot(&a);
            let snap_b = load_snapshot(&b);
            let diff_output = query::diff_snapshots(&snap_a, &snap_b);
            report::print_diff(
                &diff_output,
                &snap_a.entry,
                &snap_b.entry,
                limit,
                no_color,
            );
        }

        Commands::Packages {
            entry,
            json,
            no_cache,
            top,
            quiet,
        } => {
            let start = Instant::now();
            let (loaded, _cache_write) = loader::load_graph(&entry, no_cache)?;
            if !quiet {
                eprintln!(
                    "{} ({} modules) in {:.1}ms",
                    sc.status(if loaded.from_cache {
                        "Loaded cached graph"
                    } else {
                        "Built graph"
                    }),
                    loaded.graph.module_count(),
                    start.elapsed().as_secs_f64() * 1000.0
                );
            }

            if json {
                report::print_packages_json(&loaded.graph, top);
            } else {
                report::print_packages(&loaded.graph, top, no_color);
            }
        }

        Commands::Completions { shell } => {
            clap_complete::generate(
                shell,
                &mut Cli::command(),
                "chainsaw",
                &mut std::io::stdout(),
            );
        }
    }
    Ok(())
}

/// Determine whether a --chain/--cut argument looks like a file path
/// (as opposed to a package name).
fn looks_like_path(arg: &str, extensions: &[&str]) -> bool {
    !arg.starts_with('@')
        && (arg.contains('/')
            || arg.contains(std::path::MAIN_SEPARATOR)
            || arg.rsplit_once('.').is_some_and(|(_, suffix)| extensions.contains(&suffix)))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn scoped_npm_package_is_not_path() {
        let exts = &["ts", "tsx", "js", "jsx"];
        assert!(!looks_like_path("@slack/web-api", exts));
        assert!(!looks_like_path("@aws-sdk/client-s3", exts));
        assert!(!looks_like_path("@anthropic-ai/sdk", exts));
    }

    #[test]
    fn relative_file_path_is_path() {
        let exts = &["ts", "tsx", "js", "jsx"];
        assert!(looks_like_path("src/index.ts", exts));
        assert!(looks_like_path("lib/utils.js", exts));
    }

    #[test]
    fn bare_package_name_is_not_path() {
        let exts = &["ts", "tsx", "js", "jsx"];
        assert!(!looks_like_path("zod", exts));
        assert!(!looks_like_path("express", exts));
        // highlight.js is ambiguous — .js extension triggers path heuristic.
        // Acceptable: canonicalize will error clearly if no such file exists.
        assert!(looks_like_path("highlight.js", exts));
    }

    #[test]
    fn file_with_extension_is_path() {
        let exts = &["ts", "tsx", "js", "jsx", "py"];
        assert!(looks_like_path("utils.ts", exts));
        assert!(looks_like_path("main.py", exts));
        assert!(!looks_like_path("utils.txt", exts));
    }

    #[test]
    fn parse_size_units() {
        assert_eq!(parse_size("5MB").unwrap(), 5_000_000);
        assert_eq!(parse_size("500KB").unwrap(), 500_000);
        assert_eq!(parse_size("100B").unwrap(), 100);
        assert_eq!(parse_size("1234").unwrap(), 1234);
        assert_eq!(parse_size("1.5MB").unwrap(), 1_500_000);
        assert!(parse_size("abc").is_err());
    }
}
