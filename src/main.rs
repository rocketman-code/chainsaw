mod cache;
mod graph;
mod lang;
mod query;
mod report;
mod walker;

use std::path::{Path, PathBuf};
use std::time::Instant;

use clap::{Parser, Subcommand};

use lang::LanguageSupport;

#[derive(Parser)]
#[command(name = "chainsaw", about = "TypeScript/JavaScript dependency graph analyzer")]
struct Cli {
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

        /// Only follow static edges (default behavior)
        #[arg(long, default_value_t = true)]
        static_only: bool,

        /// Also traverse dynamic imports
        #[arg(long)]
        include_dynamic: bool,

        /// Show top N heaviest dependencies
        #[arg(long, default_value_t = 10)]
        top: usize,

        /// Show top N modules by transitive cost (0 to hide, -1 for all)
        #[arg(long, default_value_t = 20, allow_hyphen_values = true)]
        top_modules: i32,

        /// Show all shortest import chains to a specific package
        #[arg(long)]
        chain: Option<String>,

        /// Show where to cut to sever all import chains to a package
        #[arg(long)]
        cut: Option<String>,

        /// Output machine-readable JSON
        #[arg(long)]
        json: bool,

        /// Force full re-parse, ignoring cache
        #[arg(long)]
        no_cache: bool,
    },
}

fn main() {
    let cli = Cli::parse();

    match cli.command {
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
            ..
        } => {
            let start = Instant::now();

            // Determine project root from entry file
            let entry = entry.canonicalize().unwrap_or_else(|e| {
                eprintln!("error: cannot find entry file '{}': {e}", entry.display());
                std::process::exit(1);
            });

            let (root, kind) = lang::detect_project(&entry);
            let lang_support: Box<dyn LanguageSupport> = match kind {
                lang::ProjectKind::TypeScript => {
                    Box::new(lang::typescript::TypeScriptSupport::new(&root))
                }
                lang::ProjectKind::Python => {
                    Box::new(lang::python::PythonSupport::new(&root))
                }
            };

            // Load or build graph
            let (graph, from_cache) = load_or_build_graph(&root, no_cache, lang_support.as_ref());
            eprintln!(
                "{} ({} modules) in {:.1}ms",
                if from_cache { "Loaded cached graph" } else { "Built graph" },
                graph.module_count(),
                start.elapsed().as_secs_f64() * 1000.0
            );

            // Resolve entry module ID
            let entry_id = match graph.path_to_id.get(&entry) {
                Some(&id) => id,
                None => {
                    eprintln!(
                        "error: entry file '{}' not found in graph",
                        entry.display()
                    );
                    let exts = match kind {
                        lang::ProjectKind::TypeScript => ".ts/.js/.tsx/.jsx/.mjs/.cjs/.mts/.cts",
                        lang::ProjectKind::Python => ".py",
                    };
                    eprintln!("hint: is it a {exts} file within the project?");
                    std::process::exit(1);
                }
            };

            // Validate mutually exclusive flags
            if chain.is_some() && cut.is_some() {
                eprintln!("error: --chain and --cut cannot be used together");
                std::process::exit(1);
            }
            if diff.is_some() && diff_from.is_some() {
                eprintln!("error: --diff and --diff-from cannot be used together");
                std::process::exit(1);
            }

            let opts = query::TraceOptions {
                include_dynamic,
                top_n: top,
            };
            let result = query::trace(&graph, entry_id, &opts);

            // Save snapshot if requested (works with any mode)
            if let Some(ref save_path) = save {
                let snapshot = result.to_snapshot();
                let data = serde_json::to_string_pretty(&snapshot).unwrap();
                std::fs::write(save_path, data).unwrap_or_else(|e| {
                    eprintln!("error: cannot write snapshot '{}': {e}", save_path.display());
                    std::process::exit(1);
                });
                eprintln!("Snapshot saved to {}", save_path.display());
            }

            // Handle --chain mode
            if let Some(ref package_name) = chain {
                let package_exists = graph.package_map.contains_key(package_name.as_str());
                let chains = query::find_all_chains(&graph, entry_id, package_name, include_dynamic);
                if json {
                    report::print_chains_json(&graph, &chains, package_name, &root, package_exists);
                } else {
                    report::print_chains(&graph, &chains, package_name, &root, package_exists);
                }
                return;
            }

            // Handle --cut mode
            if let Some(ref package_name) = cut {
                let package_exists = graph.package_map.contains_key(package_name.as_str());
                let chains = query::find_all_chains(&graph, entry_id, package_name, include_dynamic);
                let cuts = query::find_cut_modules(&graph, &chains, entry_id, package_name, top);
                if json {
                    report::print_cut_json(&graph, &cuts, &chains, package_name, &root, package_exists);
                } else {
                    report::print_cut(&graph, &cuts, &chains, package_name, &root, package_exists);
                }
                return;
            }

            // Handle --diff-from mode (compare against saved snapshot)
            if let Some(ref snapshot_path) = diff_from {
                let data = std::fs::read_to_string(snapshot_path).unwrap_or_else(|e| {
                    eprintln!(
                        "error: cannot read snapshot '{}': {e}",
                        snapshot_path.display()
                    );
                    std::process::exit(1);
                });
                let saved: query::TraceSnapshot = serde_json::from_str(&data).unwrap_or_else(|e| {
                    eprintln!(
                        "error: invalid snapshot '{}': {e}",
                        snapshot_path.display()
                    );
                    std::process::exit(1);
                });
                let diff_output = query::diff_snapshots(&saved, &result.to_snapshot());

                let entry_rel = entry
                    .strip_prefix(&root)
                    .unwrap_or(&entry)
                    .to_string_lossy();
                let snapshot_label = snapshot_path
                    .file_name()
                    .unwrap_or(snapshot_path.as_os_str())
                    .to_string_lossy();
                report::print_diff(&diff_output, &snapshot_label, &entry_rel);
                return;
            }

            // Handle --diff mode
            if let Some(diff_path) = diff {
                let diff_entry = diff_path.canonicalize().unwrap_or_else(|e| {
                    eprintln!(
                        "error: cannot find diff entry file '{}': {e}",
                        diff_path.display()
                    );
                    std::process::exit(1);
                });

                let diff_snapshot = if let Some(&diff_id) = graph.path_to_id.get(&diff_entry) {
                    // Same graph — trace directly
                    query::trace(&graph, diff_id, &opts).to_snapshot()
                } else {
                    // Different package — build a second graph from its root
                    let (diff_root, diff_kind) = lang::detect_project(&diff_entry);
                    let diff_lang: Box<dyn LanguageSupport> = match diff_kind {
                        lang::ProjectKind::TypeScript => {
                            Box::new(lang::typescript::TypeScriptSupport::new(&diff_root))
                        }
                        lang::ProjectKind::Python => {
                            Box::new(lang::python::PythonSupport::new(&diff_root))
                        }
                    };
                    let (diff_graph, _) = load_or_build_graph(&diff_root, no_cache, diff_lang.as_ref());
                    let diff_id = match diff_graph.path_to_id.get(&diff_entry) {
                        Some(&id) => id,
                        None => {
                            eprintln!(
                                "error: diff entry file '{}' not found in graph",
                                diff_entry.display()
                            );
                            std::process::exit(1);
                        }
                    };
                    query::trace(&diff_graph, diff_id, &opts).to_snapshot()
                };

                let diff_output = query::diff_snapshots(&result.to_snapshot(), &diff_snapshot);

                let entry_rel = entry
                    .strip_prefix(&root)
                    .unwrap_or(&entry)
                    .to_string_lossy();
                let diff_root = lang::detect_project(&diff_entry).0;
                let diff_rel = diff_entry
                    .strip_prefix(&diff_root)
                    .unwrap_or(&diff_entry)
                    .to_string_lossy();
                report::print_diff(&diff_output, &entry_rel, &diff_rel);
                return;
            }

            // Normal trace output
            if json {
                report::print_trace_json(&graph, &result, &entry, &root, top_modules);
            } else {
                report::print_trace(&graph, &result, &entry, &root, top_modules);
            }

            let elapsed = start.elapsed();
            eprintln!("\nCompleted in {:.1}ms", elapsed.as_secs_f64() * 1000.0);
        }
    }
}

/// Returns (graph, from_cache).
fn load_or_build_graph(
    root: &Path,
    no_cache: bool,
    lang: &dyn LanguageSupport,
) -> (graph::ModuleGraph, bool) {
    if !no_cache {
        if let Some(g) = cache::load_cache(root) {
            return (g, true);
        }
    }
    let g = walker::build_graph(root, lang);
    cache::save_cache(root, &g);
    (g, false)
}
