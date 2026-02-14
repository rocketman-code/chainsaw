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
#[command(name = "chainsaw", version, about = "TypeScript/JavaScript and Python dependency graph analyzer")]
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
    },

    /// Compare two saved trace snapshots
    Diff {
        /// First snapshot file (the "before" or "baseline")
        a: PathBuf,

        /// Second snapshot file (the "after" or "current")
        b: PathBuf,
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
            ignore,
            ..
        } => {
            let start = Instant::now();

            // Determine project root from entry file
            let entry = entry.canonicalize().unwrap_or_else(|e| {
                eprintln!("error: cannot find entry file '{}': {e}", entry.display());
                std::process::exit(1);
            });

            if entry.is_dir() {
                eprintln!("error: '{}' is a directory, not a source file", entry.display());
                std::process::exit(1);
            }

            let (root, kind) = lang::detect_project(&entry).unwrap_or_else(|| {
                let ext = entry.extension().and_then(|e| e.to_str()).unwrap_or("(none)");
                eprintln!("error: unsupported file type '.{ext}'");
                eprintln!("hint: chainsaw supports TypeScript/JavaScript and Python files");
                std::process::exit(1);
            });
            let lang_support: Box<dyn LanguageSupport> = match kind {
                lang::ProjectKind::TypeScript => {
                    Box::new(lang::typescript::TypeScriptSupport::new(&root))
                }
                lang::ProjectKind::Python => {
                    Box::new(lang::python::PythonSupport::new(&root))
                }
            };
            let valid_extensions = lang_support.extensions();

            // Load or build graph
            let load_result = load_or_build_graph(&entry, &root, no_cache, lang_support.as_ref());
            let graph = load_result.graph;
            let unresolvable_dynamic = load_result.unresolvable_dynamic;
            eprintln!(
                "{} ({} modules) in {:.1}ms",
                if load_result.from_cache { "Loaded cached graph" } else { "Built graph" },
                graph.module_count(),
                start.elapsed().as_secs_f64() * 1000.0
            );
            if unresolvable_dynamic > 0 && !load_result.from_cache {
                eprintln!(
                    "warning: {} dynamic import{} with non-literal argument{} could not be traced",
                    unresolvable_dynamic,
                    if unresolvable_dynamic == 1 { "" } else { "s" },
                    if unresolvable_dynamic == 1 { "" } else { "s" },
                );
            }

            // Resolve entry module ID
            let entry_id = match graph.path_to_id.get(&entry) {
                Some(&id) => id,
                None => {
                    eprintln!(
                        "error: entry file '{}' not found in graph",
                        entry.display()
                    );
                    eprintln!("hint: is it reachable from the project root?");
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
                ignore,
            };
            let result = query::trace(&graph, entry_id, &opts);
            let entry_rel = entry
                .strip_prefix(&root)
                .unwrap_or(&entry)
                .to_string_lossy()
                .into_owned();

            // Save snapshot if requested (works with any mode)
            if let Some(ref save_path) = save {
                let snapshot = result.to_snapshot(&entry_rel);
                let data = serde_json::to_string_pretty(&snapshot).unwrap();
                std::fs::write(save_path, data).unwrap_or_else(|e| {
                    eprintln!("error: cannot write snapshot '{}': {e}", save_path.display());
                    std::process::exit(1);
                });
                eprintln!("Snapshot saved to {}", save_path.display());
            }

            // Resolve --chain/--cut argument: file path or package name
            struct ResolvedTarget {
                target: query::ChainTarget,
                label: String,
                exists: bool,
            }
            let resolve_target = |arg: &str| -> ResolvedTarget {
                let looks_like_path = looks_like_path(arg, valid_extensions);
                if looks_like_path {
                    let target_path = root.join(arg).canonicalize().unwrap_or_else(|e| {
                        eprintln!("error: cannot find file '{arg}': {e}");
                        std::process::exit(1);
                    });
                    match graph.path_to_id.get(&target_path) {
                        Some(&id) => {
                            let p = &graph.module(id).path;
                            let label = p.strip_prefix(&root)
                                .unwrap_or(p)
                                .to_string_lossy()
                                .into_owned();
                            ResolvedTarget {
                                target: query::ChainTarget::Module(id),
                                label,
                                exists: true,
                            }
                        }
                        None => {
                            eprintln!("error: '{arg}' is not in the dependency graph");
                            eprintln!("hint: is it reachable from the entry point?");
                            std::process::exit(1);
                        }
                    }
                } else {
                    ResolvedTarget {
                        target: query::ChainTarget::Package(arg.to_string()),
                        label: arg.to_string(),
                        exists: graph.package_map.contains_key(arg),
                    }
                }
            };

            // Handle --chain mode
            if let Some(ref chain_arg) = chain {
                let resolved = resolve_target(chain_arg);
                let chains =
                    query::find_all_chains(&graph, entry_id, &resolved.target, include_dynamic);
                if json {
                    report::print_chains_json(
                        &graph,
                        &chains,
                        &resolved.label,
                        &root,
                        resolved.exists,
                    );
                } else {
                    report::print_chains(&graph, &chains, &resolved.label, &root, resolved.exists);
                }
                if chains.is_empty() {
                    std::process::exit(1);
                }
                return;
            }

            // Handle --cut mode
            if let Some(ref cut_arg) = cut {
                let resolved = resolve_target(cut_arg);
                let chains =
                    query::find_all_chains(&graph, entry_id, &resolved.target, include_dynamic);
                let cuts = query::find_cut_modules(
                    &graph,
                    &chains,
                    entry_id,
                    &resolved.target,
                    top,
                    include_dynamic,
                );
                if json {
                    report::print_cut_json(
                        &graph,
                        &cuts,
                        &chains,
                        &resolved.label,
                        &root,
                        resolved.exists,
                    );
                } else {
                    report::print_cut(
                        &graph,
                        &cuts,
                        &chains,
                        &resolved.label,
                        &root,
                        resolved.exists,
                    );
                }
                if chains.is_empty() {
                    std::process::exit(1);
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
                let diff_output = query::diff_snapshots(&saved, &result.to_snapshot(&entry_rel));
                report::print_diff(&diff_output, &saved.entry, &entry_rel);
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

                let diff_rel = {
                    let dr = lang::detect_project(&diff_entry)
                        .map(|(r, _)| r)
                        .unwrap_or_else(|| diff_entry.parent().unwrap_or(&diff_entry).to_path_buf());
                    diff_entry
                        .strip_prefix(&dr)
                        .unwrap_or(&diff_entry)
                        .to_string_lossy()
                        .into_owned()
                };
                let diff_snapshot = if let Some(&diff_id) = graph.path_to_id.get(&diff_entry) {
                    // Same graph — trace directly
                    query::trace(&graph, diff_id, &opts).to_snapshot(&diff_rel)
                } else {
                    // Different package — build a second graph from its root
                    let (diff_root, diff_kind) = lang::detect_project(&diff_entry).unwrap_or_else(|| {
                        let ext = diff_entry.extension().and_then(|e| e.to_str()).unwrap_or("(none)");
                        eprintln!("error: unsupported file type '.{ext}'");
                        eprintln!("hint: chainsaw supports TypeScript/JavaScript and Python files");
                        std::process::exit(1);
                    });
                    let diff_lang: Box<dyn LanguageSupport> = match diff_kind {
                        lang::ProjectKind::TypeScript => {
                            Box::new(lang::typescript::TypeScriptSupport::new(&diff_root))
                        }
                        lang::ProjectKind::Python => {
                            Box::new(lang::python::PythonSupport::new(&diff_root))
                        }
                    };
                    let diff_load = load_or_build_graph(&diff_entry, &diff_root, no_cache, diff_lang.as_ref());
                    let diff_graph = diff_load.graph;
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
                    query::trace(&diff_graph, diff_id, &opts).to_snapshot(&diff_rel)
                };

                let diff_output = query::diff_snapshots(&result.to_snapshot(&entry_rel), &diff_snapshot);
                report::print_diff(&diff_output, &entry_rel, &diff_rel);
                return;
            }

            // Normal trace output
            if json {
                report::print_trace_json(&graph, &result, &entry, &root, top_modules);
            } else {
                report::print_trace(&graph, &result, &entry, &root, top, top_modules, include_dynamic);
            }

            let elapsed = start.elapsed();
            eprintln!("\nCompleted in {:.1}ms", elapsed.as_secs_f64() * 1000.0);
        }

        Commands::Diff { a, b } => {
            let load_snapshot = |path: &Path| -> query::TraceSnapshot {
                let data = std::fs::read_to_string(path).unwrap_or_else(|e| {
                    eprintln!("error: cannot read snapshot '{}': {e}", path.display());
                    std::process::exit(1);
                });
                serde_json::from_str(&data).unwrap_or_else(|e| {
                    eprintln!("error: invalid snapshot '{}': {e}", path.display());
                    std::process::exit(1);
                })
            };

            let snap_a = load_snapshot(&a);
            let snap_b = load_snapshot(&b);
            let diff_output = query::diff_snapshots(&snap_a, &snap_b);

            let label_a = a.file_name().unwrap_or(a.as_os_str()).to_string_lossy();
            let label_b = b.file_name().unwrap_or(b.as_os_str()).to_string_lossy();
            report::print_diff(&diff_output, &label_a, &label_b);
        }

        Commands::Packages { entry, json, no_cache } => {
            let entry = entry.canonicalize().unwrap_or_else(|e| {
                eprintln!("error: cannot find entry file '{}': {e}", entry.display());
                std::process::exit(1);
            });

            if entry.is_dir() {
                eprintln!("error: '{}' is a directory, not a source file", entry.display());
                std::process::exit(1);
            }

            let (root, kind) = lang::detect_project(&entry).unwrap_or_else(|| {
                let ext = entry.extension().and_then(|e| e.to_str()).unwrap_or("(none)");
                eprintln!("error: unsupported file type '.{ext}'");
                eprintln!("hint: chainsaw supports TypeScript/JavaScript and Python files");
                std::process::exit(1);
            });

            let lang_support: Box<dyn LanguageSupport> = match kind {
                lang::ProjectKind::TypeScript => {
                    Box::new(lang::typescript::TypeScriptSupport::new(&root))
                }
                lang::ProjectKind::Python => {
                    Box::new(lang::python::PythonSupport::new(&root))
                }
            };

            let graph = load_or_build_graph(&entry, &root, no_cache, lang_support.as_ref()).graph;

            if json {
                report::print_packages_json(&graph);
            } else {
                report::print_packages(&graph);
            }
        }
    }
}

struct LoadResult {
    graph: graph::ModuleGraph,
    unresolvable_dynamic: usize,
    from_cache: bool,
}

fn load_or_build_graph(
    entry: &Path,
    root: &Path,
    no_cache: bool,
    lang: &dyn LanguageSupport,
) -> LoadResult {
    let mut cache = if no_cache {
        cache::ParseCache::new()
    } else {
        cache::ParseCache::load(root)
    };

    // Tier 1: try whole-graph cache (nothing changed since last run)
    if !no_cache {
        let resolve_fn = |spec: &str| lang.resolve(root, spec).is_some();
        if let Some((graph, unresolvable_dynamic)) = cache.try_load_graph(entry, &resolve_fn) {
            return LoadResult {
                graph,
                unresolvable_dynamic,
                from_cache: true,
            };
        }
    }

    // Tier 2: BFS walk with per-file parse cache
    let result = walker::build_graph(entry, root, lang, &mut cache);
    cache.save(
        root,
        entry,
        &result.graph,
        result.unresolved_specifiers,
        result.unresolvable_dynamic,
    );
    LoadResult {
        graph: result.graph,
        unresolvable_dynamic: result.unresolvable_dynamic,
        from_cache: false,
    }
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
}
