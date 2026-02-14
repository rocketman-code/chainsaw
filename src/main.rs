#![allow(clippy::too_many_lines)]

use std::path::{Path, PathBuf};
use std::time::Instant;

use clap::{CommandFactory, Parser, Subcommand};
use clap_complete::Shell;

use chainsaw::{cache, graph, lang, query, report, walker};
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
    let sc = report::StderrColor::new();

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
            quiet,
            limit,
            max_weight,
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

            // Validate mutually exclusive flags
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
                eprintln!("{} {} cannot be used together", sc.error("error:"), query_flags.join(" and "));
                std::process::exit(1);
            }

            // Load or build graph
            let (load_result, _cache_write) = load_or_build_graph(&entry, &root, no_cache, lang_support.as_ref());
            let graph = load_result.graph;
            let unresolvable_count = load_result.unresolvable_dynamic_count;
            if !quiet {
                eprintln!(
                    "{} ({} modules) in {:.1}ms",
                    sc.status(if load_result.from_cache { "Loaded cached graph" } else { "Built graph" }),
                    graph.module_count(),
                    start.elapsed().as_secs_f64() * 1000.0
                );
                if unresolvable_count > 0 && !load_result.from_cache {
                    eprintln!(
                        "{} {} dynamic import{} with non-literal argument{} could not be traced:",
                        sc.warning("warning:"),
                        unresolvable_count,
                        if unresolvable_count == 1 { "" } else { "s" },
                        if unresolvable_count == 1 { "" } else { "s" },
                    );
                    for (path, count) in &load_result.unresolvable_dynamic_files {
                        let rel = report::relative_path(path, &root);
                        eprintln!("  {rel} ({count})");
                    }
                }
            }

            // Resolve entry module ID
            let Some(&entry_id) = graph.path_to_id.get(&entry) else {
                eprintln!(
                    "error: entry file '{}' not found in graph",
                    entry.display()
                );
                eprintln!("hint: is it reachable from the project root?");
                std::process::exit(1);
            };

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
                if !quiet {
                    eprintln!("{} to {}", sc.status("Snapshot saved"), save_path.display());
                }
            }

            // Resolve --chain/--cut argument: file path or package name
            let resolve_target = |arg: &str| -> ResolvedTarget {
                let looks_like_path = looks_like_path(arg, valid_extensions);
                if looks_like_path {
                    let target_path = root.join(arg).canonicalize().unwrap_or_else(|e| {
                        eprintln!("error: cannot find file '{arg}': {e}");
                        std::process::exit(1);
                    });
                    let Some(&id) = graph.path_to_id.get(&target_path) else {
                        eprintln!("error: '{arg}' is not in the dependency graph");
                        eprintln!("hint: is it reachable from the entry point?");
                        std::process::exit(1);
                    };
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
                if resolved.target == query::ChainTarget::Module(entry_id) {
                    eprintln!("error: --chain target is the entry point itself");
                    eprintln!("hint: --chain finds import chains from the entry to a dependency");
                    std::process::exit(1);
                }
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
                if resolved.target == query::ChainTarget::Module(entry_id) {
                    eprintln!("error: --cut target is the entry point itself");
                    eprintln!("hint: --cut finds where to sever import chains to a dependency");
                    std::process::exit(1);
                }
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
                report::print_diff(&diff_output, &saved.entry, &entry_rel, limit);
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
                if diff_entry == entry {
                    eprintln!("{} both entry points are the same file, diff will be empty", sc.warning("warning:"));
                }

                let diff_rel = {
                    let dr = lang::detect_project(&diff_entry)
                        .map_or_else(|| diff_entry.parent().unwrap_or(&diff_entry).to_path_buf(), |(r, _)| r);
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
                    let (diff_load, _diff_cache_write) = load_or_build_graph(&diff_entry, &diff_root, no_cache, diff_lang.as_ref());
                    let diff_graph = diff_load.graph;
                    let Some(&diff_id) = diff_graph.path_to_id.get(&diff_entry) else {
                        eprintln!(
                            "error: diff entry file '{}' not found in graph",
                            diff_entry.display()
                        );
                        std::process::exit(1);
                    };
                    query::trace(&diff_graph, diff_id, &opts).to_snapshot(&diff_rel)
                };

                let diff_output = query::diff_snapshots(&result.to_snapshot(&entry_rel), &diff_snapshot);
                report::print_diff(&diff_output, &entry_rel, &diff_rel, limit);
                return;
            }

            // Normal trace output
            if json {
                report::print_trace_json(&graph, &result, &entry, &root, top_modules);
            } else {
                report::print_trace(&graph, &result, &entry, &root, top, top_modules, include_dynamic);
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

            let elapsed = start.elapsed();
            if !quiet {
                eprintln!("\n{} in {:.1}ms", sc.status("Completed"), elapsed.as_secs_f64() * 1000.0);
            }
        }

        Commands::Diff { a, b, limit } => {
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

            report::print_diff(&diff_output, &snap_a.entry, &snap_b.entry, limit);
        }

        Commands::Packages { entry, json, no_cache, top, quiet } => {
            let start = Instant::now();

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

            let (load_result, _cache_write) = load_or_build_graph(&entry, &root, no_cache, lang_support.as_ref());
            if !quiet {
                eprintln!(
                    "{} ({} modules) in {:.1}ms",
                    sc.status(if load_result.from_cache { "Loaded cached graph" } else { "Built graph" }),
                    load_result.graph.module_count(),
                    start.elapsed().as_secs_f64() * 1000.0
                );
            }

            if json {
                report::print_packages_json(&load_result.graph, top);
            } else {
                report::print_packages(&load_result.graph, top);
            }
        }

        Commands::Completions { shell } => {
            clap_complete::generate(shell, &mut Cli::command(), "chainsaw", &mut std::io::stdout());
        }
    }
}

struct LoadResult {
    graph: graph::ModuleGraph,
    unresolvable_dynamic_count: usize,
    unresolvable_dynamic_files: Vec<(std::path::PathBuf, usize)>,
    from_cache: bool,
}

fn load_or_build_graph(
    entry: &Path,
    root: &Path,
    no_cache: bool,
    lang: &dyn LanguageSupport,
) -> (LoadResult, cache::CacheWriteHandle) {
    let mut cache = if no_cache {
        cache::ParseCache::new()
    } else {
        cache::ParseCache::load(root)
    };

    // Tier 1: try whole-graph cache
    if !no_cache {
        let resolve_fn = |spec: &str| lang.resolve(root, spec).is_some();
        match cache.try_load_graph(entry, &resolve_fn) {
            cache::GraphCacheResult::Hit { graph, unresolvable_dynamic, unresolved_specifiers, needs_resave } => {
                let handle = if needs_resave {
                    cache.save(root, entry, &graph, unresolved_specifiers, unresolvable_dynamic)
                } else {
                    cache::CacheWriteHandle::none()
                };
                return (
                    LoadResult {
                        graph,
                        unresolvable_dynamic_count: unresolvable_dynamic,
                        unresolvable_dynamic_files: Vec::new(),
                        from_cache: true,
                    },
                    handle,
                );
            }
            cache::GraphCacheResult::Stale {
                mut graph,
                unresolvable_dynamic,
                changed_files,
            } => {
                // Tier 1.5: incremental update — re-parse only changed files,
                // reuse the cached graph if imports haven't changed.
                if let Some(result) = try_incremental_update(
                    &mut cache,
                    &mut graph,
                    &changed_files,
                    unresolvable_dynamic,
                    lang,
                ) {
                    graph.compute_package_info();
                    let handle = cache.save_incremental(
                        root,
                        entry,
                        &graph,
                        &changed_files,
                        result.unresolvable_dynamic,
                    );
                    return (
                        LoadResult {
                            graph,
                            unresolvable_dynamic_count: result.unresolvable_dynamic,
                            unresolvable_dynamic_files: Vec::new(),
                            from_cache: true,
                        },
                        handle,
                    );
                }
                // Imports changed — fall through to full BFS
            }
            cache::GraphCacheResult::Miss => {}
        }
    }

    // Tier 2: BFS walk with per-file parse cache
    let result = walker::build_graph(entry, root, lang, &mut cache);
    let unresolvable_count: usize = result.unresolvable_dynamic.iter().map(|(_, c)| c).sum();
    let handle = cache.save(
        root,
        entry,
        &result.graph,
        result.unresolved_specifiers,
        unresolvable_count,
    );
    (
        LoadResult {
            graph: result.graph,
            unresolvable_dynamic_count: unresolvable_count,
            unresolvable_dynamic_files: result.unresolvable_dynamic,
            from_cache: false,
        },
        handle,
    )
}

struct IncrementalResult {
    unresolvable_dynamic: usize,
}

/// Try to incrementally update the cached graph when only a few files changed.
/// Re-parses the changed files and checks if their imports match the old parse.
/// Returns None if imports changed (caller should fall back to full BFS).
#[allow(clippy::cast_possible_wrap, clippy::cast_sign_loss)]
fn try_incremental_update(
    cache: &mut cache::ParseCache,
    graph: &mut graph::ModuleGraph,
    changed_files: &[PathBuf],
    old_unresolvable_total: usize,
    lang: &dyn LanguageSupport,
) -> Option<IncrementalResult> {
    let mut unresolvable_delta: isize = 0;

    for path in changed_files {
        // Get old imports without mtime check
        let old_result = cache.lookup_unchecked(path)?;
        let old_import_count = old_result.imports.len();
        let old_unresolvable = old_result.unresolvable_dynamic;
        let old_imports: Vec<_> = old_result
            .imports
            .iter()
            .map(|i| (i.specifier.as_str(), i.kind))
            .collect();

        // Re-parse the changed file
        let source = std::fs::read_to_string(path).ok()?;
        let new_result = lang.parse(path, &source).ok()?;

        // Compare import lists — if anything changed, bail out
        if new_result.imports.len() != old_import_count
            || new_result
                .imports
                .iter()
                .zip(old_imports.iter())
                .any(|(new, &(old_spec, old_kind))| {
                    new.specifier != old_spec || new.kind != old_kind
                })
        {
            return None;
        }

        // Track unresolvable dynamic count changes
        unresolvable_delta +=
            new_result.unresolvable_dynamic as isize - old_unresolvable as isize;

        // Update file size in graph
        let mid = *graph.path_to_id.get(path)?;
        let new_size = source.len() as u64;
        graph.modules[mid.0 as usize].size_bytes = new_size;

        // Update parse cache entry
        let dir = path.parent().unwrap_or(Path::new("."));
        let resolved_paths: Vec<Option<PathBuf>> = new_result
            .imports
            .iter()
            .map(|imp| lang.resolve(dir, &imp.specifier))
            .collect();
        cache.insert(path.clone(), &new_result, &resolved_paths);
    }

    let new_total = (old_unresolvable_total as isize + unresolvable_delta).max(0) as usize;
    Some(IncrementalResult {
        unresolvable_dynamic: new_total,
    })
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
