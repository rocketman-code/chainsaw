// compact_str 0.8 (oxc_span) + 0.9 (oxc_resolver) — transitive, out of our control.
#![allow(clippy::multiple_crate_versions)]

use std::path::{Path, PathBuf};
use std::time::Instant;

use clap::{Args, CommandFactory, Parser, Subcommand};
use clap_complete::Shell;

use chainsaw::{error::Error, git, loader, query, report};

#[derive(Parser)]
#[command(
    name = "chainsaw",
    version,
    about = "TypeScript/JavaScript and Python dependency graph analyzer",
    after_help = "Repository: https://github.com/rocketman-code/chainsaw"
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
    Trace(TraceArgs),

    /// Compare dependency weight across snapshots or git refs
    Diff {
        /// Snapshot file or git ref ("before" / "baseline").
        /// If only one arg, compares working tree against this.
        a: String,

        /// Snapshot file or git ref ("after" / "current").
        /// If omitted, the working tree is the "before" side.
        b: Option<String>,

        /// Entry point to trace (required when comparing git refs)
        #[arg(long)]
        entry: Option<PathBuf>,

        /// Max packages to show in diff output (-1 for all)
        #[arg(long, default_value_t = 10, allow_hyphen_values = true)]
        limit: i32,

        /// Suppress informational output (timing, warnings)
        #[arg(long, short)]
        quiet: bool,
    },

    /// List all third-party packages in the dependency graph
    Packages(PackagesArgs),

    /// Generate shell completions
    Completions {
        /// Shell to generate completions for
        shell: Shell,
    },
}

#[derive(Args)]
#[allow(clippy::struct_excessive_bools)] // CLI flags are inherently boolean
struct TraceArgs {
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

    /// Show top N modules by exclusive weight — bytes not reachable through any other path (0 to hide, -1 for all)
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
}

#[derive(Args)]
struct PackagesArgs {
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
}

#[allow(clippy::cast_possible_truncation, clippy::cast_sign_loss)]
fn parse_size(s: &str) -> Result<u64, String> {
    let s = s.trim();
    let (num_str, multiplier) = s
        .strip_suffix("MB")
        .map(|n| (n.trim(), 1_000_000.0))
        .or_else(|| s.strip_suffix("KB").map(|n| (n.trim(), 1_000.0)))
        .or_else(|| s.strip_suffix("B").map(|n| (n.trim(), 1.0)))
        .unwrap_or((s, 1.0));
    let value: f64 = num_str.parse().map_err(|_| format!("invalid size: {s}"))?;
    Ok((value * multiplier) as u64)
}

struct ResolvedTarget {
    target: query::ChainTarget,
    label: String,
    exists: bool,
}

/// Cap the rayon thread pool to avoid VFS lock contention in the kernel.
///
/// Filesystem-heavy workloads (stat, open, read) hit diminishing returns beyond
/// ~8 threads because kernel VFS locks serialize concurrent access to the same
/// directory inodes. On a 14-core machine, 8 threads is 8% faster than 14 on
/// cold builds and 21% faster on cached builds, with 2x less total kernel time.
const MAX_WALKER_THREADS: usize = 8;

fn main() {
    let cpus = std::thread::available_parallelism()
        .map(std::num::NonZero::get)
        .unwrap_or(1);
    let threads = cpus.min(MAX_WALKER_THREADS);
    rayon::ThreadPoolBuilder::new()
        .num_threads(threads)
        .build_global()
        .ok(); // ignore if already initialized (e.g. in tests)

    let cli = Cli::parse();
    let no_color = cli.no_color;
    let sc = report::StderrColor::new(no_color);

    if let Err(e) = run(cli.command, no_color, sc) {
        eprintln!("{} {e}", sc.error("error:"));
        if let Some(hint) = e.hint() {
            eprintln!("hint: {hint}");
        }
        std::process::exit(1);
    }
}

fn run(command: Commands, no_color: bool, sc: report::StderrColor) -> Result<(), Error> {
    match command {
        Commands::Trace(args) => run_trace(args, no_color, sc),

        Commands::Diff { a, b, entry, limit, quiet } => {
            run_diff(a, b, entry, limit, quiet, no_color, sc)
        }

        Commands::Packages(ref args) => run_packages(args, no_color, sc),

        Commands::Completions { shell } => {
            clap_complete::generate(
                shell,
                &mut Cli::command(),
                "chainsaw",
                &mut std::io::stdout(),
            );
            Ok(())
        }
    }
}

fn run_trace(args: TraceArgs, no_color: bool, sc: report::StderrColor) -> Result<(), Error> {
    let start = Instant::now();

    // Validate mutually exclusive flags before loading graph
    let query_flags: Vec<&str> = [
        args.chain.as_ref().map(|_| "--chain"),
        args.cut.as_ref().map(|_| "--cut"),
        args.diff.as_ref().map(|_| "--diff"),
        args.diff_from.as_ref().map(|_| "--diff-from"),
    ]
    .into_iter()
    .flatten()
    .collect();
    if query_flags.len() > 1 {
        return Err(Error::MutuallyExclusiveFlags(query_flags.join(" and ")));
    }

    // Load or build graph
    let (loaded, _cache_write) = loader::load_graph(&args.entry, args.no_cache)?;
    if !args.quiet {
        print_build_status(&loaded, start, sc);
    }

    // Resolve entry module ID
    let Some(&entry_id) = loaded.graph.path_to_id.get(&loaded.entry) else {
        return Err(Error::EntryNotInGraph(loaded.entry));
    };

    let opts = query::TraceOptions {
        include_dynamic: args.include_dynamic,
        top_n: args.top,
        ignore: args.ignore,
    };
    let result = query::trace(&loaded.graph, entry_id, &opts);
    let entry_rel = entry_label(&loaded.entry, &loaded.root);

    // Save snapshot if requested (works with any mode)
    if let Some(ref save_path) = args.save {
        save_snapshot(save_path, &result, &entry_rel, args.quiet, sc)?;
    }

    // Handle --chain/--cut/--diff-from/--diff modes
    if let Some(ref chain_arg) = args.chain {
        if !handle_chain(&loaded, entry_id, chain_arg, args.include_dynamic, args.json, no_color)? {
            std::process::exit(1);
        }
        return Ok(());
    }
    if let Some(ref cut_arg) = args.cut {
        if !handle_cut(&loaded, entry_id, cut_arg, args.top, args.include_dynamic, args.json, no_color)? {
            std::process::exit(1);
        }
        return Ok(());
    }
    if let Some(ref snapshot_path) = args.diff_from {
        handle_diff_from(snapshot_path, &result, &entry_rel, args.limit, no_color)?;
        return Ok(());
    }
    if let Some(ref diff_path) = args.diff {
        return handle_diff(diff_path, &loaded, &result, &entry_rel, &opts, args.no_cache, args.limit, no_color, sc);
    }

    // Normal trace output
    if args.json {
        report::print_trace_json(
            &loaded.graph,
            &result,
            &loaded.entry,
            &loaded.root,
            args.top_modules,
        );
    } else {
        let display_opts = report::DisplayOpts {
            top: args.top,
            top_modules: args.top_modules,
            include_dynamic: args.include_dynamic,
            no_color,
            max_weight: args.max_weight,
        };
        report::print_trace(
            &loaded.graph,
            &result,
            &loaded.entry,
            &loaded.root,
            &display_opts,
        );
    }

    if args.max_weight.is_some_and(|t| result.static_weight > t) {
        std::process::exit(1);
    }

    if !args.quiet {
        eprintln!(
            "\n{} in {:.1}ms",
            sc.status("Completed"),
            start.elapsed().as_secs_f64() * 1000.0
        );
    }

    Ok(())
}

fn print_build_status(loaded: &loader::LoadedGraph, start: Instant, sc: report::StderrColor) {
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
    for w in &loaded.file_warnings {
        eprintln!("{} {w}", sc.warning("warning:"));
    }
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

fn save_snapshot(
    path: &Path,
    result: &query::TraceResult,
    entry_rel: &str,
    quiet: bool,
    sc: report::StderrColor,
) -> Result<(), Error> {
    let snapshot = result.to_snapshot(entry_rel);
    let data = serde_json::to_string_pretty(&snapshot).unwrap();
    std::fs::write(path, &data)
        .map_err(|e| Error::SnapshotWrite(path.to_path_buf(), e))?;
    if !quiet {
        eprintln!(
            "{} to {}",
            sc.status("Snapshot saved"),
            path.display()
        );
    }
    Ok(())
}

fn load_snapshot(path: &Path) -> Result<query::TraceSnapshot, Error> {
    let data = std::fs::read_to_string(path)
        .map_err(|e| Error::SnapshotRead(path.to_path_buf(), e))?;
    serde_json::from_str(&data)
        .map_err(|e| Error::SnapshotParse(path.to_path_buf(), e))
}

fn resolve_target(arg: &str, loaded: &loader::LoadedGraph) -> ResolvedTarget {
    if looks_like_path(arg, loaded.valid_extensions) {
        if let Ok(target_path) = loaded.root.join(arg).canonicalize() {
            if let Some(&id) = loaded.graph.path_to_id.get(&target_path) {
                let p = &loaded.graph.module(id).path;
                let label = p
                    .strip_prefix(&loaded.root)
                    .unwrap_or(p)
                    .to_string_lossy()
                    .into_owned();
                return ResolvedTarget {
                    target: query::ChainTarget::Module(id),
                    label,
                    exists: true,
                };
            }
        }
        // File doesn't exist or isn't in the graph — fall through to
        // package name lookup. Handles packages like "six.py" or
        // "highlight.js" whose names match file extensions.
    }
    let name = arg.to_string();
    let exists = loaded.graph.package_map.contains_key(arg);
    ResolvedTarget {
        target: query::ChainTarget::Package(name.clone()),
        label: name,
        exists,
    }
}

fn check_entry_target(
    resolved: &ResolvedTarget,
    entry_id: chainsaw::graph::ModuleId,
    flag_name: &str,
) -> Result<(), Error> {
    if resolved.target == query::ChainTarget::Module(entry_id) {
        return Err(Error::TargetIsEntryPoint(flag_name.to_string()));
    }
    Ok(())
}

fn handle_chain(
    loaded: &loader::LoadedGraph,
    entry_id: chainsaw::graph::ModuleId,
    chain_arg: &str,
    include_dynamic: bool,
    json: bool,
    no_color: bool,
) -> Result<bool, Error> {
    let resolved = resolve_target(chain_arg, loaded);
    check_entry_target(&resolved, entry_id, "--chain")?;
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
    Ok(!chains.is_empty())
}

#[allow(clippy::too_many_arguments)] // private dispatch, called from one site
fn handle_cut(
    loaded: &loader::LoadedGraph,
    entry_id: chainsaw::graph::ModuleId,
    cut_arg: &str,
    top: i32,
    include_dynamic: bool,
    json: bool,
    no_color: bool,
) -> Result<bool, Error> {
    let resolved = resolve_target(cut_arg, loaded);
    check_entry_target(&resolved, entry_id, "--cut")?;
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
    Ok(!chains.is_empty())
}

fn handle_diff_from(
    snapshot_path: &Path,
    result: &query::TraceResult,
    entry_rel: &str,
    limit: i32,
    no_color: bool,
) -> Result<(), Error> {
    let saved = load_snapshot(snapshot_path)?;
    let diff_output =
        query::diff_snapshots(&saved, &result.to_snapshot(entry_rel));
    report::print_diff(
        &diff_output,
        &saved.entry,
        entry_rel,
        limit,
        no_color,
    );
    Ok(())
}

#[allow(clippy::too_many_arguments)] // private dispatch, called from one site
fn handle_diff(
    diff_path: &Path,
    loaded: &loader::LoadedGraph,
    result: &query::TraceResult,
    entry_rel: &str,
    opts: &query::TraceOptions,
    no_cache: bool,
    limit: i32,
    no_color: bool,
    sc: report::StderrColor,
) -> Result<(), Error> {
    let diff_entry = diff_path
        .canonicalize()
        .map_err(|e| Error::EntryNotFound(diff_path.to_path_buf(), e))?;
    if diff_entry == loaded.entry {
        eprintln!(
            "{} both entry points are the same file, diff will be empty",
            sc.warning("warning:")
        );
    }

    let diff_snapshot =
        if let Some(&diff_id) = loaded.graph.path_to_id.get(&diff_entry) {
            // Same graph — trace directly
            let diff_rel = entry_label(&diff_entry, &loaded.root);
            query::trace(&loaded.graph, diff_id, opts).to_snapshot(&diff_rel)
        } else {
            // Different project — build second graph
            let (diff_loaded, _diff_cache_write) =
                loader::load_graph(&diff_entry, no_cache)?;
            let Some(&diff_id) =
                diff_loaded.graph.path_to_id.get(&diff_loaded.entry)
            else {
                return Err(Error::EntryNotInGraph(diff_loaded.entry));
            };
            let diff_rel = entry_label(&diff_loaded.entry, &diff_loaded.root);
            query::trace(&diff_loaded.graph, diff_id, opts)
                .to_snapshot(&diff_rel)
        };

    let diff_output =
        query::diff_snapshots(&result.to_snapshot(entry_rel), &diff_snapshot);
    report::print_diff(
        &diff_output,
        entry_rel,
        &diff_snapshot.entry,
        limit,
        no_color,
    );
    Ok(())
}

fn run_packages(args: &PackagesArgs, no_color: bool, sc: report::StderrColor) -> Result<(), Error> {
    let start = Instant::now();
    let (loaded, _cache_write) = loader::load_graph(&args.entry, args.no_cache)?;
    if !args.quiet {
        print_build_status(&loaded, start, sc);
    }

    if args.json {
        report::print_packages_json(&loaded.graph, args.top);
    } else {
        report::print_packages(&loaded.graph, args.top, no_color);
    }
    Ok(())
}

// ---------------------------------------------------------------------------
// Diff across snapshots and/or git refs
// ---------------------------------------------------------------------------

#[allow(clippy::too_many_arguments)]
fn run_diff(
    a: String,
    b: Option<String>,
    entry: Option<PathBuf>,
    limit: i32,
    quiet: bool,
    no_color: bool,
    sc: report::StderrColor,
) -> Result<(), Error> {
    let start = Instant::now();

    // Determine repo root (needed for git ref detection and worktrees).
    // Use CWD — if we're not in a git repo, classify_diff_arg will fail
    // gracefully for git refs and succeed for snapshot files.
    let cwd = std::env::current_dir()
        .map_err(|e| Error::GitError(format!("cannot determine working directory: {e}")))?;
    let repo_root = git::repo_root(&cwd).ok();

    // Classify arguments.
    let fallback = cwd.clone();
    let root = repo_root.as_deref().unwrap_or(&fallback);
    let arg_a = git::classify_diff_arg(&a, root)?;
    let arg_b = b.map(|s| git::classify_diff_arg(&s, root)).transpose()?;

    let has_ref = matches!(arg_a, git::DiffArg::GitRef(_))
        || matches!(&arg_b, Some(git::DiffArg::GitRef(_)));

    // --entry is required when any side is a git ref.
    if has_ref && entry.is_none() {
        return Err(Error::EntryRequired);
    }

    // Build snapshots from each side.
    let (snap_a, label_a, _wt_a) = build_diff_side(&arg_a, entry.as_deref(), root, quiet, sc)?;
    let (snap_b, label_b, _wt_b) = match arg_b {
        Some(ref arg) => {
            let (s, l, w) = build_diff_side(arg, entry.as_deref(), root, quiet, sc)?;
            (s, l, w)
        }
        None => {
            // One arg: working tree is side A, the arg becomes side B.
            // Swap: what we built as "a" is actually side B, working tree is side A.
            let entry_path = entry.as_ref().ok_or(Error::EntryRequired)?;
            let wt_snap = build_snapshot_from_working_tree(entry_path, quiet, sc)?;
            let wt_label = wt_snap.entry.clone();
            // snap_a is the ref/snapshot side (B), wt is current working tree (A)
            return finish_diff(&wt_snap, &wt_label, &snap_a, &label_a, limit, no_color, start, quiet, sc);
        }
    };

    finish_diff(&snap_a, &label_a, &snap_b, &label_b, limit, no_color, start, quiet, sc)
}

#[allow(clippy::too_many_arguments)] // private dispatch, called from one site
fn finish_diff(
    snap_a: &query::TraceSnapshot,
    label_a: &str,
    snap_b: &query::TraceSnapshot,
    label_b: &str,
    limit: i32,
    no_color: bool,
    start: Instant,
    quiet: bool,
    sc: report::StderrColor,
) -> Result<(), Error> {
    let diff_output = query::diff_snapshots(snap_a, snap_b);
    report::print_diff(&diff_output, label_a, label_b, limit, no_color);
    if !quiet {
        eprintln!(
            "\n{} in {:.1}ms",
            sc.status("Compared"),
            start.elapsed().as_secs_f64() * 1000.0
        );
    }
    Ok(())
}

/// Build a snapshot from one side of a diff (either a snapshot file or a git ref).
/// Returns (snapshot, display_label, optional_worktree_handle).
fn build_diff_side(
    arg: &git::DiffArg,
    entry: Option<&Path>,
    repo_root: &Path,
    quiet: bool,
    sc: report::StderrColor,
) -> Result<(query::TraceSnapshot, String, Option<git::TempWorktree>), Error> {
    match arg {
        git::DiffArg::Snapshot(path) => {
            let snap = load_snapshot(path)?;
            let label = snap.entry.clone();
            Ok((snap, label, None))
        }
        git::DiffArg::GitRef(git_ref) => {
            let entry = entry.ok_or(Error::EntryRequired)?;
            let (snap, wt) = build_snapshot_from_ref(repo_root, git_ref, entry, quiet, sc)?;
            let label = format!("{} ({})", entry.display(), git_ref);
            Ok((snap, label, Some(wt)))
        }
    }
}

/// Create a worktree at the given ref, build the graph, trace, and return the snapshot.
fn build_snapshot_from_ref(
    repo_root: &Path,
    git_ref: &str,
    entry: &Path,
    quiet: bool,
    sc: report::StderrColor,
) -> Result<(query::TraceSnapshot, git::TempWorktree), Error> {
    let start = Instant::now();
    let wt = git::create_worktree(repo_root, git_ref)?;
    let wt_entry = wt.path().join(entry);
    let (loaded, _cache_write) = loader::load_graph(&wt_entry, true)?; // no-cache
    if !quiet {
        eprintln!(
            "{} {} at {} ({} modules) in {:.1}ms",
            sc.status("Built graph"),
            entry.display(),
            git_ref,
            loaded.graph.module_count(),
            start.elapsed().as_secs_f64() * 1000.0,
        );
    }
    let Some(&entry_id) = loaded.graph.path_to_id.get(&loaded.entry) else {
        return Err(Error::EntryNotInGraph(loaded.entry));
    };
    let opts = query::TraceOptions {
        include_dynamic: false,
        top_n: 0,
        ignore: vec![],
    };
    let result = query::trace(&loaded.graph, entry_id, &opts);
    let label = format!("{} ({})", entry.display(), git_ref);
    Ok((result.to_snapshot(&label), wt))
}

/// Build a snapshot from the current working tree.
fn build_snapshot_from_working_tree(
    entry: &Path,
    quiet: bool,
    sc: report::StderrColor,
) -> Result<query::TraceSnapshot, Error> {
    let start = Instant::now();
    let (loaded, _cache_write) = loader::load_graph(entry, false)?;
    if !quiet {
        print_build_status(&loaded, start, sc);
    }
    let Some(&entry_id) = loaded.graph.path_to_id.get(&loaded.entry) else {
        return Err(Error::EntryNotInGraph(loaded.entry));
    };
    let opts = query::TraceOptions {
        include_dynamic: false,
        top_n: 0,
        ignore: vec![],
    };
    let result = query::trace(&loaded.graph, entry_id, &opts);
    let label = entry_label(&loaded.entry, &loaded.root);
    Ok(result.to_snapshot(&label))
}

/// Build a display label for an entry point that includes the project
/// directory name for disambiguation (e.g. `wrangler/src/index.ts`
/// instead of just `src/index.ts`).
fn entry_label(entry: &Path, root: &Path) -> String {
    let rel = entry.strip_prefix(root).unwrap_or(entry);
    root.file_name().map_or_else(
        || rel.to_string_lossy().into_owned(),
        |name| Path::new(name).join(rel).to_string_lossy().into_owned(),
    )
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
        // resolve_target tries as file path first, falls back to package lookup.
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
    fn resolve_target_falls_back_to_package_for_extension_named_package() {
        use chainsaw::graph::{ModuleGraph, PackageInfo, ModuleId};

        let tmp = tempfile::tempdir().unwrap();
        let root = tmp.path().canonicalize().unwrap();
        let entry = root.join("entry.py");
        std::fs::write(&entry, "").unwrap();

        let mut graph = ModuleGraph::new();
        graph.add_module(entry.clone(), 0, None);
        graph.package_map.insert(
            "six.py".to_string(),
            PackageInfo {
                name: "six.py".to_string(),
                entry_module: ModuleId(0),
                total_reachable_size: 100,
                total_reachable_files: 1,
            },
        );

        let loaded = loader::LoadedGraph {
            graph,
            root,
            entry,
            valid_extensions: &["py"],
            from_cache: false,
            unresolvable_dynamic_count: 0,
            unresolvable_dynamic_files: vec![],
            file_warnings: vec![],
        };
        // "six.py" looks like a path (.py extension) but no such file exists.
        // Should fall back to package lookup, not exit(1).
        let resolved = resolve_target("six.py", &loaded);
        assert_eq!(resolved.target, query::ChainTarget::Package("six.py".to_string()));
        assert!(resolved.exists);
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
