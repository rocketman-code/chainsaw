// compact_str 0.8 (oxc_span) + 0.9 (oxc_resolver) — transitive, out of our control.
#![allow(clippy::multiple_crate_versions)]

use std::io::IsTerminal;
use std::path::{Path, PathBuf};
use std::sync::Arc;
use std::time::Instant;

use clap::{Args, CommandFactory, Parser, Subcommand};
use clap_complete::Shell;

use chainsaw::{
    error::Error,
    git, loader, query, repl, report,
    session::{self, Session},
    vfs,
};

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
        /// If only one arg, this is the baseline and the working tree is "after".
        a: String,

        /// Snapshot file or git ref ("after" / "current").
        /// If omitted, the current working tree is the "after" side.
        b: Option<String>,

        /// Entry point to trace (required for git refs and working tree)
        #[arg(long)]
        entry: Option<PathBuf>,

        /// Max packages to show in diff output (-1 for all)
        #[arg(long, default_value_t = report::DEFAULT_TOP, allow_hyphen_values = true)]
        limit: i32,

        /// Suppress informational output (timing, warnings)
        #[arg(long, short)]
        quiet: bool,
    },

    /// List all third-party packages in the dependency graph
    Packages(PackagesArgs),

    /// Interactive exploration mode
    Repl {
        /// Entry point file to start from
        entry: PathBuf,
    },

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
    #[arg(long, default_value_t = report::DEFAULT_TOP, allow_hyphen_values = true)]
    top: i32,

    /// Show top N modules by exclusive weight — bytes not reachable through any other path (0 to hide, -1 for all)
    #[arg(long, default_value_t = report::DEFAULT_TOP_MODULES, allow_hyphen_values = true)]
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
    #[arg(long, default_value_t = report::DEFAULT_TOP, allow_hyphen_values = true)]
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
    #[arg(long, default_value_t = report::DEFAULT_TOP, allow_hyphen_values = true)]
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
    let value: f64 = num_str
        .parse()
        .map_err(|_| format!("invalid size: {s}\nhint: valid formats: 5MB, 500KB, 100B"))?;
    Ok((value * multiplier) as u64)
}

fn resolve_color(no_color: bool) -> bool {
    report::should_use_color(
        std::io::stdout().is_terminal(),
        no_color,
        std::env::var_os("NO_COLOR").is_some(),
        std::env::var("TERM").is_ok_and(|v| v == "dumb"),
    )
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
    let color = resolve_color(no_color);
    match command {
        Commands::Trace(args) => run_trace(args, color, sc),

        Commands::Diff {
            a,
            b,
            entry,
            limit,
            quiet,
        } => run_diff(a, b, entry, limit, quiet, color, sc),

        Commands::Packages(ref args) => run_packages(args, color, sc),

        Commands::Repl { ref entry } => repl::run(entry, no_color, sc),

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

// ---------------------------------------------------------------------------
// trace subcommand
// ---------------------------------------------------------------------------

fn run_trace(args: TraceArgs, color: bool, sc: report::StderrColor) -> Result<(), Error> {
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

    if args.top < -1 {
        return Err(Error::InvalidTopValue("--top", args.top));
    }
    if args.top_modules < -1 {
        return Err(Error::InvalidTopValue("--top-modules", args.top_modules));
    }
    if args.limit < -1 {
        return Err(Error::InvalidTopValue("--limit", args.limit));
    }

    let mut session = Session::open(&args.entry, args.no_cache)?;
    if !args.quiet {
        print_session_status(&session, start, sc);
    }

    let opts = query::TraceOptions {
        include_dynamic: args.include_dynamic,
        top_n: args.top,
        ignore: args.ignore,
    };
    let result = session.trace(&opts);
    let entry_rel = session.entry_label();

    // Save snapshot if requested (works with any mode)
    if let Some(ref save_path) = args.save {
        save_snapshot(save_path, &result, &entry_rel, args.quiet, sc)?;
    }

    // --chain
    if let Some(ref chain_arg) = args.chain {
        let resolved = session.resolve_target(chain_arg);
        if resolved.target == query::ChainTarget::Module(session.entry_id()) {
            return Err(Error::TargetIsEntryPoint("--chain".into()));
        }
        let report = session.chain_report(chain_arg, args.include_dynamic);
        if args.json {
            println!("{}", report.to_json());
        } else {
            print!("{}", report.to_terminal(color));
        }
        if report.chains.is_empty() {
            std::process::exit(1);
        }
        return Ok(());
    }

    // --cut
    if let Some(ref cut_arg) = args.cut {
        let resolved = session.resolve_target(cut_arg);
        if resolved.target == query::ChainTarget::Module(session.entry_id()) {
            return Err(Error::TargetIsEntryPoint("--cut".into()));
        }
        let report = session.cut_report(cut_arg, args.top, args.include_dynamic);
        if args.json {
            println!("{}", report.to_json());
        } else {
            print!("{}", report.to_terminal(color));
        }
        if report.chain_count == 0 {
            std::process::exit(1);
        }
        return Ok(());
    }

    // --diff-from
    if let Some(ref snapshot_path) = args.diff_from {
        let saved = load_snapshot(snapshot_path)?;
        let diff = query::diff_snapshots(&saved, &result.to_snapshot(&entry_rel));
        let report = report::DiffReport::from_diff(&diff, &saved.entry, &entry_rel, args.limit);
        if args.json {
            println!("{}", report.to_json());
        } else {
            print!("{}", report.to_terminal(color));
        }
        return Ok(());
    }

    // --diff
    if let Some(ref diff_path) = args.diff {
        return handle_trace_diff(
            &session,
            diff_path,
            &result,
            &entry_rel,
            &opts,
            args.no_cache,
            args.limit,
            color,
            sc,
        );
    }

    // Normal trace output
    let report = session.trace_report(&opts, args.top_modules);
    if args.json {
        println!("{}", report.to_json());
    } else {
        print!("{}", report.to_terminal(color));
    }

    if let Some(threshold) = args.max_weight.filter(|&t| report.static_weight_bytes > t) {
        let kind = if args.include_dynamic {
            "total"
        } else {
            "static"
        };
        return Err(Error::MaxWeightExceeded {
            kind,
            weight: report.static_weight_bytes,
            module_count: report.static_module_count,
            threshold,
        });
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

/// Handle `trace --diff <file>` by comparing two entry points.
#[allow(clippy::too_many_arguments)] // private dispatch, called from one site
fn handle_trace_diff(
    session: &Session,
    diff_path: &Path,
    result: &query::TraceResult,
    entry_rel: &str,
    opts: &query::TraceOptions,
    no_cache: bool,
    limit: i32,
    color: bool,
    sc: report::StderrColor,
) -> Result<(), Error> {
    let diff_entry = diff_path
        .canonicalize()
        .map_err(|e| Error::EntryNotFound(diff_path.to_path_buf(), e))?;
    if *session.entry() == diff_entry {
        eprintln!(
            "{} both entry points are the same file, diff will be empty",
            sc.warning("warning:")
        );
    }

    let diff_snapshot = if let Some(&diff_id) = session.graph().path_to_id.get(&diff_entry) {
        // Same graph — trace directly
        let diff_rel = session.entry_label_for(&diff_entry);
        query::trace(session.graph(), diff_id, opts).to_snapshot(&diff_rel)
    } else {
        // Different project — open a separate session
        let diff_session = Session::open(&diff_entry, no_cache)?;
        diff_session
            .trace(opts)
            .to_snapshot(&diff_session.entry_label())
    };

    let diff_output = query::diff_snapshots(&result.to_snapshot(entry_rel), &diff_snapshot);
    let report =
        report::DiffReport::from_diff(&diff_output, entry_rel, &diff_snapshot.entry, limit);
    print!("{}", report.to_terminal(color));
    Ok(())
}

fn print_session_status(session: &Session, start: Instant, sc: report::StderrColor) {
    report::print_load_status(
        session.from_cache(),
        session.graph().module_count(),
        start.elapsed().as_secs_f64() * 1000.0,
        session.file_warnings(),
        session.unresolvable_dynamic_count(),
        session.unresolvable_dynamic_files(),
        session.root(),
        sc,
    );
}

// ---------------------------------------------------------------------------
// packages subcommand
// ---------------------------------------------------------------------------

fn run_packages(args: &PackagesArgs, color: bool, sc: report::StderrColor) -> Result<(), Error> {
    if args.top < -1 {
        return Err(Error::InvalidTopValue("--top", args.top));
    }
    let start = Instant::now();
    let session = Session::open(&args.entry, args.no_cache)?;
    if !args.quiet {
        print_session_status(&session, start, sc);
    }

    let report = session.packages_report(args.top);
    if args.json {
        println!("{}", report.to_json());
    } else {
        print!("{}", report.to_terminal(color));
    }
    Ok(())
}

// ---------------------------------------------------------------------------
// Snapshot helpers
// ---------------------------------------------------------------------------

fn save_snapshot(
    path: &Path,
    result: &query::TraceResult,
    entry_rel: &str,
    quiet: bool,
    sc: report::StderrColor,
) -> Result<(), Error> {
    let snapshot = result.to_snapshot(entry_rel);
    let data = serde_json::to_string_pretty(&snapshot).unwrap();
    std::fs::write(path, &data).map_err(|e| Error::SnapshotWrite(path.to_path_buf(), e))?;
    if !quiet {
        eprintln!("{} to {}", sc.status("Snapshot saved"), path.display());
    }
    Ok(())
}

fn load_snapshot(path: &Path) -> Result<query::TraceSnapshot, Error> {
    let data =
        std::fs::read_to_string(path).map_err(|e| Error::SnapshotRead(path.to_path_buf(), e))?;
    serde_json::from_str(&data).map_err(|e| Error::SnapshotParse(path.to_path_buf(), e))
}

// ---------------------------------------------------------------------------
// diff subcommand (git refs / snapshots — uses LoadedGraph directly)
// ---------------------------------------------------------------------------

fn print_build_status(loaded: &loader::LoadedGraph, start: Instant, sc: report::StderrColor) {
    report::print_load_status(
        loaded.from_cache,
        loaded.graph.module_count(),
        start.elapsed().as_secs_f64() * 1000.0,
        &loaded.file_warnings,
        loaded.unresolvable_dynamic_count,
        &loaded.unresolvable_dynamic_files,
        &loaded.root,
        sc,
    );
}

#[allow(clippy::too_many_arguments)]
fn run_diff(
    a: String,
    b: Option<String>,
    entry: Option<PathBuf>,
    limit: i32,
    quiet: bool,
    color: bool,
    sc: report::StderrColor,
) -> Result<(), Error> {
    if limit < -1 {
        return Err(Error::InvalidTopValue("--limit", limit));
    }
    let start = Instant::now();

    // Determine repo root (needed for git ref detection and in-process tree reading).
    // Use CWD — if we're not in a git repo, classify_diff_arg will fail
    // gracefully for git refs and succeed for snapshot files.
    let cwd = std::env::current_dir()
        .map_err(|e| Error::GitError(format!("cannot determine working directory: {e}")))?;
    let repo_root = git::repo_root(&cwd).ok();

    // Classify arguments.
    let root = repo_root.as_deref().unwrap_or(&cwd);
    let arg_a = git::classify_diff_arg(&a, root)?;
    let arg_b = b.map(|s| git::classify_diff_arg(&s, root)).transpose()?;

    let has_ref =
        matches!(arg_a, git::DiffArg::GitRef(_)) || matches!(&arg_b, Some(git::DiffArg::GitRef(_)));

    // --entry is required when any side is a git ref or when the working
    // tree is an implicit side (one-arg form).
    if entry.is_none() && (has_ref || arg_b.is_none()) {
        return Err(Error::EntryRequired);
    }

    // Build snapshots from each side.
    let (snap_a, label_a) = build_diff_side(&arg_a, entry.as_deref(), root, quiet, sc)?;
    let (snap_b, label_b) = match arg_b {
        Some(ref arg) => build_diff_side(arg, entry.as_deref(), root, quiet, sc)?,
        None => {
            // One arg: the arg is "before" (baseline), working tree is "after" (current).
            // Matches `git diff <ref>` semantics.
            let entry_path = entry.as_ref().ok_or(Error::EntryRequired)?;
            let wt_snap = build_snapshot_from_working_tree(entry_path, quiet, sc)?;
            let wt_label = wt_snap.entry.clone();
            return finish_diff(
                &snap_a, &label_a, &wt_snap, &wt_label, limit, color, start, quiet, sc,
            );
        }
    };

    finish_diff(
        &snap_a, &label_a, &snap_b, &label_b, limit, color, start, quiet, sc,
    )
}

#[allow(clippy::too_many_arguments)] // private dispatch, called from one site
fn finish_diff(
    snap_a: &query::TraceSnapshot,
    label_a: &str,
    snap_b: &query::TraceSnapshot,
    label_b: &str,
    limit: i32,
    color: bool,
    start: Instant,
    quiet: bool,
    sc: report::StderrColor,
) -> Result<(), Error> {
    let diff_output = query::diff_snapshots(snap_a, snap_b);
    let report = report::DiffReport::from_diff(&diff_output, label_a, label_b, limit);
    print!("{}", report.to_terminal(color));
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
fn build_diff_side(
    arg: &git::DiffArg,
    entry: Option<&Path>,
    repo_root: &Path,
    quiet: bool,
    sc: report::StderrColor,
) -> Result<(query::TraceSnapshot, String), Error> {
    match arg {
        git::DiffArg::Snapshot(path) => {
            let snap = load_snapshot(path)?;
            let label = snap.entry.clone();
            Ok((snap, label))
        }
        git::DiffArg::GitRef(git_ref) => {
            let entry = entry.ok_or(Error::EntryRequired)?;
            let snap = build_snapshot_from_ref(repo_root, git_ref, entry, quiet, sc)?;
            let label = snap.entry.clone();
            Ok((snap, label))
        }
    }
}

/// Read the tree at `git_ref` in-process via gix and build a snapshot.
fn build_snapshot_from_ref(
    repo_root: &Path,
    git_ref: &str,
    entry: &Path,
    quiet: bool,
    sc: report::StderrColor,
) -> Result<query::TraceSnapshot, Error> {
    let start = Instant::now();
    let git_vfs = Arc::new(
        vfs::GitTreeVfs::new(repo_root, git_ref, repo_root)
            .map_err(|e| Error::GitError(e.to_string()))?,
    );
    let entry_in_vfs = repo_root.join(entry);
    let (loaded, _cache_write) = loader::load_graph_with_vfs(&entry_in_vfs, true, git_vfs)?; // no-cache
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
    Ok(result.to_snapshot(&label))
}

/// Build a snapshot from the current working tree.
///
/// Uses `no_cache=true` to avoid polluting the disk cache — the "before"
/// side of a diff may have written a reduced (git-tree-only) graph, and
/// the working-tree snapshot must not read or overwrite that entry.
fn build_snapshot_from_working_tree(
    entry: &Path,
    quiet: bool,
    sc: report::StderrColor,
) -> Result<query::TraceSnapshot, Error> {
    let start = Instant::now();
    let (loaded, _cache_write) = loader::load_graph(entry, true)?;
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
    let label = session::entry_label(&loaded.entry, &loaded.root);
    Ok(result.to_snapshot(&label))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_size_units() {
        assert_eq!(parse_size("5MB").unwrap(), 5_000_000);
        assert_eq!(parse_size("500KB").unwrap(), 500_000);
        assert_eq!(parse_size("100B").unwrap(), 100);
        assert_eq!(parse_size("1234").unwrap(), 1234);
        assert_eq!(parse_size("1.5MB").unwrap(), 1_500_000);
        assert!(parse_size("abc").is_err());
    }

    #[test]
    fn parse_size_error_includes_hint() {
        let err = parse_size("abc").unwrap_err();
        assert!(err.contains("hint:"), "expected hint in error: {err}");
        assert!(
            err.contains("MB"),
            "expected format examples in error: {err}"
        );
    }

    #[test]
    fn invalid_top_value_includes_flag_name() {
        let err = Error::InvalidTopValue("--top", -5);
        let msg = err.to_string();
        assert!(msg.contains("--top"), "expected flag name in error: {msg}");
        assert!(msg.contains("-5"), "expected value in error: {msg}");
    }

    #[test]
    fn invalid_limit_value_includes_flag_name() {
        let err = Error::InvalidTopValue("--limit", -3);
        let msg = err.to_string();
        assert!(
            msg.contains("--limit"),
            "expected flag name in error: {msg}"
        );
    }
}
