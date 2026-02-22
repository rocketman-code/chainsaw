//! REPL command parser and interactive loop.
//!
//! The command parser is deliberately simple: one word command, optional
//! argument string. No shell-style quoting or flags — the REPL is an
//! interactive explorer, not a second CLI.

use std::io::IsTerminal;
use std::path::{Path, PathBuf};

use rustyline::Helper;
use rustyline::completion::{Completer, Pair};
use rustyline::highlight::Highlighter;
use rustyline::hint::Hinter;
use rustyline::validate::Validator;

use crate::error::Error;
use crate::graph::EdgeKind;
use crate::query::{self, ChainTarget};
use crate::report::{self, StderrColor};
use crate::session::Session;

/// A parsed REPL command.
pub enum Command {
    /// Trace transitive weight (optionally from a different file).
    Trace(Option<String>),
    /// Switch the session entry point.
    Entry(String),
    /// Show import chains to a target.
    Chain(String),
    /// Show optimal cut points for a target.
    Cut(String),
    /// Diff against another entry point.
    Diff(String),
    /// List all third-party packages.
    Packages,
    /// List direct imports of a file.
    Imports(String),
    /// List files that import a given file.
    Importers(String),
    /// Show package info by name.
    Info(String),
    /// Print help text.
    Help,
    /// Exit the REPL.
    Quit,
    /// Unrecognised input or missing argument.
    Unknown(String),
}

impl Command {
    /// Parse a single line of user input into a command.
    pub fn parse(line: &str) -> Self {
        /// Extract a non-empty argument or return an error message.
        fn require(arg: Option<&str>, msg: &str) -> Result<String, String> {
            match arg {
                Some(a) if !a.is_empty() => Ok(a.to_string()),
                _ => Err(msg.to_string()),
            }
        }

        let line = line.trim();
        if line.is_empty() {
            return Self::Help;
        }
        let (cmd, arg) = line
            .split_once(' ')
            .map_or((line, None), |(c, a)| (c, Some(a.trim())));

        match cmd {
            "trace" => Self::Trace(arg.map(String::from)),
            "entry" => match require(arg, "entry requires a file argument") {
                Ok(a) => Self::Entry(a),
                Err(e) => Self::Unknown(e),
            },
            "chain" => match require(arg, "chain requires a target argument") {
                Ok(a) => Self::Chain(a),
                Err(e) => Self::Unknown(e),
            },
            "cut" => match require(arg, "cut requires a target argument") {
                Ok(a) => Self::Cut(a),
                Err(e) => Self::Unknown(e),
            },
            "diff" => match require(arg, "diff requires a file argument") {
                Ok(a) => Self::Diff(a),
                Err(e) => Self::Unknown(e),
            },
            "imports" => match require(arg, "imports requires a file argument") {
                Ok(a) => Self::Imports(a),
                Err(e) => Self::Unknown(e),
            },
            "importers" => match require(arg, "importers requires a file argument") {
                Ok(a) => Self::Importers(a),
                Err(e) => Self::Unknown(e),
            },
            "info" => match require(arg, "info requires a package name") {
                Ok(a) => Self::Info(a),
                Err(e) => Self::Unknown(e),
            },
            "packages" => Self::Packages,
            "help" | "?" => Self::Help,
            "quit" | "exit" => Self::Quit,
            _ => Self::Unknown(format!("unknown command: {cmd}")),
        }
    }
}

/// All command names, for tab completion.
pub const COMMAND_NAMES: &[&str] = &[
    "trace",
    "entry",
    "chain",
    "cut",
    "diff",
    "packages",
    "imports",
    "importers",
    "info",
    "help",
    "quit",
    "exit",
];

// ---------------------------------------------------------------------------
// Tab completion
// ---------------------------------------------------------------------------

const MAX_COMPLETIONS: usize = 20;

/// Binary-search a sorted slice for entries starting with `prefix`, returning
/// up to `limit` matches.
fn sorted_prefix_matches<'a>(sorted: &'a [String], prefix: &str, limit: usize) -> Vec<&'a str> {
    if limit == 0 {
        return Vec::new();
    }
    let start = sorted.partition_point(|s| s.as_str() < prefix);
    sorted[start..]
        .iter()
        .take_while(|s| s.starts_with(prefix))
        .take(limit)
        .map(String::as_str)
        .collect()
}

struct ChainsawHelper {
    file_paths: Vec<String>,
    package_names: Vec<String>,
}

impl ChainsawHelper {
    fn new() -> Self {
        Self {
            file_paths: Vec::new(),
            package_names: Vec::new(),
        }
    }

    fn update_from_session(&mut self, session: &Session) {
        self.file_paths = session
            .graph()
            .modules
            .iter()
            .map(|m| report::relative_path(&m.path, session.root()))
            .collect();
        self.file_paths.sort_unstable();
        self.package_names = session.graph().package_map.keys().cloned().collect();
        self.package_names.sort_unstable();
    }
}

impl Completer for ChainsawHelper {
    type Candidate = Pair;

    fn complete(
        &self,
        line: &str,
        pos: usize,
        _ctx: &rustyline::Context<'_>,
    ) -> rustyline::Result<(usize, Vec<Pair>)> {
        let line = &line[..pos];

        // Complete command names at start of line.
        if !line.contains(' ') {
            let matches: Vec<Pair> = COMMAND_NAMES
                .iter()
                .filter(|c| c.starts_with(line))
                .map(|&c| Pair {
                    display: c.to_string(),
                    replacement: c.to_string(),
                })
                .collect();
            return Ok((0, matches));
        }

        // Complete arguments based on command.
        let (cmd, partial) = line.split_once(' ').unwrap_or((line, ""));
        let partial = partial.trim_start();
        let start = pos - partial.len();

        let matches: Vec<Pair> = match cmd {
            "chain" | "cut" => {
                let pkgs = sorted_prefix_matches(&self.package_names, partial, MAX_COMPLETIONS);
                let remaining = MAX_COMPLETIONS - pkgs.len();
                let files = sorted_prefix_matches(&self.file_paths, partial, remaining);
                pkgs.into_iter()
                    .chain(files)
                    .map(|c| Pair {
                        display: c.to_string(),
                        replacement: c.to_string(),
                    })
                    .collect()
            }
            "info" => sorted_prefix_matches(&self.package_names, partial, MAX_COMPLETIONS)
                .into_iter()
                .map(|c| Pair {
                    display: c.to_string(),
                    replacement: c.to_string(),
                })
                .collect(),
            "trace" | "entry" | "imports" | "importers" | "diff" => {
                sorted_prefix_matches(&self.file_paths, partial, MAX_COMPLETIONS)
                    .into_iter()
                    .map(|c| Pair {
                        display: c.to_string(),
                        replacement: c.to_string(),
                    })
                    .collect()
            }
            _ => return Ok((start, vec![])),
        };

        Ok((start, matches))
    }
}

impl Hinter for ChainsawHelper {
    type Hint = String;
}
impl Highlighter for ChainsawHelper {}
impl Validator for ChainsawHelper {}
impl Helper for ChainsawHelper {}

// ---------------------------------------------------------------------------
// REPL loop
// ---------------------------------------------------------------------------

/// Run the interactive REPL loop.
pub fn run(entry: &Path, no_color: bool, sc: StderrColor) -> Result<(), Error> {
    let start = std::time::Instant::now();
    let mut session = Session::open(entry, false)?;

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
    eprintln!("Type 'help' for commands, 'quit' to exit.\n");

    session.watch();

    let color = report::should_use_color(
        std::io::stdout().is_terminal(),
        no_color,
        std::env::var_os("NO_COLOR").is_some(),
        std::env::var("TERM").is_ok_and(|v| v == "dumb"),
    );

    let mut helper = ChainsawHelper::new();
    helper.update_from_session(&session);

    let mut rl =
        rustyline::Editor::new().map_err(|e| Error::Readline(format!("init failed: {e}")))?;
    rl.set_helper(Some(helper));

    let history_path = history_file();
    if let Some(ref path) = history_path {
        let _ = rl.load_history(path);
    }

    let prompt = format!("{}> ", sc.status("chainsaw"));

    loop {
        // Check for file changes before each command.
        match session.refresh() {
            Ok(true) => {
                eprintln!(
                    "{} graph refreshed ({} modules)",
                    sc.status("Reloaded:"),
                    session.graph().module_count()
                );
                if let Some(h) = rl.helper_mut() {
                    h.update_from_session(&session);
                }
            }
            Ok(false) => {}
            Err(e) => eprintln!("{} refresh failed: {e}", sc.warning("warning:")),
        }

        let line = match rl.readline(&prompt) {
            Ok(line) => line,
            Err(rustyline::error::ReadlineError::Interrupted) => continue,
            Err(rustyline::error::ReadlineError::Eof) => break,
            Err(e) => {
                eprintln!("{} {e}", sc.error("error:"));
                break;
            }
        };

        let trimmed = line.trim();
        if trimmed.is_empty() {
            continue;
        }
        rl.add_history_entry(trimmed).ok();

        match Command::parse(trimmed) {
            Command::Trace(file) => dispatch_trace(&mut session, file.as_deref(), color, sc),
            Command::Entry(path) => dispatch_entry(&mut session, &path, sc),
            Command::Chain(target) => dispatch_chain(&session, &target, color, sc),
            Command::Cut(target) => dispatch_cut(&mut session, &target, color, sc),
            Command::Diff(path) => dispatch_diff(&mut session, &path, color, sc),
            Command::Packages => dispatch_packages(&session, color),
            Command::Imports(path) => dispatch_imports(&session, &path, sc),
            Command::Importers(path) => dispatch_importers(&session, &path, sc),
            Command::Info(name) => dispatch_info(&session, &name, sc),
            Command::Help => print_help(),
            Command::Quit => break,
            Command::Unknown(msg) => eprintln!("{} {msg}", sc.error("error:")),
        }
    }

    if let Some(ref path) = history_path {
        let _ = rl.save_history(path);
    }
    Ok(())
}

fn history_file() -> Option<PathBuf> {
    let dir = std::env::var_os("XDG_DATA_HOME")
        .map(PathBuf::from)
        .or_else(|| std::env::var_os("HOME").map(|h| PathBuf::from(h).join(".local/share")))?;
    let dir = dir.join("chainsaw");
    std::fs::create_dir_all(&dir).ok()?;
    Some(dir.join("history"))
}

// ---------------------------------------------------------------------------
// Command dispatch
// ---------------------------------------------------------------------------

fn dispatch_trace(session: &mut Session, file: Option<&str>, color: bool, sc: StderrColor) {
    let opts = query::TraceOptions::default();
    let report = if let Some(f) = file {
        match session.trace_from_report(Path::new(f), &opts, report::DEFAULT_TOP_MODULES) {
            Ok((r, _)) => r,
            Err(e) => {
                eprintln!("{} {e}", sc.error("error:"));
                return;
            }
        }
    } else {
        session.trace_report(&opts, report::DEFAULT_TOP_MODULES)
    };
    print!("{}", report.to_terminal(color));
}

fn dispatch_entry(session: &mut Session, path: &str, sc: StderrColor) {
    if let Err(e) = session.set_entry(Path::new(path)) {
        eprintln!("{} {e}", sc.error("error:"));
        return;
    }
    let rel = report::relative_path(session.entry(), session.root());
    eprintln!("{} entry point is now {rel}", sc.status("Switched:"));
}

fn dispatch_chain(session: &Session, target: &str, color: bool, sc: StderrColor) {
    let resolved = session.resolve_target(target);
    if resolved.target == ChainTarget::Module(session.entry_id()) {
        eprintln!("{} target is the entry point itself", sc.error("error:"));
        return;
    }
    let report = session.chain_report(target, false);
    print!("{}", report.to_terminal(color));
}

fn dispatch_cut(session: &mut Session, target: &str, color: bool, sc: StderrColor) {
    let resolved = session.resolve_target(target);
    if resolved.target == ChainTarget::Module(session.entry_id()) {
        eprintln!("{} target is the entry point itself", sc.error("error:"));
        return;
    }
    let report = session.cut_report(target, report::DEFAULT_TOP, false);
    print!("{}", report.to_terminal(color));
}

fn dispatch_diff(session: &mut Session, path: &str, color: bool, sc: StderrColor) {
    let opts = query::TraceOptions::default();
    match session.diff_report(Path::new(path), &opts, report::DEFAULT_TOP) {
        Ok(report) => print!("{}", report.to_terminal(color)),
        Err(e) => eprintln!("{} {e}", sc.error("error:")),
    }
}

fn dispatch_packages(session: &Session, color: bool) {
    let report = session.packages_report(report::DEFAULT_TOP);
    print!("{}", report.to_terminal(color));
}

fn dispatch_imports(session: &Session, path: &str, sc: StderrColor) {
    match session.imports(Path::new(path)) {
        Ok(imports) => {
            if imports.is_empty() {
                println!("  (no imports)");
                return;
            }
            for (p, kind) in &imports {
                let rel = report::relative_path(p, session.root());
                let suffix = match kind {
                    EdgeKind::Static => "",
                    EdgeKind::Dynamic => " (dynamic)",
                    EdgeKind::TypeOnly => " (type-only)",
                };
                println!("  {rel}{suffix}");
            }
        }
        Err(e) => eprintln!("{} {e}", sc.error("error:")),
    }
}

fn dispatch_importers(session: &Session, path: &str, sc: StderrColor) {
    match session.importers(Path::new(path)) {
        Ok(importers) => {
            if importers.is_empty() {
                println!("  (no importers)");
                return;
            }
            for (p, kind) in &importers {
                let rel = report::relative_path(p, session.root());
                let suffix = match kind {
                    EdgeKind::Static => "",
                    EdgeKind::Dynamic => " (dynamic)",
                    EdgeKind::TypeOnly => " (type-only)",
                };
                println!("  {rel}{suffix}");
            }
        }
        Err(e) => eprintln!("{} {e}", sc.error("error:")),
    }
}

fn dispatch_info(session: &Session, name: &str, sc: StderrColor) {
    match session.info(name) {
        Some(info) => {
            println!(
                "  {} ({} files, {})",
                info.name,
                info.total_reachable_files,
                report::format_size(info.total_reachable_size)
            );
        }
        None => eprintln!("{} package '{name}' not found", sc.error("error:")),
    }
}

fn print_help() {
    println!("Commands:");
    println!("  trace [file]       Trace from entry point (or specified file)");
    println!("  entry <file>       Switch the default entry point");
    println!("  chain <target>     Show import chains to a package or file");
    println!("  cut <target>       Show where to cut to sever chains");
    println!("  diff <file>        Compare weight against another entry");
    println!("  packages           List third-party packages");
    println!("  imports <file>     Show what a file imports");
    println!("  importers <file>   Show what imports a file");
    println!("  info <package>     Show package details");
    println!("  help               Show this help");
    println!("  quit               Exit");
}

#[cfg(test)]
mod tests {
    use super::*;

    // -----------------------------------------------------------------------
    // sorted_prefix_matches
    // -----------------------------------------------------------------------

    #[test]
    fn prefix_empty_list() {
        let empty: Vec<String> = vec![];
        assert!(sorted_prefix_matches(&empty, "foo", 10).is_empty());
    }

    #[test]
    fn prefix_no_matches() {
        let list = vec!["alpha".into(), "beta".into(), "gamma".into()];
        assert!(sorted_prefix_matches(&list, "delta", 10).is_empty());
    }

    #[test]
    fn prefix_exact_match() {
        let list = vec!["alpha".into(), "beta".into(), "gamma".into()];
        assert_eq!(sorted_prefix_matches(&list, "beta", 10), vec!["beta"]);
    }

    #[test]
    fn prefix_multiple_matches() {
        let list = vec![
            "src/a.ts".into(),
            "src/b.ts".into(),
            "src/c.ts".into(),
            "test/d.ts".into(),
        ];
        assert_eq!(
            sorted_prefix_matches(&list, "src/", 10),
            vec!["src/a.ts", "src/b.ts", "src/c.ts"]
        );
    }

    #[test]
    fn prefix_respects_limit() {
        let list = vec![
            "src/a.ts".into(),
            "src/b.ts".into(),
            "src/c.ts".into(),
            "src/d.ts".into(),
        ];
        assert_eq!(
            sorted_prefix_matches(&list, "src/", 2),
            vec!["src/a.ts", "src/b.ts"]
        );
    }

    #[test]
    fn prefix_empty_prefix_matches_all_up_to_limit() {
        let list = vec!["a".into(), "b".into(), "c".into()];
        assert_eq!(sorted_prefix_matches(&list, "", 2), vec!["a", "b"]);
    }

    #[test]
    fn prefix_zero_limit_returns_empty() {
        let list = vec!["a".into(), "b".into()];
        assert!(sorted_prefix_matches(&list, "", 0).is_empty());
    }

    // -----------------------------------------------------------------------
    // ChainsawHelper completion
    // -----------------------------------------------------------------------

    fn helper_with(files: Vec<&str>, packages: Vec<&str>) -> ChainsawHelper {
        let mut file_paths: Vec<String> = files.into_iter().map(String::from).collect();
        let mut package_names: Vec<String> = packages.into_iter().map(String::from).collect();
        file_paths.sort_unstable();
        package_names.sort_unstable();
        ChainsawHelper {
            file_paths,
            package_names,
        }
    }

    fn complete_line(helper: &ChainsawHelper, line: &str) -> Vec<String> {
        let history = rustyline::history::DefaultHistory::new();
        let ctx = rustyline::Context::new(&history);
        let (_, pairs) = helper.complete(line, line.len(), &ctx).unwrap();
        pairs.into_iter().map(|p| p.replacement).collect()
    }

    #[test]
    fn complete_command_names() {
        let h = helper_with(vec![], vec![]);
        let results = complete_line(&h, "tr");
        assert_eq!(results, vec!["trace"]);
    }

    #[test]
    fn complete_trace_file_paths() {
        let h = helper_with(vec!["src/a.ts", "src/b.ts", "lib/c.ts"], vec![]);
        let results = complete_line(&h, "trace src/");
        assert_eq!(results, vec!["src/a.ts", "src/b.ts"]);
    }

    #[test]
    fn complete_chain_packages_then_files() {
        let h = helper_with(vec!["zod-utils.ts"], vec!["zod", "zustand"]);
        let results = complete_line(&h, "chain z");
        // packages first, then files
        assert_eq!(results, vec!["zod", "zustand", "zod-utils.ts"]);
    }

    #[test]
    fn complete_info_packages_only() {
        let h = helper_with(vec!["src/react.ts"], vec!["react", "react-dom"]);
        let results = complete_line(&h, "info react");
        assert_eq!(results, vec!["react", "react-dom"]);
    }

    #[test]
    fn complete_no_matches() {
        let h = helper_with(vec!["src/a.ts"], vec!["zod"]);
        let results = complete_line(&h, "trace zzz");
        assert!(results.is_empty());
    }

    #[test]
    fn complete_unknown_command_returns_empty() {
        let h = helper_with(vec!["src/a.ts"], vec!["zod"]);
        let results = complete_line(&h, "bogus src/");
        assert!(results.is_empty());
    }

    #[test]
    fn complete_max_completions_truncates() {
        let files: Vec<&str> = (0..30)
            .map(|i| {
                // Leak is fine in tests — avoids lifetime gymnastics.
                Box::leak(format!("src/{i:02}.ts").into_boxed_str()) as &str
            })
            .collect();
        let h = helper_with(files, vec![]);
        let results = complete_line(&h, "trace src/");
        assert_eq!(results.len(), MAX_COMPLETIONS);
    }

    // -----------------------------------------------------------------------
    // Command parsing
    // -----------------------------------------------------------------------

    #[test]
    fn parse_trace_no_arg() {
        assert!(matches!(Command::parse("trace"), Command::Trace(None)));
    }

    #[test]
    fn parse_trace_with_file() {
        assert!(
            matches!(Command::parse("trace src/index.ts"), Command::Trace(Some(ref f)) if f == "src/index.ts")
        );
    }

    #[test]
    fn parse_chain() {
        assert!(matches!(Command::parse("chain zod"), Command::Chain(ref t) if t == "zod"));
    }

    #[test]
    fn parse_entry() {
        assert!(
            matches!(Command::parse("entry src/other.ts"), Command::Entry(ref f) if f == "src/other.ts")
        );
    }

    #[test]
    fn parse_packages() {
        assert!(matches!(Command::parse("packages"), Command::Packages));
    }

    #[test]
    fn parse_imports() {
        assert!(
            matches!(Command::parse("imports src/foo.ts"), Command::Imports(ref f) if f == "src/foo.ts")
        );
    }

    #[test]
    fn parse_importers() {
        assert!(
            matches!(Command::parse("importers lib/bar.py"), Command::Importers(ref f) if f == "lib/bar.py")
        );
    }

    #[test]
    fn parse_info() {
        assert!(matches!(Command::parse("info zod"), Command::Info(ref p) if p == "zod"));
    }

    #[test]
    fn parse_empty_is_help() {
        assert!(matches!(Command::parse(""), Command::Help));
    }

    #[test]
    fn parse_question_mark_is_help() {
        assert!(matches!(Command::parse("?"), Command::Help));
    }

    #[test]
    fn parse_quit() {
        assert!(matches!(Command::parse("quit"), Command::Quit));
        assert!(matches!(Command::parse("exit"), Command::Quit));
    }

    #[test]
    fn parse_unknown() {
        assert!(matches!(Command::parse("blah"), Command::Unknown(_)));
    }

    #[test]
    fn parse_missing_arg() {
        assert!(matches!(Command::parse("chain"), Command::Unknown(_)));
        assert!(matches!(Command::parse("entry"), Command::Unknown(_)));
        assert!(matches!(Command::parse("cut"), Command::Unknown(_)));
        assert!(matches!(Command::parse("diff"), Command::Unknown(_)));
        assert!(matches!(Command::parse("imports"), Command::Unknown(_)));
        assert!(matches!(Command::parse("importers"), Command::Unknown(_)));
        assert!(matches!(Command::parse("info"), Command::Unknown(_)));
    }

    #[test]
    fn parse_preserves_arg_with_spaces() {
        assert!(
            matches!(Command::parse("chain @scope/pkg"), Command::Chain(ref t) if t == "@scope/pkg")
        );
    }

    #[test]
    fn parse_trims_whitespace() {
        assert!(matches!(Command::parse("  quit  "), Command::Quit));
    }
}
