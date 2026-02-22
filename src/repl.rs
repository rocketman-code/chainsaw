//! REPL command parser and interactive loop.
//!
//! Commands accept optional inline flags (`--include-dynamic`, `--top N`, etc.)
//! that override session-level settings for a single invocation. Use
//! `set`/`unset`/`show` to manage persistent session settings.

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

/// Session-level settings that persist across REPL commands.
#[derive(Debug)]
#[non_exhaustive]
pub struct ReplSettings {
    pub include_dynamic: bool,
    pub top: i32,
    pub top_modules: i32,
    pub ignore: Vec<String>,
}

impl Default for ReplSettings {
    fn default() -> Self {
        Self {
            include_dynamic: false,
            top: report::DEFAULT_TOP,
            top_modules: report::DEFAULT_TOP_MODULES,
            ignore: Vec::new(),
        }
    }
}

/// Per-command option overrides. `None` means "use session default".
#[non_exhaustive]
#[derive(Default, Debug)]
pub struct CommandOptions {
    pub include_dynamic: Option<bool>,
    pub top: Option<i32>,
    pub top_modules: Option<i32>,
    pub ignore: Option<Vec<String>>,
    pub json: bool,
}

impl CommandOptions {
    /// Resolve per-command overrides against session settings.
    /// Returns `(TraceOptions, top_modules)`.
    pub fn resolve(&self, settings: &ReplSettings) -> (query::TraceOptions, i32) {
        let opts = query::TraceOptions {
            include_dynamic: self.include_dynamic.unwrap_or(settings.include_dynamic),
            top_n: self.top.unwrap_or(settings.top),
            ignore: self
                .ignore
                .clone()
                .unwrap_or_else(|| settings.ignore.clone()),
        };
        let top_modules = self.top_modules.unwrap_or(settings.top_modules);
        (opts, top_modules)
    }
}

/// Extract known `--flag` tokens from an argument list.
///
/// Returns `Ok((CommandOptions, positional_arg))` or `Err(message)` if an
/// unknown `--flag` is encountered. The positional argument is the first
/// non-flag token that isn't consumed as a flag value. `--ignore` consumes
/// all subsequent non-flag tokens until the next `--` flag or end of input,
/// so it must appear after the positional arg or be the last flag.
fn parse_flags(tokens: &[&str]) -> Result<(CommandOptions, String), String> {
    let mut opts = CommandOptions::default();
    let mut positional = Vec::new();
    let mut i = 0;
    while i < tokens.len() {
        match tokens[i] {
            "--json" => opts.json = true,
            "--include-dynamic" => opts.include_dynamic = Some(true),
            "--no-include-dynamic" => opts.include_dynamic = Some(false),
            "--top" => {
                if let Some(val) = tokens.get(i + 1).and_then(|v| v.parse().ok()) {
                    i += 1;
                    if val >= -1 {
                        opts.top = Some(val);
                    }
                }
            }
            "--top-modules" => {
                if let Some(val) = tokens.get(i + 1).and_then(|v| v.parse().ok()) {
                    i += 1;
                    if val >= -1 {
                        opts.top_modules = Some(val);
                    }
                }
            }
            "--ignore" => {
                let mut pkgs = Vec::new();
                i += 1;
                while i < tokens.len() && !tokens[i].starts_with("--") {
                    pkgs.push(tokens[i].to_string());
                    i += 1;
                }
                if !pkgs.is_empty() {
                    opts.ignore = Some(pkgs);
                }
                continue; // i already advanced past consumed tokens
            }
            other if other.starts_with("--") => {
                return Err(format!(
                    "unknown flag '{other}' (try: --json, --include-dynamic, --top, --top-modules, --ignore)"
                ));
            }
            other => positional.push(other),
        }
        i += 1;
    }
    Ok((opts, positional.join(" ")))
}

/// A parsed REPL command.
#[derive(Debug)]
pub enum Command {
    /// Trace transitive weight (optionally from a different file).
    Trace(Option<String>, CommandOptions),
    /// Switch the session entry point.
    Entry(String),
    /// Show import chains to a target.
    Chain(String, CommandOptions),
    /// Show optimal cut points for a target.
    Cut(String, CommandOptions),
    /// Diff against another entry point.
    Diff(String, CommandOptions),
    /// List all third-party packages.
    Packages(CommandOptions),
    /// List direct imports of a file.
    Imports(String, CommandOptions),
    /// List files that import a given file.
    Importers(String, CommandOptions),
    /// Show package info by name.
    Info(String),
    /// Set a session option.
    Set(String),
    /// Reset a session option to its default.
    Unset(String),
    /// Display current session settings.
    Show,
    /// Print help text.
    Help,
    /// Exit the REPL.
    Quit,
    /// Unrecognised input or missing argument.
    Unknown(String),
}

impl Command {
    /// Parse a single line of user input into a command.
    #[allow(clippy::too_many_lines)]
    pub fn parse(line: &str) -> Self {
        /// Extract a non-empty positional or return an error message.
        fn require_positional(positional: &str, msg: &str) -> Result<String, String> {
            if positional.is_empty() {
                Err(msg.to_string())
            } else {
                Ok(positional.to_string())
            }
        }

        /// Require a non-empty raw argument string.
        fn require_arg(arg: Option<&str>, msg: &str) -> Result<String, String> {
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

        // Tokenize the argument string for commands that accept flags.
        let tokens: Vec<&str> = arg
            .map(|a| a.split_whitespace().collect())
            .unwrap_or_default();

        match cmd {
            "trace" => match parse_flags(&tokens) {
                Ok((opts, positional)) => {
                    let file = if positional.is_empty() {
                        None
                    } else {
                        Some(positional)
                    };
                    Self::Trace(file, opts)
                }
                Err(e) => Self::Unknown(e),
            },
            "entry" => match require_arg(arg, "entry requires a file argument") {
                Ok(a) => Self::Entry(a),
                Err(e) => Self::Unknown(e),
            },
            "chain" => match parse_flags(&tokens) {
                Ok((opts, positional)) => {
                    match require_positional(&positional, "chain requires a target argument") {
                        Ok(a) => Self::Chain(a, opts),
                        Err(e) => Self::Unknown(e),
                    }
                }
                Err(e) => Self::Unknown(e),
            },
            "cut" => match parse_flags(&tokens) {
                Ok((opts, positional)) => {
                    match require_positional(&positional, "cut requires a target argument") {
                        Ok(a) => Self::Cut(a, opts),
                        Err(e) => Self::Unknown(e),
                    }
                }
                Err(e) => Self::Unknown(e),
            },
            "diff" => match parse_flags(&tokens) {
                Ok((opts, positional)) => {
                    match require_positional(&positional, "diff requires a file argument") {
                        Ok(a) => Self::Diff(a, opts),
                        Err(e) => Self::Unknown(e),
                    }
                }
                Err(e) => Self::Unknown(e),
            },
            "packages" => match parse_flags(&tokens) {
                Ok((opts, _)) => Self::Packages(opts),
                Err(e) => Self::Unknown(e),
            },
            "imports" => match parse_flags(&tokens) {
                Ok((opts, positional)) => {
                    match require_positional(&positional, "imports requires a file argument") {
                        Ok(a) => Self::Imports(a, opts),
                        Err(e) => Self::Unknown(e),
                    }
                }
                Err(e) => Self::Unknown(e),
            },
            "importers" => match parse_flags(&tokens) {
                Ok((opts, positional)) => {
                    match require_positional(&positional, "importers requires a file argument") {
                        Ok(a) => Self::Importers(a, opts),
                        Err(e) => Self::Unknown(e),
                    }
                }
                Err(e) => Self::Unknown(e),
            },
            "info" => match require_arg(arg, "info requires a package name") {
                Ok(a) => Self::Info(a),
                Err(e) => Self::Unknown(e),
            },
            "set" => match require_arg(arg, "set requires an option name") {
                Ok(a) => Self::Set(a),
                Err(e) => Self::Unknown(e),
            },
            "unset" => match require_arg(arg, "unset requires an option name") {
                Ok(a) => Self::Unset(a),
                Err(e) => Self::Unknown(e),
            },
            "show" => Self::Show,
            "help" | "?" => Self::Help,
            "quit" | "exit" => Self::Quit,
            _ => Self::Unknown(format!("unknown command: {cmd}")),
        }
    }
}

/// All command names, for tab completion.
pub const COMMAND_NAMES: &[&str] = &[
    "chain",
    "cut",
    "diff",
    "entry",
    "exit",
    "help",
    "importers",
    "imports",
    "info",
    "packages",
    "quit",
    "set",
    "show",
    "trace",
    "unset",
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

/// Option names for `set`/`unset` tab completion (sorted).
const OPTION_NAMES: &[&str] = &["dynamic", "ignore", "include-dynamic", "top", "top-modules"];

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
            "set" | "unset" => OPTION_NAMES
                .iter()
                .filter(|c| c.starts_with(partial))
                .take(MAX_COMPLETIONS)
                .map(|&c| Pair {
                    display: c.to_string(),
                    replacement: c.to_string(),
                })
                .collect(),
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

    let mut settings = ReplSettings::default();
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
            Command::Trace(file, ref opts) => {
                dispatch_trace(&mut session, file.as_deref(), opts, &settings, color, sc);
            }
            Command::Entry(path) => dispatch_entry(&mut session, &path, sc),
            Command::Chain(target, ref opts) => {
                dispatch_chain(&session, &target, opts, &settings, color, sc);
            }
            Command::Cut(target, ref opts) => {
                dispatch_cut(&mut session, &target, opts, &settings, color, sc);
            }
            Command::Diff(path, ref opts) => {
                dispatch_diff(&mut session, &path, opts, &settings, color, sc);
            }
            Command::Packages(ref opts) => {
                dispatch_packages(&session, opts, &settings, color);
            }
            Command::Imports(path, ref opts) => dispatch_imports(&session, &path, opts, sc),
            Command::Importers(path, ref opts) => {
                dispatch_importers(&session, &path, opts, sc);
            }
            Command::Info(name) => dispatch_info(&session, &name, sc),
            Command::Set(arg) => dispatch_set(&mut settings, &arg, sc),
            Command::Unset(arg) => dispatch_unset(&mut settings, &arg, sc),
            Command::Show => dispatch_show(&settings),
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

fn dispatch_trace(
    session: &mut Session,
    file: Option<&str>,
    opts: &CommandOptions,
    settings: &ReplSettings,
    color: bool,
    sc: StderrColor,
) {
    let (trace_opts, top_modules) = opts.resolve(settings);
    let report = if let Some(f) = file {
        match session.trace_from_report(Path::new(f), &trace_opts, top_modules) {
            Ok((r, _)) => r,
            Err(e) => {
                eprintln!("{} {e}", sc.error("error:"));
                return;
            }
        }
    } else {
        session.trace_report(&trace_opts, top_modules)
    };
    if opts.json {
        println!("{}", report.to_json());
    } else {
        print!("{}", report.to_terminal(color));
    }
}

fn dispatch_entry(session: &mut Session, path: &str, sc: StderrColor) {
    if let Err(e) = session.set_entry(Path::new(path)) {
        eprintln!("{} {e}", sc.error("error:"));
        return;
    }
    let rel = report::relative_path(session.entry(), session.root());
    eprintln!("{} entry point is now {rel}", sc.status("Switched:"));
}

fn dispatch_chain(
    session: &Session,
    target: &str,
    opts: &CommandOptions,
    settings: &ReplSettings,
    color: bool,
    sc: StderrColor,
) {
    let resolved = session.resolve_target(target);
    if resolved.target == ChainTarget::Module(session.entry_id()) {
        eprintln!("{} target is the entry point itself", sc.error("error:"));
        return;
    }
    let (trace_opts, _) = opts.resolve(settings);
    let report = session.chain_report(target, trace_opts.include_dynamic);
    if opts.json {
        println!("{}", report.to_json());
    } else {
        print!("{}", report.to_terminal(color));
    }
}

fn dispatch_cut(
    session: &mut Session,
    target: &str,
    opts: &CommandOptions,
    settings: &ReplSettings,
    color: bool,
    sc: StderrColor,
) {
    let resolved = session.resolve_target(target);
    if resolved.target == ChainTarget::Module(session.entry_id()) {
        eprintln!("{} target is the entry point itself", sc.error("error:"));
        return;
    }
    let (trace_opts, _) = opts.resolve(settings);
    let report = session.cut_report(target, trace_opts.top_n, trace_opts.include_dynamic);
    if opts.json {
        println!("{}", report.to_json());
    } else {
        print!("{}", report.to_terminal(color));
    }
}

fn dispatch_diff(
    session: &mut Session,
    path: &str,
    opts: &CommandOptions,
    settings: &ReplSettings,
    color: bool,
    sc: StderrColor,
) {
    let (trace_opts, _) = opts.resolve(settings);
    match session.diff_report(Path::new(path), &trace_opts, trace_opts.top_n) {
        Ok(report) => print!("{}", report.to_terminal(color)),
        Err(e) => eprintln!("{} {e}", sc.error("error:")),
    }
}

fn dispatch_packages(
    session: &Session,
    opts: &CommandOptions,
    settings: &ReplSettings,
    color: bool,
) {
    let top = opts.top.unwrap_or(settings.top);
    let report = session.packages_report(top);
    if opts.json {
        println!("{}", report.to_json());
    } else {
        print!("{}", report.to_terminal(color));
    }
}

fn dispatch_imports(session: &Session, path: &str, opts: &CommandOptions, sc: StderrColor) {
    match session.imports(Path::new(path)) {
        Ok(imports) => {
            if opts.json {
                let entries: Vec<_> = imports
                    .iter()
                    .map(|(p, kind)| {
                        serde_json::json!({
                            "path": report::relative_path(p, session.root()),
                            "kind": match kind {
                                EdgeKind::Static => "static",
                                EdgeKind::Dynamic => "dynamic",
                                EdgeKind::TypeOnly => "type-only",
                            }
                        })
                    })
                    .collect();
                println!("{}", serde_json::to_string_pretty(&entries).unwrap());
                return;
            }
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

fn dispatch_importers(session: &Session, path: &str, opts: &CommandOptions, sc: StderrColor) {
    match session.importers(Path::new(path)) {
        Ok(importers) => {
            if opts.json {
                let entries: Vec<_> = importers
                    .iter()
                    .map(|(p, kind)| {
                        serde_json::json!({
                            "path": report::relative_path(p, session.root()),
                            "kind": match kind {
                                EdgeKind::Static => "static",
                                EdgeKind::Dynamic => "dynamic",
                                EdgeKind::TypeOnly => "type-only",
                            }
                        })
                    })
                    .collect();
                println!("{}", serde_json::to_string_pretty(&entries).unwrap());
                return;
            }
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

fn dispatch_set(settings: &mut ReplSettings, arg: &str, sc: StderrColor) {
    let mut parts = arg.split_whitespace();
    let Some(key) = parts.next() else {
        eprintln!("{} set requires an option name", sc.error("error:"));
        return;
    };
    match key {
        "dynamic" | "include-dynamic" => {
            let value = match parts.next() {
                Some("true") => true,
                Some("false") => false,
                None => !settings.include_dynamic, // toggle
                Some(v) => {
                    eprintln!(
                        "{} invalid value '{v}' for dynamic (expected true/false)",
                        sc.error("error:")
                    );
                    return;
                }
            };
            settings.include_dynamic = value;
            eprintln!("{} dynamic = {value}", sc.status("Set:"));
        }
        "top" => {
            let Some(val) = parts.next().and_then(|v| v.parse::<i32>().ok()) else {
                eprintln!("{} top requires a number", sc.error("error:"));
                return;
            };
            if val < -1 {
                eprintln!(
                    "{} invalid value {val} for top: must be -1 (all) or 0+",
                    sc.error("error:")
                );
                return;
            }
            settings.top = val;
            eprintln!("{} top = {val}", sc.status("Set:"));
        }
        "top-modules" => {
            let Some(val) = parts.next().and_then(|v| v.parse::<i32>().ok()) else {
                eprintln!("{} top-modules requires a number", sc.error("error:"));
                return;
            };
            if val < -1 {
                eprintln!(
                    "{} invalid value {val} for top-modules: must be -1 (all) or 0+",
                    sc.error("error:")
                );
                return;
            }
            settings.top_modules = val;
            eprintln!("{} top-modules = {val}", sc.status("Set:"));
        }
        "ignore" => {
            let pkgs: Vec<String> = parts.map(String::from).collect();
            if pkgs.is_empty() {
                eprintln!(
                    "{} ignore requires one or more package names",
                    sc.error("error:")
                );
                return;
            }
            eprintln!("{} ignore = [{}]", sc.status("Set:"), pkgs.join(", "));
            settings.ignore = pkgs;
        }
        _ => eprintln!(
            "{} unknown option '{key}' (try: dynamic, top, top-modules, ignore)",
            sc.error("error:")
        ),
    }
}

fn dispatch_unset(settings: &mut ReplSettings, key: &str, sc: StderrColor) {
    let key = key.trim();
    match key {
        "dynamic" | "include-dynamic" => {
            settings.include_dynamic = false;
            eprintln!("{} dynamic reset to false", sc.status("Unset:"));
        }
        "top" => {
            settings.top = report::DEFAULT_TOP;
            eprintln!(
                "{} top reset to {}",
                sc.status("Unset:"),
                report::DEFAULT_TOP
            );
        }
        "top-modules" => {
            settings.top_modules = report::DEFAULT_TOP_MODULES;
            eprintln!(
                "{} top-modules reset to {}",
                sc.status("Unset:"),
                report::DEFAULT_TOP_MODULES
            );
        }
        "ignore" => {
            settings.ignore.clear();
            eprintln!("{} ignore cleared", sc.status("Unset:"));
        }
        _ => eprintln!(
            "{} unknown option '{key}' (try: dynamic, top, top-modules, ignore)",
            sc.error("error:")
        ),
    }
}

fn dispatch_show(settings: &ReplSettings) {
    println!("Settings:");
    println!("  dynamic     = {}", settings.include_dynamic);
    println!("  top         = {}", settings.top);
    println!("  top-modules = {}", settings.top_modules);
    if settings.ignore.is_empty() {
        println!("  ignore      = (none)");
    } else {
        println!("  ignore      = [{}]", settings.ignore.join(", "));
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
    println!("  set <opt> [val]    Set a session option (omit val to toggle booleans)");
    println!("  unset <opt>        Reset an option to its default");
    println!("  show               Display current settings");
    println!("  help               Show this help");
    println!("  quit               Exit");
    println!();
    println!("Inline flags (override session settings for one command):");
    println!("  --json                      Output as JSON instead of terminal format");
    println!("  --include-dynamic / --no-include-dynamic    Include/exclude dynamic imports");
    println!("  --top N                     Limit heavy deps / packages shown");
    println!("  --top-modules N             Limit modules by exclusive weight");
    println!("  --ignore pkg1 pkg2 ...      Exclude packages from heavy deps");
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
        assert!(matches!(Command::parse("trace"), Command::Trace(None, _)));
    }

    #[test]
    fn parse_trace_with_file() {
        assert!(
            matches!(Command::parse("trace src/index.ts"), Command::Trace(Some(ref f), _) if f == "src/index.ts")
        );
    }

    #[test]
    fn parse_chain() {
        assert!(matches!(Command::parse("chain zod"), Command::Chain(ref t, _) if t == "zod"));
    }

    #[test]
    fn parse_entry() {
        assert!(
            matches!(Command::parse("entry src/other.ts"), Command::Entry(ref f) if f == "src/other.ts")
        );
    }

    #[test]
    fn parse_packages() {
        assert!(matches!(Command::parse("packages"), Command::Packages(_)));
    }

    #[test]
    fn parse_imports() {
        assert!(
            matches!(Command::parse("imports src/foo.ts"), Command::Imports(ref f, _) if f == "src/foo.ts")
        );
    }

    #[test]
    fn parse_importers() {
        assert!(
            matches!(Command::parse("importers lib/bar.py"), Command::Importers(ref f, _) if f == "lib/bar.py")
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
            matches!(Command::parse("chain @scope/pkg"), Command::Chain(ref t, _) if t == "@scope/pkg")
        );
    }

    #[test]
    fn parse_trims_whitespace() {
        assert!(matches!(Command::parse("  quit  "), Command::Quit));
    }

    #[test]
    fn settings_defaults() {
        let s = ReplSettings::default();
        assert!(!s.include_dynamic);
        assert_eq!(s.top, report::DEFAULT_TOP);
        assert_eq!(s.top_modules, report::DEFAULT_TOP_MODULES);
        assert!(s.ignore.is_empty());
    }

    #[test]
    fn command_options_resolve_uses_settings_when_none() {
        let settings = ReplSettings::default();
        let opts = CommandOptions::default();
        let (trace_opts, top_modules) = opts.resolve(&settings);
        assert!(!trace_opts.include_dynamic);
        assert_eq!(trace_opts.top_n, report::DEFAULT_TOP);
        assert!(trace_opts.ignore.is_empty());
        assert_eq!(top_modules, report::DEFAULT_TOP_MODULES);
    }

    #[test]
    fn command_options_resolve_overrides_settings() {
        let settings = ReplSettings::default();
        let opts = CommandOptions {
            include_dynamic: Some(true),
            top: Some(5),
            top_modules: Some(50),
            ignore: Some(vec!["zod".into()]),
            json: false,
        };
        let (trace_opts, top_modules) = opts.resolve(&settings);
        assert!(trace_opts.include_dynamic);
        assert_eq!(trace_opts.top_n, 5);
        assert_eq!(trace_opts.ignore, vec!["zod".to_string()]);
        assert_eq!(top_modules, 50);
    }

    #[test]
    fn parse_flags_no_flags() {
        let (opts, remaining) = parse_flags(&["src/index.ts"]).unwrap();
        assert!(opts.include_dynamic.is_none());
        assert!(opts.top.is_none());
        assert_eq!(remaining, "src/index.ts");
    }

    #[test]
    fn parse_flags_dynamic() {
        let (opts, remaining) = parse_flags(&["--include-dynamic", "src/index.ts"]).unwrap();
        assert_eq!(opts.include_dynamic, Some(true));
        assert_eq!(remaining, "src/index.ts");
    }

    #[test]
    fn parse_flags_no_dynamic() {
        let (opts, remaining) = parse_flags(&["--no-include-dynamic", "src/index.ts"]).unwrap();
        assert_eq!(opts.include_dynamic, Some(false));
        assert_eq!(remaining, "src/index.ts");
    }

    #[test]
    fn parse_flags_top() {
        let (opts, remaining) = parse_flags(&["--top", "5", "src/index.ts"]).unwrap();
        assert_eq!(opts.top, Some(5));
        assert_eq!(remaining, "src/index.ts");
    }

    #[test]
    fn parse_flags_top_modules() {
        let (opts, remaining) = parse_flags(&["--top-modules", "30", "src/index.ts"]).unwrap();
        assert_eq!(opts.top_modules, Some(30));
        assert_eq!(remaining, "src/index.ts");
    }

    #[test]
    fn parse_flags_ignore() {
        // --ignore is greedy: consumes all non-flag tokens after it.
        // Users should put --ignore last or use `set ignore`.
        let (opts, remaining) =
            parse_flags(&["src/index.ts", "--ignore", "zod", "lodash"]).unwrap();
        assert_eq!(opts.ignore, Some(vec!["zod".into(), "lodash".into()]));
        assert_eq!(remaining, "src/index.ts");
    }

    #[test]
    fn parse_flags_ignore_stops_at_next_flag() {
        let (opts, remaining) =
            parse_flags(&["src/index.ts", "--ignore", "zod", "--include-dynamic"]).unwrap();
        assert_eq!(opts.ignore, Some(vec!["zod".to_string()]));
        assert_eq!(opts.include_dynamic, Some(true));
        assert_eq!(remaining, "src/index.ts");
    }

    #[test]
    fn parse_flags_multiple() {
        let (opts, remaining) = parse_flags(&["--include-dynamic", "--top", "5", "zod"]).unwrap();
        assert_eq!(opts.include_dynamic, Some(true));
        assert_eq!(opts.top, Some(5));
        assert_eq!(remaining, "zod");
    }

    #[test]
    fn parse_flags_empty() {
        let (opts, remaining) = parse_flags(&[]).unwrap();
        assert!(opts.include_dynamic.is_none());
        assert!(remaining.is_empty());
    }

    #[test]
    fn parse_flags_only_flags_no_positional() {
        let (opts, remaining) = parse_flags(&["--include-dynamic"]).unwrap();
        assert_eq!(opts.include_dynamic, Some(true));
        assert!(remaining.is_empty());
    }

    #[test]
    fn parse_flags_scoped_package_not_treated_as_flag() {
        let (opts, remaining) = parse_flags(&["@scope/pkg"]).unwrap();
        assert!(opts.include_dynamic.is_none());
        assert_eq!(remaining, "@scope/pkg");
    }

    #[test]
    fn parse_flags_top_non_numeric_preserves_positional() {
        // When --top is followed by a non-numeric token, the token should not
        // be consumed as the --top value — it stays as a positional arg.
        let (opts, remaining) = parse_flags(&["--top", "src/index.ts"]).unwrap();
        assert!(opts.top.is_none());
        assert_eq!(remaining, "src/index.ts");
    }

    #[test]
    fn parse_flags_top_modules_non_numeric_preserves_positional() {
        let (opts, remaining) = parse_flags(&["--top-modules", "src/index.ts"]).unwrap();
        assert!(opts.top_modules.is_none());
        assert_eq!(remaining, "src/index.ts");
    }

    #[test]
    fn parse_flags_top_rejects_negative_below_minus_one() {
        let (opts, _) = parse_flags(&["--top", "-5", "src/index.ts"]).unwrap();
        assert!(opts.top.is_none());
    }

    #[test]
    fn parse_flags_top_accepts_negative_one() {
        let (opts, _) = parse_flags(&["--top", "-1", "src/index.ts"]).unwrap();
        assert_eq!(opts.top, Some(-1));
    }

    #[test]
    fn parse_flags_top_modules_rejects_negative_below_minus_one() {
        let (opts, _) = parse_flags(&["--top-modules", "-5", "src/index.ts"]).unwrap();
        assert!(opts.top_modules.is_none());
    }

    #[test]
    fn parse_flags_top_modules_accepts_negative_one() {
        let (opts, _) = parse_flags(&["--top-modules", "-1", "src/index.ts"]).unwrap();
        assert_eq!(opts.top_modules, Some(-1));
    }

    #[test]
    fn parse_flags_json() {
        let (opts, remaining) = parse_flags(&["--json", "src/index.ts"]).unwrap();
        assert!(opts.json);
        assert_eq!(remaining, "src/index.ts");
    }

    #[test]
    fn parse_flags_unknown_flag_returns_error() {
        let err = parse_flags(&["--bogus", "src/index.ts"]).unwrap_err();
        assert!(err.contains("unknown flag '--bogus'"));
    }

    #[test]
    fn parse_trace_with_flags() {
        let cmd = Command::parse("trace --include-dynamic --top 5 src/index.ts");
        match cmd {
            Command::Trace(Some(ref f), ref opts) => {
                assert_eq!(f, "src/index.ts");
                assert_eq!(opts.include_dynamic, Some(true));
                assert_eq!(opts.top, Some(5));
            }
            other => panic!("expected Trace, got {other:?}"),
        }
    }

    #[test]
    fn parse_trace_flags_no_file() {
        let cmd = Command::parse("trace --include-dynamic");
        match cmd {
            Command::Trace(None, ref opts) => {
                assert_eq!(opts.include_dynamic, Some(true));
            }
            other => panic!("expected Trace(None, _), got {other:?}"),
        }
    }

    #[test]
    fn parse_chain_with_dynamic() {
        let cmd = Command::parse("chain --include-dynamic zod");
        match cmd {
            Command::Chain(ref target, ref opts) => {
                assert_eq!(target, "zod");
                assert_eq!(opts.include_dynamic, Some(true));
            }
            other => panic!("expected Chain, got {other:?}"),
        }
    }

    #[test]
    fn parse_cut_with_top() {
        let cmd = Command::parse("cut --top 3 zod");
        match cmd {
            Command::Cut(ref target, ref opts) => {
                assert_eq!(target, "zod");
                assert_eq!(opts.top, Some(3));
            }
            other => panic!("expected Cut, got {other:?}"),
        }
    }

    #[test]
    fn parse_packages_with_top() {
        let cmd = Command::parse("packages --top 20");
        match cmd {
            Command::Packages(ref opts) => {
                assert_eq!(opts.top, Some(20));
            }
            other => panic!("expected Packages, got {other:?}"),
        }
    }

    #[test]
    fn parse_set() {
        assert!(matches!(
            Command::parse("set dynamic"),
            Command::Set(ref s) if s == "dynamic"
        ));
        assert!(matches!(
            Command::parse("set top 5"),
            Command::Set(ref s) if s == "top 5"
        ));
    }

    #[test]
    fn parse_unset() {
        assert!(matches!(
            Command::parse("unset ignore"),
            Command::Unset(ref s) if s == "ignore"
        ));
    }

    #[test]
    fn parse_show() {
        assert!(matches!(Command::parse("show"), Command::Show));
    }

    #[test]
    fn parse_set_missing_arg() {
        assert!(matches!(Command::parse("set"), Command::Unknown(_)));
    }

    #[test]
    fn parse_unset_missing_arg() {
        assert!(matches!(Command::parse("unset"), Command::Unknown(_)));
    }

    #[test]
    fn set_dynamic_toggle() {
        let mut s = ReplSettings::default();
        dispatch_set(&mut s, "dynamic", StderrColor::new(true));
        assert!(s.include_dynamic);
        dispatch_set(&mut s, "dynamic", StderrColor::new(true));
        assert!(!s.include_dynamic);
    }

    #[test]
    fn set_dynamic_explicit() {
        let mut s = ReplSettings::default();
        dispatch_set(&mut s, "dynamic true", StderrColor::new(true));
        assert!(s.include_dynamic);
        dispatch_set(&mut s, "dynamic false", StderrColor::new(true));
        assert!(!s.include_dynamic);
    }

    #[test]
    fn set_include_dynamic_alias() {
        let mut s = ReplSettings::default();
        dispatch_set(&mut s, "include-dynamic true", StderrColor::new(true));
        assert!(s.include_dynamic);
    }

    #[test]
    fn unset_include_dynamic_alias() {
        let mut s = ReplSettings {
            include_dynamic: true,
            ..ReplSettings::default()
        };
        dispatch_unset(&mut s, "include-dynamic", StderrColor::new(true));
        assert!(!s.include_dynamic);
    }

    #[test]
    fn set_top() {
        let mut s = ReplSettings::default();
        dispatch_set(&mut s, "top 5", StderrColor::new(true));
        assert_eq!(s.top, 5);
    }

    #[test]
    fn set_top_modules() {
        let mut s = ReplSettings::default();
        dispatch_set(&mut s, "top-modules 30", StderrColor::new(true));
        assert_eq!(s.top_modules, 30);
    }

    #[test]
    fn set_top_rejects_invalid_value() {
        let mut s = ReplSettings::default();
        dispatch_set(&mut s, "top -5", StderrColor::new(true));
        // Value should remain at default since -5 < -1.
        assert_eq!(s.top, report::DEFAULT_TOP);
    }

    #[test]
    fn set_top_modules_rejects_invalid_value() {
        let mut s = ReplSettings::default();
        dispatch_set(&mut s, "top-modules -2", StderrColor::new(true));
        assert_eq!(s.top_modules, report::DEFAULT_TOP_MODULES);
    }

    #[test]
    fn set_top_accepts_negative_one() {
        let mut s = ReplSettings::default();
        dispatch_set(&mut s, "top -1", StderrColor::new(true));
        assert_eq!(s.top, -1);
    }

    #[test]
    fn set_top_accepts_zero() {
        let mut s = ReplSettings::default();
        dispatch_set(&mut s, "top 0", StderrColor::new(true));
        assert_eq!(s.top, 0);
    }

    #[test]
    fn set_ignore() {
        let mut s = ReplSettings::default();
        dispatch_set(&mut s, "ignore zod lodash", StderrColor::new(true));
        assert_eq!(s.ignore, vec!["zod".to_string(), "lodash".to_string()]);
    }

    #[test]
    fn unset_dynamic() {
        let mut s = ReplSettings {
            include_dynamic: true,
            ..ReplSettings::default()
        };
        dispatch_unset(&mut s, "dynamic", StderrColor::new(true));
        assert!(!s.include_dynamic);
    }

    #[test]
    fn unset_ignore() {
        let mut s = ReplSettings {
            ignore: vec!["zod".into()],
            ..ReplSettings::default()
        };
        dispatch_unset(&mut s, "ignore", StderrColor::new(true));
        assert!(s.ignore.is_empty());
    }

    #[test]
    fn unset_top() {
        let mut s = ReplSettings {
            top: 99,
            ..ReplSettings::default()
        };
        dispatch_unset(&mut s, "top", StderrColor::new(true));
        assert_eq!(s.top, report::DEFAULT_TOP);
    }

    #[test]
    fn unset_top_modules() {
        let mut s = ReplSettings {
            top_modules: 99,
            ..ReplSettings::default()
        };
        dispatch_unset(&mut s, "top-modules", StderrColor::new(true));
        assert_eq!(s.top_modules, report::DEFAULT_TOP_MODULES);
    }

    #[test]
    fn parse_diff_with_dynamic() {
        let cmd = Command::parse("diff --include-dynamic src/other.ts");
        match cmd {
            Command::Diff(ref path, ref opts) => {
                assert_eq!(path, "src/other.ts");
                assert_eq!(opts.include_dynamic, Some(true));
            }
            other => panic!("expected Diff, got {other:?}"),
        }
    }
}
