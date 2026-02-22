//! Human-readable output formatting for trace results, diffs, and package lists.

use std::collections::HashMap;
use std::io::IsTerminal;
use std::path::{Component, Path, PathBuf};

use serde::Serialize;

use crate::graph::{ModuleGraph, ModuleId};
use crate::query::{CutModule, DiffResult, TraceResult};

/// Default number of heavy dependencies to display.
pub const DEFAULT_TOP: i32 = 10;
/// Default number of modules by exclusive weight to display.
pub const DEFAULT_TOP_MODULES: i32 = 20;

/// Determine whether color output should be used for a given stream.
///
/// Color is disabled when any of these hold:
/// - `no_color_flag` is true (`--no-color`)
/// - `no_color_env` is true (`NO_COLOR` set, per <https://no-color.org>)
/// - `term_dumb` is true (`TERM=dumb`)
/// - the stream is not a TTY
#[allow(clippy::fn_params_excessive_bools)]
pub const fn should_use_color(
    stream_is_tty: bool,
    no_color_flag: bool,
    no_color_env: bool,
    term_dumb: bool,
) -> bool {
    if no_color_flag || no_color_env || term_dumb {
        return false;
    }
    stream_is_tty
}

const fn plural(n: u64) -> &'static str {
    if n == 1 { "" } else { "s" }
}

#[derive(Clone, Copy)]
struct C {
    color: bool,
}

impl C {
    fn new(no_color: bool) -> Self {
        Self {
            color: should_use_color(
                std::io::stdout().is_terminal(),
                no_color,
                std::env::var_os("NO_COLOR").is_some(),
                std::env::var("TERM").is_ok_and(|v| v == "dumb"),
            ),
        }
    }

    fn bold_green(self, s: &str) -> String {
        if self.color {
            format!("\x1b[1;92m{s}\x1b[0m")
        } else {
            s.to_string()
        }
    }

    fn red(self, s: &str) -> String {
        if self.color {
            format!("\x1b[31m{s}\x1b[0m")
        } else {
            s.to_string()
        }
    }

    fn green(self, s: &str) -> String {
        if self.color {
            format!("\x1b[32m{s}\x1b[0m")
        } else {
            s.to_string()
        }
    }

    fn dim(self, s: &str) -> String {
        if self.color {
            format!("\x1b[2m{s}\x1b[0m")
        } else {
            s.to_string()
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct StderrColor {
    color: bool,
}

impl StderrColor {
    pub fn new(no_color: bool) -> Self {
        Self {
            color: should_use_color(
                std::io::stderr().is_terminal(),
                no_color,
                std::env::var_os("NO_COLOR").is_some(),
                std::env::var("TERM").is_ok_and(|v| v == "dumb"),
            ),
        }
    }

    pub fn error(self, s: &str) -> String {
        if self.color {
            format!("\x1b[1;91m{s}\x1b[0m")
        } else {
            s.to_string()
        }
    }

    pub fn warning(self, s: &str) -> String {
        if self.color {
            format!("\x1b[1;93m{s}\x1b[0m")
        } else {
            s.to_string()
        }
    }

    pub fn status(self, s: &str) -> String {
        if self.color {
            format!("\x1b[1;92m{s}\x1b[0m")
        } else {
            s.to_string()
        }
    }
}

/// Print the standard graph-load status line plus any warnings.
///
/// Used by the CLI (trace, packages, diff) and the REPL startup to avoid
/// duplicating the same formatting logic.
#[allow(clippy::too_many_arguments)]
pub fn print_load_status(
    from_cache: bool,
    module_count: usize,
    elapsed_ms: f64,
    file_warnings: &[String],
    unresolvable_dynamic_count: usize,
    unresolvable_dynamic_files: &[(PathBuf, usize)],
    root: &Path,
    sc: StderrColor,
) {
    eprintln!(
        "{} ({module_count} modules) in {elapsed_ms:.1}ms",
        sc.status(if from_cache {
            "Loaded cached graph"
        } else {
            "Built graph"
        }),
    );
    for w in file_warnings {
        eprintln!("{} {w}", sc.warning("warning:"));
    }
    if unresolvable_dynamic_count > 0 {
        let n = unresolvable_dynamic_count;
        eprintln!(
            "{} {n} dynamic import{} with non-literal argument{} could not be traced:",
            sc.warning("warning:"),
            if n == 1 { "" } else { "s" },
            if n == 1 { "" } else { "s" },
        );
        let mut files: Vec<_> = unresolvable_dynamic_files.to_vec();
        files.sort_by(|a, b| a.0.cmp(&b.0));
        for (path, file_count) in &files {
            let rel = relative_path(path, root);
            eprintln!("  {rel} ({file_count})");
        }
    }
}

#[allow(clippy::cast_precision_loss)]
pub fn format_size(bytes: u64) -> String {
    if bytes >= 1_000_000 {
        format!("{:.1} MB", bytes as f64 / 1_000_000.0)
    } else if bytes >= 1_000 {
        format!("{:.0} KB", bytes as f64 / 1_000.0)
    } else {
        format!("{bytes} B")
    }
}

pub fn relative_path(path: &Path, root: &Path) -> String {
    path.strip_prefix(root)
        .unwrap_or(path)
        .to_string_lossy()
        .into_owned()
}

fn display_name(graph: &ModuleGraph, mid: ModuleId, root: &Path) -> String {
    let m = graph.module(mid);
    m.package
        .clone()
        .unwrap_or_else(|| relative_path(&m.path, root))
}

/// Path relative to the package directory (e.g. `dateutil/__init__.py`).
/// Handles scoped packages (`@scope/name` spans two path components).
/// Falls back to the file name if the package name isn't found in path components.
fn package_relative_path(path: &Path, package_name: &str) -> String {
    let components: Vec<_> = path.components().collect();
    // Scan backwards to find the last match — avoids false matches from
    // workspace dirs that share a package name (e.g. pnpm store paths
    // like .pnpm/cloudflare@5.2.0/node_modules/cloudflare/index.js where
    // an ancestor dir is also named "cloudflare").
    if let Some((scope, name)) = package_name.split_once('/') {
        for (i, pair) in components.windows(2).enumerate().rev() {
            if let (Component::Normal(a), Component::Normal(b)) = (&pair[0], &pair[1])
                && a.to_str() == Some(scope)
                && b.to_str() == Some(name)
            {
                let sub: PathBuf = components[i..].iter().collect();
                return sub.to_string_lossy().into_owned();
            }
        }
    } else {
        for (i, comp) in components.iter().enumerate().rev() {
            if let Component::Normal(name) = comp
                && name.to_str() == Some(package_name)
            {
                let sub: PathBuf = components[i..].iter().collect();
                return sub.to_string_lossy().into_owned();
            }
        }
    }
    path.file_name()
        .and_then(|n| n.to_str())
        .unwrap_or("?")
        .to_string()
}

#[derive(Debug, Clone, Copy)]
pub struct DisplayOpts {
    pub top: i32,
    pub top_modules: i32,
    pub include_dynamic: bool,
    pub no_color: bool,
    pub max_weight: Option<u64>,
}

/// Build display names for a chain, expanding duplicate package nodes
/// to package-relative file paths for disambiguation.
fn chain_display_names(graph: &ModuleGraph, chain: &[ModuleId], root: &Path) -> Vec<String> {
    let names: Vec<String> = chain
        .iter()
        .map(|&mid| display_name(graph, mid, root))
        .collect();
    let mut counts: HashMap<&str, usize> = HashMap::new();
    for name in &names {
        *counts.entry(name.as_str()).or_default() += 1;
    }
    names
        .iter()
        .enumerate()
        .map(|(i, name)| {
            if counts[name.as_str()] > 1 {
                let m = graph.module(chain[i]);
                if let Some(ref pkg) = m.package {
                    return package_relative_path(&m.path, pkg);
                }
            }
            name.clone()
        })
        .collect()
}

#[allow(clippy::cast_sign_loss)]
pub fn print_trace(
    graph: &ModuleGraph,
    result: &TraceResult,
    entry_path: &Path,
    root: &Path,
    opts: &DisplayOpts,
) {
    let c = C::new(opts.no_color);
    println!("{}", relative_path(entry_path, root));
    let (kind, suffix) = if opts.include_dynamic {
        ("total", ", static + dynamic")
    } else {
        ("static", "")
    };
    let weight = format_size(result.static_weight);
    let modules = format!(
        "{} module{}{}",
        result.static_module_count,
        plural(result.static_module_count as u64),
        suffix,
    );

    if let Some(threshold) = opts.max_weight.filter(|&t| result.static_weight > t) {
        let sc = StderrColor::new(opts.no_color);
        eprintln!(
            "{} {kind} transitive weight {weight} ({modules}) exceeds --max-weight threshold {}",
            sc.error("error:"),
            format_size(threshold),
        );
    } else {
        let label = if opts.include_dynamic {
            "Total transitive weight:"
        } else {
            "Static transitive weight:"
        };
        println!("{} {weight} ({modules})", c.bold_green(label));
    }

    if !opts.include_dynamic && result.dynamic_only_module_count > 0 {
        println!(
            "{} {} ({} module{}, not loaded at startup)",
            c.bold_green("Dynamic-only weight:"),
            format_size(result.dynamic_only_weight),
            result.dynamic_only_module_count,
            plural(result.dynamic_only_module_count as u64)
        );
    }

    if opts.top != 0 {
        println!();
        let deps_label = if opts.include_dynamic {
            "Heavy dependencies (static + dynamic):"
        } else {
            "Heavy dependencies (static):"
        };
        println!("{}", c.bold_green(deps_label));
        if result.heavy_packages.is_empty() {
            println!("  (none \u{2014} all reachable modules are first-party)");
        } else {
            for pkg in &result.heavy_packages {
                println!(
                    "  {:<35} {}  {} file{}",
                    pkg.name,
                    format_size(pkg.total_size),
                    pkg.file_count,
                    plural(u64::from(pkg.file_count))
                );
                if pkg.chain.len() > 1 {
                    let chain_str = chain_display_names(graph, &pkg.chain, root);
                    println!("    -> {}", chain_str.join(" -> "));
                }
            }
        }
        println!();
    }

    if opts.top_modules != 0 && !result.modules_by_cost.is_empty() {
        println!("{}", c.bold_green("Modules (sorted by exclusive weight):"));
        let display_count = if opts.top_modules < 0 {
            result.modules_by_cost.len()
        } else {
            result.modules_by_cost.len().min(opts.top_modules as usize)
        };
        for mc in &result.modules_by_cost[..display_count] {
            let m = graph.module(mc.module_id);
            println!(
                "  {:<55} {}",
                relative_path(&m.path, root),
                format_size(mc.exclusive_size)
            );
        }
        if result.modules_by_cost.len() > display_count {
            let remaining = result.modules_by_cost.len() - display_count;
            println!(
                "  ... and {remaining} more module{}",
                plural(remaining as u64)
            );
        }
    }
}

#[allow(clippy::cast_sign_loss, clippy::too_many_lines)]
pub fn print_diff(diff: &DiffResult, entry_a: &str, entry_b: &str, limit: i32, no_color: bool) {
    let c = C::new(no_color);
    println!("Diff: {entry_a} vs {entry_b}");
    println!();
    println!("  {:<40} {}", entry_a, format_size(diff.entry_a_weight));
    println!("  {:<40} {}", entry_b, format_size(diff.entry_b_weight));
    let sign = if diff.weight_delta >= 0 { "+" } else { "-" };
    println!(
        "  {:<40} {sign}{}",
        "Delta",
        format_size(diff.weight_delta.unsigned_abs())
    );

    let has_dynamic = diff.dynamic_a_weight > 0 || diff.dynamic_b_weight > 0;
    if has_dynamic {
        println!();
        println!(
            "  {:<40} {}",
            "Dynamic-only (before)",
            format_size(diff.dynamic_a_weight)
        );
        println!(
            "  {:<40} {}",
            "Dynamic-only (after)",
            format_size(diff.dynamic_b_weight)
        );
        let dyn_sign = if diff.dynamic_weight_delta >= 0 {
            "+"
        } else {
            "-"
        };
        println!(
            "  {:<40} {dyn_sign}{}",
            "Dynamic delta",
            format_size(diff.dynamic_weight_delta.unsigned_abs())
        );
    }

    println!();

    if !diff.only_in_a.is_empty() {
        let show = if limit < 0 {
            diff.only_in_a.len()
        } else {
            diff.only_in_a.len().min(limit as usize)
        };
        println!("{}", c.red(&format!("Only in {entry_a}:")));
        for pkg in &diff.only_in_a[..show] {
            println!(
                "{}",
                c.red(&format!("  - {:<35} {}", pkg.name, format_size(pkg.size)))
            );
        }
        let remaining = diff.only_in_a.len() - show;
        if remaining > 0 {
            println!("{}", c.dim(&format!("  - ... and {remaining} more")));
        }
    }
    if !diff.only_in_b.is_empty() {
        let show = if limit < 0 {
            diff.only_in_b.len()
        } else {
            diff.only_in_b.len().min(limit as usize)
        };
        println!("{}", c.green(&format!("Only in {entry_b}:")));
        for pkg in &diff.only_in_b[..show] {
            println!(
                "{}",
                c.green(&format!("  + {:<35} {}", pkg.name, format_size(pkg.size)))
            );
        }
        let remaining = diff.only_in_b.len() - show;
        if remaining > 0 {
            println!("{}", c.dim(&format!("  + ... and {remaining} more")));
        }
    }

    if !diff.dynamic_only_in_a.is_empty() {
        let show = if limit < 0 {
            diff.dynamic_only_in_a.len()
        } else {
            diff.dynamic_only_in_a.len().min(limit as usize)
        };
        println!("{}", c.red(&format!("Dynamic only in {entry_a}:")));
        for pkg in &diff.dynamic_only_in_a[..show] {
            println!(
                "{}",
                c.red(&format!("  - {:<35} {}", pkg.name, format_size(pkg.size)))
            );
        }
        let remaining = diff.dynamic_only_in_a.len() - show;
        if remaining > 0 {
            println!("{}", c.dim(&format!("  - ... and {remaining} more")));
        }
    }
    if !diff.dynamic_only_in_b.is_empty() {
        let show = if limit < 0 {
            diff.dynamic_only_in_b.len()
        } else {
            diff.dynamic_only_in_b.len().min(limit as usize)
        };
        println!("{}", c.green(&format!("Dynamic only in {entry_b}:")));
        for pkg in &diff.dynamic_only_in_b[..show] {
            println!(
                "{}",
                c.green(&format!("  + {:<35} {}", pkg.name, format_size(pkg.size)))
            );
        }
        let remaining = diff.dynamic_only_in_b.len() - show;
        if remaining > 0 {
            println!("{}", c.dim(&format!("  + ... and {remaining} more")));
        }
    }

    if diff.shared_count > 0 {
        println!(
            "{}",
            c.dim(&format!(
                "Shared: {} package{}",
                diff.shared_count,
                if diff.shared_count == 1 { "" } else { "s" }
            ))
        );
    }
}

pub fn print_chains(
    graph: &ModuleGraph,
    chains: &[Vec<ModuleId>],
    target_label: &str,
    root: &Path,
    target_exists: bool,
    no_color: bool,
) {
    let c = C::new(no_color);
    if chains.is_empty() {
        if target_exists {
            eprintln!(
                "\"{target_label}\" exists in the graph but is not reachable from this entry point."
            );
        } else {
            eprintln!(
                "\"{target_label}\" is not in the dependency graph. Check the spelling or verify it's installed."
            );
        }
        return;
    }
    let hops = chains[0].len().saturating_sub(1);
    println!(
        "{}\n",
        c.bold_green(&format!(
            "{} chain{} to \"{}\" ({} hop{}):",
            chains.len(),
            if chains.len() == 1 { "" } else { "s" },
            target_label,
            hops,
            if hops == 1 { "" } else { "s" },
        )),
    );
    for (i, chain) in chains.iter().enumerate() {
        let chain_str = chain_display_names(graph, chain, root);
        println!("  {}. {}", i + 1, chain_str.join(" -> "));
    }
}

pub fn print_chains_json(
    graph: &ModuleGraph,
    chains: &[Vec<ModuleId>],
    target_label: &str,
    root: &Path,
    target_exists: bool,
) {
    if chains.is_empty() {
        let json = JsonChainsEmpty {
            target: target_label.to_string(),
            found_in_graph: target_exists,
            chain_count: 0,
            chains: Vec::new(),
        };
        println!("{}", serde_json::to_string_pretty(&json).unwrap());
        return;
    }
    let json = JsonChains {
        target: target_label.to_string(),
        chain_count: chains.len(),
        hop_count: chains.first().map_or(0, |c| c.len().saturating_sub(1)),
        chains: chains
            .iter()
            .map(|chain| chain_display_names(graph, chain, root))
            .collect(),
    };
    println!("{}", serde_json::to_string_pretty(&json).unwrap());
}

pub fn print_cut(
    graph: &ModuleGraph,
    cuts: &[CutModule],
    chains: &[Vec<ModuleId>],
    target_label: &str,
    root: &Path,
    target_exists: bool,
    no_color: bool,
) {
    let c = C::new(no_color);
    if chains.is_empty() {
        if target_exists {
            eprintln!(
                "\"{target_label}\" exists in the graph but is not reachable from this entry point."
            );
        } else {
            eprintln!(
                "\"{target_label}\" is not in the dependency graph. Check the spelling or verify it's installed."
            );
        }
        return;
    }

    if cuts.is_empty() {
        let is_direct = chains.iter().all(|c| c.len() == 2);
        if is_direct {
            println!(
                "Entry file directly imports \"{target_label}\" \u{2014} remove the import to sever the dependency."
            );
        } else {
            println!(
                "No single cut point can sever all {} chain{} to \"{target_label}\".",
                chains.len(),
                if chains.len() == 1 { "" } else { "s" },
            );
            println!("Each chain takes a different path \u{2014} multiple fixes needed.");
        }
        return;
    }

    println!(
        "{}\n",
        c.bold_green(&format!(
            "{} cut point{} to sever all {} chain{} to \"{}\":",
            cuts.len(),
            if cuts.len() == 1 { "" } else { "s" },
            chains.len(),
            if chains.len() == 1 { "" } else { "s" },
            target_label,
        )),
    );
    for cut in cuts {
        if chains.len() == 1 {
            println!(
                "  {:<45} {:>8}",
                display_name(graph, cut.module_id, root),
                format_size(cut.exclusive_size),
            );
        } else {
            println!(
                "  {:<45} {:>8}  (breaks {}/{} chains)",
                display_name(graph, cut.module_id, root),
                format_size(cut.exclusive_size),
                cut.chains_broken,
                chains.len()
            );
        }
    }
}

pub fn print_cut_json(
    graph: &ModuleGraph,
    cuts: &[CutModule],
    chains: &[Vec<ModuleId>],
    target_label: &str,
    root: &Path,
    target_exists: bool,
) {
    if chains.is_empty() {
        let json = JsonChainsEmpty {
            target: target_label.to_string(),
            found_in_graph: target_exists,
            chain_count: 0,
            chains: Vec::new(),
        };
        println!("{}", serde_json::to_string_pretty(&json).unwrap());
        return;
    }

    let json = JsonCut {
        target: target_label.to_string(),
        chain_count: chains.len(),
        direct_import: cuts.is_empty() && chains.iter().all(|c| c.len() == 2),
        cut_points: cuts
            .iter()
            .map(|c| JsonCutPoint {
                module: display_name(graph, c.module_id, root),
                exclusive_size_bytes: c.exclusive_size,
                chains_broken: c.chains_broken,
            })
            .collect(),
    };
    println!("{}", serde_json::to_string_pretty(&json).unwrap());
}

#[allow(clippy::cast_sign_loss)]
pub fn print_packages(graph: &ModuleGraph, top: i32, no_color: bool) {
    let c = C::new(no_color);
    let mut packages: Vec<_> = graph.package_map.values().collect();
    packages.sort_by(|a, b| b.total_reachable_size.cmp(&a.total_reachable_size));

    if packages.is_empty() {
        println!("No third-party packages found in the dependency graph.");
        return;
    }

    let total = packages.len();
    let display_count = if top < 0 {
        total
    } else {
        total.min(top as usize)
    };

    println!(
        "{}\n",
        c.bold_green(&format!("{} package{}:", total, plural(total as u64)))
    );
    for pkg in &packages[..display_count] {
        println!(
            "  {:<40} {:>8}  {} file{}",
            pkg.name,
            format_size(pkg.total_reachable_size),
            pkg.total_reachable_files,
            plural(u64::from(pkg.total_reachable_files))
        );
    }
    if total > display_count {
        let remaining = total - display_count;
        println!(
            "  ... and {remaining} more package{}",
            plural(remaining as u64)
        );
    }
}

#[allow(clippy::cast_sign_loss)]
pub fn print_packages_json(graph: &ModuleGraph, top: i32) {
    let mut packages: Vec<_> = graph.package_map.values().collect();
    packages.sort_by(|a, b| b.total_reachable_size.cmp(&a.total_reachable_size));

    let total = packages.len();
    let display_count = if top < 0 {
        total
    } else {
        total.min(top as usize)
    };

    let json_packages: Vec<serde_json::Value> = packages[..display_count]
        .iter()
        .map(|pkg| {
            serde_json::json!({
                "name": pkg.name,
                "size": pkg.total_reachable_size,
                "files": pkg.total_reachable_files,
            })
        })
        .collect();

    let output = serde_json::json!({
        "package_count": total,
        "packages": json_packages,
    });
    println!("{}", serde_json::to_string_pretty(&output).unwrap());
}

// JSON output types

#[derive(Serialize)]
struct JsonCut {
    target: String,
    chain_count: usize,
    direct_import: bool,
    cut_points: Vec<JsonCutPoint>,
}

#[derive(Serialize)]
struct JsonCutPoint {
    module: String,
    exclusive_size_bytes: u64,
    chains_broken: usize,
}

#[derive(Serialize)]
struct JsonChains {
    target: String,
    chain_count: usize,
    hop_count: usize,
    chains: Vec<Vec<String>>,
}

#[derive(Serialize)]
struct JsonChainsEmpty {
    target: String,
    found_in_graph: bool,
    chain_count: usize,
    chains: Vec<Vec<String>>,
}

#[derive(Serialize)]
struct JsonTrace {
    entry: String,
    static_weight_bytes: u64,
    static_module_count: usize,
    dynamic_only_weight_bytes: u64,
    dynamic_only_module_count: usize,
    heavy_packages: Vec<JsonPackage>,
    modules_by_cost: Vec<JsonModuleCost>,
}

#[derive(Serialize)]
struct JsonPackage {
    name: String,
    total_size_bytes: u64,
    file_count: u32,
    chain: Vec<String>,
}

#[derive(Serialize)]
struct JsonModuleCost {
    path: String,
    exclusive_size_bytes: u64,
}

#[allow(clippy::cast_sign_loss)]
pub fn print_trace_json(
    graph: &ModuleGraph,
    result: &TraceResult,
    entry_path: &Path,
    root: &Path,
    top_modules: i32,
) {
    let json = JsonTrace {
        entry: relative_path(entry_path, root),
        static_weight_bytes: result.static_weight,
        static_module_count: result.static_module_count,
        dynamic_only_weight_bytes: result.dynamic_only_weight,
        dynamic_only_module_count: result.dynamic_only_module_count,
        heavy_packages: result
            .heavy_packages
            .iter()
            .map(|pkg| JsonPackage {
                name: pkg.name.clone(),
                total_size_bytes: pkg.total_size,
                file_count: pkg.file_count,
                chain: chain_display_names(graph, &pkg.chain, root),
            })
            .collect(),
        modules_by_cost: {
            let limit = if top_modules < 0 {
                result.modules_by_cost.len()
            } else {
                result.modules_by_cost.len().min(top_modules as usize)
            };
            result.modules_by_cost[..limit]
                .iter()
                .map(|mc| {
                    let m = graph.module(mc.module_id);
                    JsonModuleCost {
                        path: relative_path(&m.path, root),
                        exclusive_size_bytes: mc.exclusive_size,
                    }
                })
                .collect()
        },
    };

    println!("{}", serde_json::to_string_pretty(&json).unwrap());
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn color_enabled_when_tty_and_no_overrides() {
        assert!(should_use_color(true, false, false, false));
    }

    #[test]
    fn color_disabled_when_not_tty() {
        assert!(!should_use_color(false, false, false, false));
    }

    #[test]
    fn color_disabled_by_flag() {
        assert!(!should_use_color(true, true, false, false));
    }

    #[test]
    fn color_disabled_by_no_color_env() {
        assert!(!should_use_color(true, false, true, false));
    }

    #[test]
    fn color_disabled_by_term_dumb() {
        assert!(!should_use_color(true, false, false, true));
    }

    #[test]
    fn package_relative_path_pnpm_store() {
        // pnpm store path where workspace dir matches package name
        let path = PathBuf::from(
            "/dev/cloudflare/workers-sdk/node_modules/.pnpm/cloudflare@5.2.0/node_modules/cloudflare/index.js",
        );
        assert_eq!(
            package_relative_path(&path, "cloudflare"),
            "cloudflare/index.js"
        );
    }

    #[test]
    fn package_relative_path_scoped_pnpm() {
        let path = PathBuf::from(
            "/project/node_modules/.pnpm/@babel+parser@7.25.0/node_modules/@babel/parser/lib/index.js",
        );
        assert_eq!(
            package_relative_path(&path, "@babel/parser"),
            "@babel/parser/lib/index.js"
        );
    }

    #[test]
    fn package_relative_path_simple() {
        // Non-pnpm: straightforward node_modules/pkg/file
        let path = PathBuf::from("/project/node_modules/lodash/fp/map.js");
        assert_eq!(package_relative_path(&path, "lodash"), "lodash/fp/map.js");
    }
}
