use std::path::Path;

use serde::Serialize;

use crate::graph::{ModuleGraph, ModuleId};
use crate::query::{CutModule, DiffResult, TraceResult};

fn format_size(bytes: u64) -> String {
    if bytes >= 1_000_000 {
        format!("{:.1} MB", bytes as f64 / 1_000_000.0)
    } else if bytes >= 1_000 {
        format!("{:.0} KB", bytes as f64 / 1_000.0)
    } else {
        format!("{bytes} B")
    }
}

fn relative_path(path: &Path, root: &Path) -> String {
    path.strip_prefix(root)
        .unwrap_or(path)
        .to_string_lossy()
        .into_owned()
}

fn display_name(graph: &ModuleGraph, mid: ModuleId, root: &Path) -> String {
    let m = graph.module(mid);
    match m.package {
        Some(ref pkg) => pkg.clone(),
        None => relative_path(&m.path, root),
    }
}

pub fn print_trace(graph: &ModuleGraph, result: &TraceResult, entry_path: &Path, root: &Path, top_modules: i32) {
    println!("{}", relative_path(entry_path, root));
    println!(
        "Static transitive weight: {} ({} modules)",
        format_size(result.static_weight),
        result.static_module_count
    );
    if result.dynamic_only_module_count > 0 {
        println!(
            "Dynamic-only weight: {} ({} modules, not loaded at startup)",
            format_size(result.dynamic_only_weight),
            result.dynamic_only_module_count
        );
    }
    println!();

    if !result.heavy_packages.is_empty() {
        println!("Heavy dependencies (static):");
        for pkg in &result.heavy_packages {
            println!(
                "  {:<35} {}  {} files",
                pkg.name,
                format_size(pkg.total_size),
                pkg.file_count
            );
            if pkg.chain.len() > 1 {
                let chain_str: Vec<String> = pkg
                    .chain
                    .iter()
                    .map(|&mid| display_name(graph, mid, root))
                    .collect();
                println!("    -> {}", chain_str.join(" -> "));
            }
        }
        println!();
    }

    if top_modules != 0 && !result.modules_by_cost.is_empty() {
        println!("Modules (sorted by transitive cost):");
        let display_count = if top_modules < 0 {
            result.modules_by_cost.len()
        } else {
            result.modules_by_cost.len().min(top_modules as usize)
        };
        for mc in &result.modules_by_cost[..display_count] {
            let m = graph.module(mc.module_id);
            println!(
                "  {:<55} {}",
                relative_path(&m.path, root),
                format_size(mc.transitive_size)
            );
        }
        if result.modules_by_cost.len() > display_count {
            println!(
                "  ... and {} more modules",
                result.modules_by_cost.len() - display_count
            );
        }
    }
}

pub fn print_diff(diff: &DiffResult, entry_a: &str, entry_b: &str) {
    println!("Diff: {entry_a} vs {entry_b}");
    println!();
    println!(
        "  {:<40} {}",
        entry_a,
        format_size(diff.entry_a_weight)
    );
    println!(
        "  {:<40} {}",
        entry_b,
        format_size(diff.entry_b_weight)
    );
    let sign = if diff.weight_delta >= 0 { "+" } else { "" };
    println!(
        "  {:<40} {sign}{}",
        "Delta",
        format_size(diff.weight_delta.unsigned_abs())
    );
    println!();

    if !diff.only_in_a.is_empty() {
        println!("Only in {entry_a}:");
        for pkg in &diff.only_in_a {
            println!("  - {pkg}");
        }
    }
    if !diff.only_in_b.is_empty() {
        println!("Only in {entry_b}:");
        for pkg in &diff.only_in_b {
            println!("  + {pkg}");
        }
    }
    if !diff.shared_packages.is_empty() {
        println!(
            "Shared: {} package{}",
            diff.shared_packages.len(),
            if diff.shared_packages.len() == 1 { "" } else { "s" }
        );
    }
}

pub fn print_chains(
    graph: &ModuleGraph,
    chains: &[Vec<ModuleId>],
    package_name: &str,
    root: &Path,
    package_exists: bool,
) {
    if chains.is_empty() {
        if package_exists {
            println!("Package \"{package_name}\" exists in the graph but is not reachable from this entry point.");
        } else {
            println!("Package \"{package_name}\" is not in the dependency graph. Check the spelling or verify it's installed.");
        }
        return;
    }
    let hops = chains[0].len().saturating_sub(1);
    println!(
        "{} chain{} to \"{}\" ({} hop{}):\n",
        chains.len(),
        if chains.len() == 1 { "" } else { "s" },
        package_name,
        hops,
        if hops == 1 { "" } else { "s" },
    );
    for (i, chain) in chains.iter().enumerate() {
        let chain_str: Vec<String> = chain
            .iter()
            .map(|&mid| display_name(graph, mid, root))
            .collect();
        println!("  {}. {}", i + 1, chain_str.join(" -> "));
    }
}

pub fn print_chains_json(
    graph: &ModuleGraph,
    chains: &[Vec<ModuleId>],
    package_name: &str,
    root: &Path,
    package_exists: bool,
) {
    if chains.is_empty() {
        let json = JsonChainsEmpty {
            package: package_name.to_string(),
            found_in_graph: package_exists,
            chain_count: 0,
            chains: Vec::new(),
        };
        println!("{}", serde_json::to_string_pretty(&json).unwrap());
        return;
    }
    let json = JsonChains {
        package: package_name.to_string(),
        chain_count: chains.len(),
        hop_count: chains.first().map(|c| c.len().saturating_sub(1)).unwrap_or(0),
        chains: chains
            .iter()
            .map(|chain| {
                chain
                    .iter()
                    .map(|&mid| display_name(graph, mid, root))
                    .collect()
            })
            .collect(),
    };
    println!("{}", serde_json::to_string_pretty(&json).unwrap());
}

pub fn print_cut(
    graph: &ModuleGraph,
    cuts: &[CutModule],
    chains: &[Vec<ModuleId>],
    package_name: &str,
    root: &Path,
    package_exists: bool,
) {
    if chains.is_empty() {
        if package_exists {
            println!("Package \"{package_name}\" exists in the graph but is not reachable from this entry point.");
        } else {
            println!("Package \"{package_name}\" is not in the dependency graph. Check the spelling or verify it's installed.");
        }
        return;
    }

    if cuts.is_empty() {
        let is_direct = chains.iter().all(|c| c.len() == 2);
        if is_direct {
            println!(
                "Entry file directly imports \"{package_name}\" — remove the import to sever the dependency."
            );
        } else {
            println!(
                "No single cut point can sever all {} chain{} to \"{package_name}\".",
                chains.len(),
                if chains.len() == 1 { "" } else { "s" },
            );
            println!("Each chain takes a different path — multiple fixes needed.");
        }
        return;
    }

    println!(
        "{} cut point{} to sever all {} chain{} to \"{}\":\n",
        cuts.len(),
        if cuts.len() == 1 { "" } else { "s" },
        chains.len(),
        if chains.len() == 1 { "" } else { "s" },
        package_name,
    );
    for cut in cuts {
        println!(
            "  {:<45} {:>8}  (breaks {}/{} chains)",
            display_name(graph, cut.module_id, root),
            format_size(cut.transitive_size),
            cut.chains_broken,
            chains.len()
        );
    }
}

pub fn print_cut_json(
    graph: &ModuleGraph,
    cuts: &[CutModule],
    chains: &[Vec<ModuleId>],
    package_name: &str,
    root: &Path,
    package_exists: bool,
) {
    if chains.is_empty() {
        let json = JsonChainsEmpty {
            package: package_name.to_string(),
            found_in_graph: package_exists,
            chain_count: 0,
            chains: Vec::new(),
        };
        println!("{}", serde_json::to_string_pretty(&json).unwrap());
        return;
    }

    let json = JsonCut {
        package: package_name.to_string(),
        chain_count: chains.len(),
        direct_import: cuts.is_empty() && chains.iter().all(|c| c.len() == 2),
        cut_points: cuts
            .iter()
            .map(|c| JsonCutPoint {
                module: display_name(graph, c.module_id, root),
                transitive_size_bytes: c.transitive_size,
                chains_broken: c.chains_broken,
            })
            .collect(),
    };
    println!("{}", serde_json::to_string_pretty(&json).unwrap());
}

// JSON output types

#[derive(Serialize)]
struct JsonCut {
    package: String,
    chain_count: usize,
    direct_import: bool,
    cut_points: Vec<JsonCutPoint>,
}

#[derive(Serialize)]
struct JsonCutPoint {
    module: String,
    transitive_size_bytes: u64,
    chains_broken: usize,
}

#[derive(Serialize)]
struct JsonChains {
    package: String,
    chain_count: usize,
    hop_count: usize,
    chains: Vec<Vec<String>>,
}

#[derive(Serialize)]
struct JsonChainsEmpty {
    package: String,
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
    transitive_size_bytes: u64,
}

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
                chain: pkg
                    .chain
                    .iter()
                    .map(|&mid| display_name(graph, mid, root))
                    .collect(),
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
                        transitive_size_bytes: mc.transitive_size,
                    }
                })
                .collect()
        },
    };

    println!("{}", serde_json::to_string_pretty(&json).unwrap());
}
