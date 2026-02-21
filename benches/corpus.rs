use std::fmt::Write as _;
use std::fs;
use std::path::{Path, PathBuf};

use chainsaw::graph::EdgeKind;

const CORPUS_VERSION: u32 = 1;

struct Rng(u64);

impl Rng {
    const fn new(seed: u64) -> Self {
        Self(seed)
    }

    const fn next_u64(&mut self) -> u64 {
        self.0 = self
            .0
            .wrapping_mul(6_364_136_223_846_793_005)
            .wrapping_add(1_442_695_040_888_963_407);
        self.0
    }

    const fn next_usize(&mut self, max: usize) -> usize {
        #[allow(clippy::cast_possible_truncation)]
        let result = (self.next_u64() % max as u64) as usize;
        result
    }

    #[allow(clippy::cast_precision_loss)]
    fn next_f64(&mut self) -> f64 {
        (self.next_u64() >> 11) as f64 / (1u64 << 53) as f64
    }

    /// Average of two random draws from `low..=high` (triangular-ish distribution).
    const fn triangular(&mut self, low: usize, high: usize) -> usize {
        let span = high - low + 1;
        let a = low + self.next_usize(span);
        let b = low + self.next_usize(span);
        a.midpoint(b)
    }
}

fn corpus_root() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR")).join("target/bench-corpus")
}

fn is_cached(sub: &str) -> bool {
    let version_file = corpus_root().join(sub).join(".version");
    fs::read_to_string(&version_file)
        .is_ok_and(|content| content.trim().parse::<u32>().ok() == Some(CORPUS_VERSION))
}

fn mark_cached(sub: &str) {
    let dir = corpus_root().join(sub);
    fs::create_dir_all(&dir).expect("failed to create corpus directory");
    fs::write(dir.join(".version"), CORPUS_VERSION.to_string())
        .expect("failed to write corpus version file");
}

// --- Shared helpers ---

fn split_dir(dir: &str) -> Vec<&str> {
    if dir.is_empty() {
        Vec::new()
    } else {
        dir.split('/').collect()
    }
}

/// Pick a weighted random import count: 2-5, avg ~3.3.
const fn pick_import_count(rng: &mut Rng) -> usize {
    match rng.next_usize(10) {
        0 => 2,
        1..=4 => 3,
        5..=7 => 4,
        _ => 5,
    }
}

/// Pick a random target from `candidates` that is not `self_idx`, with fallback.
fn pick_non_self(rng: &mut Rng, candidates: &[usize], self_idx: usize, total: usize) -> usize {
    if candidates.is_empty() || (candidates.len() == 1 && candidates[0] == self_idx) {
        let mut t = rng.next_usize(total);
        if t == self_idx {
            t = (t + 1) % total;
        }
        return t;
    }
    let mut t = candidates[rng.next_usize(candidates.len())];
    if t == self_idx {
        t = candidates[(rng.next_usize(candidates.len().max(1)) + 1) % candidates.len()];
    }
    if t == self_idx {
        (self_idx + 1) % total
    } else {
        t
    }
}

/// Generate padding lines to reach `target_bytes`. Each line uses `template` with {i} and {m}.
fn fill_padding(
    buf: &mut String,
    module_idx: usize,
    target_bytes: usize,
    approx_line_bytes: usize,
    fmt_line: fn(usize, usize) -> String,
) {
    if buf.len() >= target_bytes {
        return;
    }
    let needed = target_bytes - buf.len();
    let approx_lines = needed / approx_line_bytes + 1;
    for i in 0..approx_lines {
        buf.push_str(&fmt_line(module_idx, i));
        if buf.len() >= target_bytes {
            break;
        }
    }
}

fn ts_padding_line(module_idx: usize, i: usize) -> String {
    format!("export const CONFIG_{i} = \"value_{module_idx}_{i}\";\n")
}

fn py_padding_line(module_idx: usize, i: usize) -> String {
    format!("CONFIG_{i} = \"value_{module_idx}_{i}\"\n")
}

fn join_subdir(base: &Path, dir: &str, filename: String) -> PathBuf {
    if dir.is_empty() {
        base.join(filename)
    } else {
        base.join(dir).join(filename)
    }
}

// --- TS corpus constants ---

const TS_MODULE_COUNT: usize = 3000;
const TS_SPINE_DEPTH: usize = 15;
const TS_HUB_COUNT: usize = 100;
const TS_PKG_COUNT: usize = 20;
const TS_PHANTOM_COUNT: usize = 50;
const TS_PKG_WITH_INTERNALS: usize = 5;
const TS_SEED: u64 = 0xC4A1_5AED;

/// Directory paths relative to src/ for distributing modules.
const TS_DIRS: &[&str] = &[
    "core",
    "core/auth",
    "core/data",
    "core/utils",
    "api",
    "api/handlers",
    "api/middleware",
    "ui",
    "ui/components",
    "ui/hooks",
    "ui/pages",
    "services",
    "services/cache",
    "services/queue",
    "config",
    "config/env",
    "types",
    "workers",
    "workers/jobs",
    "db",
    "db/models",
    "db/migrations",
    "shared",
    "shared/constants",
    "shared/helpers",
    "routes",
    "routes/v1",
    "routes/v2",
];

/// Metadata for a generated module: its directory and BFS level.
struct TsModule {
    dir_idx: usize,
    bfs_level: usize,
}

/// An edge in the TS corpus dependency graph.
struct TsEdge {
    from: usize,
    to: Option<usize>, // None for bare specifier edges
    kind: EdgeKind,
    bare_specifier: Option<String>,
}

/// Compute the relative import path from `from_dir` to `to_dir/to_name` (no extension).
fn relative_specifier(from_dir: &str, to_dir: &str, to_name: &str) -> String {
    let from_parts = split_dir(from_dir);
    let to_parts = split_dir(to_dir);

    let common = from_parts
        .iter()
        .zip(to_parts.iter())
        .take_while(|(a, b)| a == b)
        .count();

    let ups = from_parts.len() - common;
    let mut rel = String::new();
    if ups == 0 {
        rel.push_str("./");
    } else {
        for _ in 0..ups {
            rel.push_str("../");
        }
    }
    for part in &to_parts[common..] {
        rel.push_str(part);
        rel.push('/');
    }
    rel.push_str(to_name);
    rel
}

fn ts_module_name(idx: usize) -> String {
    if idx == 0 {
        "index".to_string()
    } else {
        format!("gen_{idx:04}")
    }
}

fn ts_module_dir(modules: &[TsModule], idx: usize) -> &'static str {
    if idx == 0 || modules[idx].dir_idx == usize::MAX {
        "" // src/ root
    } else {
        TS_DIRS[modules[idx].dir_idx]
    }
}

/// Ensure each hub module has at least 10 importers by adding extra static edges.
fn boost_hub_fan_in(
    rng: &mut Rng,
    edges: &mut Vec<TsEdge>,
    modules: &[TsModule],
    by_level: &[Vec<usize>],
    hubs: &[usize],
    hub_set: &[bool],
) {
    let mut hub_fan_in = vec![0usize; TS_MODULE_COUNT];
    for e in edges.iter() {
        if let Some(to) = e.to
            && hub_set[to]
        {
            hub_fan_in[to] += 1;
        }
    }
    for &hub_idx in hubs {
        while hub_fan_in[hub_idx] < 10 {
            let hub_level = modules[hub_idx].bfs_level;
            let max_level = hub_level.saturating_sub(1).max(1);
            let source_level = rng.next_usize(max_level + 1);
            let candidates = &by_level[source_level];
            if candidates.is_empty() {
                break;
            }
            let source = candidates[rng.next_usize(candidates.len())];
            if source == hub_idx {
                continue;
            }
            edges.push(TsEdge {
                from: source,
                to: Some(hub_idx),
                kind: EdgeKind::Static,
                bare_specifier: None,
            });
            hub_fan_in[hub_idx] += 1;
        }
    }
}

/// Generate edges for the TS corpus: spine chain, reachability, random imports,
/// bare specifiers, and hub fan-in boosting.
fn generate_ts_edges(
    rng: &mut Rng,
    modules: &[TsModule],
    by_level: &[Vec<usize>],
    hubs: &[usize],
    hub_set: &[bool],
) -> Vec<TsEdge> {
    let pkg_names: Vec<String> = (0..TS_PKG_COUNT).map(|i| format!("pkg-{i:02}")).collect();
    let phantom_names: Vec<String> = (0..TS_PHANTOM_COUNT)
        .map(|i| format!("phantom-{i:02}"))
        .collect();

    let mut edges: Vec<TsEdge> = Vec::with_capacity(TS_MODULE_COUNT * 4);

    // Spine chain: module 0 -> 1 -> 2 -> ... -> 15
    for i in 0..TS_SPINE_DEPTH {
        edges.push(TsEdge {
            from: i,
            to: Some(i + 1),
            kind: EdgeKind::Static,
            bare_specifier: None,
        });
    }

    // Reachability guarantee: every non-spine module at level N gets at least
    // one incoming static edge from a random module at level N-1.
    for level in 1..=TS_SPINE_DEPTH {
        let parents = &by_level[level - 1];
        if parents.is_empty() {
            continue;
        }
        for &child in &by_level[level] {
            if child <= TS_SPINE_DEPTH {
                continue;
            }
            let parent = parents[rng.next_usize(parents.len())];
            edges.push(TsEdge {
                from: parent,
                to: Some(child),
                kind: EdgeKind::Static,
                bare_specifier: None,
            });
        }
    }

    // Track how many edges each module already has (from spine + reachability)
    let mut edge_count = vec![0usize; TS_MODULE_COUNT];
    for e in &edges {
        edge_count[e.from] += 1;
    }

    // For each module, add random edges up to the target import count (2-5, avg ~3.3)
    for from_idx in 0..TS_MODULE_COUNT {
        let from_level = modules[from_idx].bfs_level;
        let new_edge_count = pick_import_count(rng).saturating_sub(edge_count[from_idx]);

        for _ in 0..new_edge_count {
            // Edge kind: 85% Static, 10% TypeOnly, 5% Dynamic
            let kind_roll = rng.next_f64();
            let kind = if kind_roll < 0.85 {
                EdgeKind::Static
            } else if kind_roll < 0.95 {
                EdgeKind::TypeOnly
            } else {
                EdgeKind::Dynamic
            };

            // Specifier type: 12% bare, 88% relative
            if rng.next_f64() < 0.12 {
                // 20/(20+50) = ~29% chance of existing package, ~71% phantom
                let bare_name = if rng.next_f64() < 0.29 {
                    pkg_names[rng.next_usize(TS_PKG_COUNT)].clone()
                } else {
                    phantom_names[rng.next_usize(TS_PHANTOM_COUNT)].clone()
                };
                edges.push(TsEdge {
                    from: from_idx,
                    to: None,
                    kind,
                    bare_specifier: Some(bare_name),
                });
            } else {
                // Relative import: 30% chance to target a hub module
                let target = if !hubs.is_empty() && rng.next_f64() < 0.30 {
                    hubs[rng.next_usize(hubs.len())]
                } else {
                    let min_level = from_level.saturating_sub(1);
                    let max_level = (from_level + 3).min(TS_SPINE_DEPTH);
                    let target_level = min_level + rng.next_usize(max_level - min_level + 1);
                    pick_non_self(rng, &by_level[target_level], from_idx, TS_MODULE_COUNT)
                };
                edges.push(TsEdge {
                    from: from_idx,
                    to: Some(target),
                    kind,
                    bare_specifier: None,
                });
            }
        }
    }

    boost_hub_fan_in(rng, &mut edges, modules, by_level, hubs, hub_set);

    edges
}

/// Write all TS module source files under `src/`.
fn write_ts_module_files(rng: &mut Rng, src: &Path, modules: &[TsModule], edges: &[TsEdge]) {
    let mut edges_by_module: Vec<Vec<&TsEdge>> = vec![Vec::new(); TS_MODULE_COUNT];
    for edge in edges {
        edges_by_module[edge.from].push(edge);
    }

    for (idx, my_edges) in edges_by_module.iter().enumerate() {
        let dir = ts_module_dir(modules, idx);
        let name = ts_module_name(idx);
        let file_path = join_subdir(src, dir, format!("{name}.ts"));

        let target_bytes = rng.triangular(500, 4000);
        let mut content = String::with_capacity(target_bytes + 256);

        // Write import statements
        for edge in my_edges {
            let specifier = if let Some(ref bare) = edge.bare_specifier {
                bare.clone()
            } else if let Some(to) = edge.to {
                relative_specifier(dir, ts_module_dir(modules, to), &ts_module_name(to))
            } else {
                continue;
            };

            let binding_name = specifier
                .replace(['/', '-', '.', '@'], "_")
                .trim_start_matches('_')
                .to_string();

            match edge.kind {
                EdgeKind::Static => {
                    let _ = writeln!(content, "import {{ {binding_name} }} from \"{specifier}\";");
                }
                EdgeKind::TypeOnly => {
                    let _ = writeln!(
                        content,
                        "import type {{ {binding_name} }} from \"{specifier}\";"
                    );
                }
                EdgeKind::Dynamic => {
                    let _ = writeln!(
                        content,
                        "const _{binding_name} = await import(\"{specifier}\");"
                    );
                }
                _ => unreachable!("corpus generator only produces Static/TypeOnly/Dynamic edges"),
            }
        }

        let _ = write!(
            content,
            "\nexport interface State_{idx} {{\n    value: string;\n    count: number;\n}}\n\n"
        );
        let _ = write!(
            content,
            "export function process_{idx}(input: State_{idx}): State_{idx} {{\n    return {{ ...input, count: input.count + 1 }};\n}}\n\n"
        );

        fill_padding(&mut content, idx, target_bytes, 48, ts_padding_line);

        fs::write(&file_path, &content)
            .unwrap_or_else(|e| panic!("failed to write {}: {e}", file_path.display()));
    }
}

/// Generate `node_modules/` packages with optional internal submodules.
fn write_ts_packages(rng: &mut Rng, nm: &Path) {
    let pkg_names: Vec<String> = (0..TS_PKG_COUNT).map(|i| format!("pkg-{i:02}")).collect();

    for (i, pkg_name) in pkg_names.iter().enumerate() {
        let pkg_dir = nm.join(pkg_name);
        fs::create_dir_all(&pkg_dir).expect("failed to create package dir");

        fs::write(
            pkg_dir.join("package.json"),
            format!("{{\"name\":\"{pkg_name}\",\"main\":\"index.ts\"}}"),
        )
        .expect("failed to write package.json");

        let mut index_content = String::new();
        if i < TS_PKG_WITH_INTERNALS {
            let internal_count = 3 + rng.next_usize(8);
            for j in 0..internal_count {
                let _ = writeln!(
                    index_content,
                    "export {{ internal_{j} }} from \"./internal_{j}\";"
                );
                let internal_content = format!(
                    "export function internal_{j}() {{ return \"{pkg_name}/internal_{j}\"; }}\n\
                     export const DATA_{j} = {{ name: \"{pkg_name}\", index: {j} }};\n"
                );
                fs::write(pkg_dir.join(format!("internal_{j}.ts")), internal_content)
                    .expect("failed to write internal file");
            }
        }
        let _ = writeln!(
            index_content,
            "export function init_{i}() {{ return \"{pkg_name}\"; }}"
        );
        fs::write(pkg_dir.join("index.ts"), index_content)
            .expect("failed to write package index.ts");
    }
}

fn generate_ts_corpus(root: &Path) {
    let mut rng = Rng::new(TS_SEED);

    // --- 1. Create directory structure ---
    let src = root.join("src");
    fs::create_dir_all(&src).expect("failed to create src/");
    for dir in TS_DIRS {
        fs::create_dir_all(src.join(dir)).expect("failed to create src subdir");
    }
    let nm = root.join("node_modules");
    fs::create_dir_all(&nm).expect("failed to create node_modules/");

    // --- 2. Assign modules to directories and BFS levels ---
    let mut modules: Vec<TsModule> = Vec::with_capacity(TS_MODULE_COUNT);

    // Module 0 = entry point (src/index.ts), BFS level 0, dir = "" (src root)
    modules.push(TsModule {
        dir_idx: usize::MAX, // sentinel: lives in src/ root
        bfs_level: 0,
    });

    // Modules 1..=SPINE_DEPTH form the spine chain (BFS levels 1..=15)
    for level in 1..=TS_SPINE_DEPTH {
        modules.push(TsModule {
            dir_idx: rng.next_usize(TS_DIRS.len()),
            bfs_level: level,
        });
    }

    // Remaining modules distributed across BFS levels 1..=14
    for _ in (TS_SPINE_DEPTH + 1)..TS_MODULE_COUNT {
        let level = rng.triangular(1, TS_SPINE_DEPTH - 1);
        modules.push(TsModule {
            dir_idx: rng.next_usize(TS_DIRS.len()),
            bfs_level: level,
        });
    }

    // --- 3. Designate hub modules ---
    let hub_candidates: Vec<usize> = (0..TS_MODULE_COUNT)
        .filter(|&i| (3..=8).contains(&modules[i].bfs_level))
        .collect();
    let mut hubs: Vec<usize> = Vec::with_capacity(TS_HUB_COUNT);
    let mut hub_set = vec![false; TS_MODULE_COUNT];
    {
        let mut candidates = hub_candidates;
        // Fisher-Yates partial shuffle
        let pick_count = TS_HUB_COUNT.min(candidates.len());
        for i in 0..pick_count {
            let j = i + rng.next_usize(candidates.len() - i);
            candidates.swap(i, j);
        }
        for &idx in &candidates[..pick_count] {
            hubs.push(idx);
            hub_set[idx] = true;
        }
    }

    // --- 4. Build index of modules by BFS level ---
    let mut by_level: Vec<Vec<usize>> = vec![Vec::new(); TS_SPINE_DEPTH + 1];
    for (i, m) in modules.iter().enumerate() {
        by_level[m.bfs_level].push(i);
    }

    // --- 5. Generate edges ---
    let edges = generate_ts_edges(&mut rng, &modules, &by_level, &hubs, &hub_set);

    // --- 6. Write module files ---
    write_ts_module_files(&mut rng, &src, &modules, &edges);

    // --- 7. Generate node_modules packages ---
    write_ts_packages(&mut rng, &nm);

    // --- 8. Root package.json ---
    fs::write(
        root.join("package.json"),
        "{\"name\":\"bench-corpus\",\"private\":true}",
    )
    .expect("failed to write root package.json");
}

// --- PY corpus constants ---

const PY_MODULE_COUNT: usize = 600;
const PY_SPINE_DEPTH: usize = 10;
const PY_SEED: u64 = 0xB07B_0000;
const PY_MEDIAN_SIZE: usize = 1000;

/// Directory paths relative to app/ for distributing Python modules.
const PY_DIRS: &[&str] = &[
    "core",
    "core/auth",
    "core/data",
    "api",
    "api/handlers",
    "models",
    "models/base",
    "services",
    "services/cache",
    "utils",
    "utils/helpers",
    "config",
    "handlers",
    "handlers/events",
    "middleware",
];

/// Metadata for a generated Python module: its directory index and BFS level.
struct PyModule {
    dir_idx: usize,
    bfs_level: usize,
}

/// An edge in the PY corpus dependency graph.
struct PyEdge {
    from: usize,
    to: usize,
}

/// Compute a Python relative import specifier from `from_dir` to `to_dir/to_name`.
/// Both dirs are relative to `app/`. Returns specifiers like `.gen_042` or `..models.gen_042`.
fn py_relative_specifier(from_dir: &str, to_dir: &str, to_name: &str) -> String {
    let from_parts = split_dir(from_dir);
    let to_parts = split_dir(to_dir);

    let common = from_parts
        .iter()
        .zip(to_parts.iter())
        .take_while(|(a, b)| a == b)
        .count();

    let ups = from_parts.len() - common;

    // Number of dots: 1 for same directory, 1 + ups for parent traversal
    let mut spec = ".".repeat(ups + 1);

    for part in &to_parts[common..] {
        spec.push_str(part);
        spec.push('.');
    }
    spec.push_str(to_name);
    spec
}

/// Compute a Python absolute import specifier for a module at `to_dir/to_name`
/// under the `app` package. Returns specifiers like `app.core.gen_042`.
fn py_absolute_specifier(to_dir: &str, to_name: &str) -> String {
    let mut spec = String::from("app");
    if !to_dir.is_empty() {
        for part in to_dir.split('/') {
            spec.push('.');
            spec.push_str(part);
        }
    }
    spec.push('.');
    spec.push_str(to_name);
    spec
}

fn py_module_name(idx: usize) -> String {
    if idx == 0 {
        "__init__".to_string()
    } else {
        format!("gen_{idx:04}")
    }
}

fn py_module_dir(modules: &[PyModule], idx: usize) -> &'static str {
    if idx == 0 || modules[idx].dir_idx == usize::MAX {
        "" // app/ root
    } else {
        PY_DIRS[modules[idx].dir_idx]
    }
}

/// Generate edges for the PY corpus: spine chain, reachability, random imports.
fn generate_py_edges(rng: &mut Rng, modules: &[PyModule], by_level: &[Vec<usize>]) -> Vec<PyEdge> {
    let mut edges: Vec<PyEdge> = Vec::with_capacity(PY_MODULE_COUNT * 4);

    // Spine chain: module 0 -> 1 -> 2 -> ... -> 10
    for i in 0..PY_SPINE_DEPTH {
        edges.push(PyEdge { from: i, to: i + 1 });
    }

    // Reachability guarantee
    for level in 1..=PY_SPINE_DEPTH {
        let parents = &by_level[level - 1];
        if parents.is_empty() {
            continue;
        }
        for &child in &by_level[level] {
            if child <= PY_SPINE_DEPTH {
                continue;
            }
            let parent = parents[rng.next_usize(parents.len())];
            edges.push(PyEdge {
                from: parent,
                to: child,
            });
        }
    }

    // Track how many edges each module already has
    let mut edge_count = vec![0usize; PY_MODULE_COUNT];
    for e in &edges {
        edge_count[e.from] += 1;
    }

    // For each module, add random edges up to target count (2-5, avg ~3.3)
    for from_idx in 0..PY_MODULE_COUNT {
        let from_level = modules[from_idx].bfs_level;
        let new_edge_count = pick_import_count(rng).saturating_sub(edge_count[from_idx]);

        for _ in 0..new_edge_count {
            let min_level = from_level.saturating_sub(1);
            let max_level = (from_level + 3).min(PY_SPINE_DEPTH);
            let target_level = min_level + rng.next_usize(max_level - min_level + 1);
            let target = pick_non_self(rng, &by_level[target_level], from_idx, PY_MODULE_COUNT);
            edges.push(PyEdge {
                from: from_idx,
                to: target,
            });
        }
    }

    edges
}

/// Write all PY module source files under `app/`.
fn write_py_module_files(rng: &mut Rng, app: &Path, modules: &[PyModule], edges: &[PyEdge]) {
    let mut edges_by_module: Vec<Vec<&PyEdge>> = vec![Vec::new(); PY_MODULE_COUNT];
    for edge in edges {
        edges_by_module[edge.from].push(edge);
    }

    for (idx, my_edges) in edges_by_module.iter().enumerate() {
        let dir = py_module_dir(modules, idx);
        let name = py_module_name(idx);
        let file_path = join_subdir(app, dir, format!("{name}.py"));

        let target_bytes = rng.triangular(PY_MEDIAN_SIZE.saturating_sub(600), PY_MEDIAN_SIZE + 600);
        let mut content = String::with_capacity(target_bytes + 256);

        // Write import statements
        for edge in my_edges {
            if edge.to == 0 {
                content.push_str("from app import __init__ as _app_init\n");
                continue;
            }

            let to_dir = py_module_dir(modules, edge.to);
            let to_name = py_module_name(edge.to);

            let binding = if to_name == "__init__" {
                format!("init_{}", edge.to)
            } else {
                format!("process_{}", edge.to)
            };

            // Decide between relative (60%) and absolute (40%) imports
            let specifier = if rng.next_f64() < 0.60 {
                py_relative_specifier(dir, to_dir, &to_name)
            } else {
                py_absolute_specifier(to_dir, &to_name)
            };

            let _ = writeln!(content, "from {specifier} import {binding}");
        }

        let _ = write!(
            content,
            "\n\nclass State_{idx}:\n    \
             def __init__(self, value: str, count: int) -> None:\n        \
             self.value = value\n        \
             self.count = count\n\n    \
             def process(self) -> \"State_{idx}\":\n        \
             return State_{idx}(self.value, self.count + 1)\n\n"
        );

        let _ = writeln!(
            content,
            "def process_{idx}(x: int) -> int:\n    return x + 1\n"
        );

        fill_padding(&mut content, idx, target_bytes, 35, py_padding_line);

        fs::write(&file_path, &content)
            .unwrap_or_else(|e| panic!("failed to write {}: {e}", file_path.display()));
    }
}

fn generate_py_corpus(root: &Path) {
    let mut rng = Rng::new(PY_SEED);

    // --- 1. Create directory structure ---
    let app = root.join("app");
    fs::create_dir_all(&app).expect("failed to create app/");
    for dir in PY_DIRS {
        fs::create_dir_all(app.join(dir)).expect("failed to create app subdir");
    }

    // --- 2. Write pyproject.toml ---
    fs::write(
        root.join("pyproject.toml"),
        "[project]\nname = \"bench-corpus\"\nversion = \"0.1.0\"\n",
    )
    .expect("failed to write pyproject.toml");

    // --- 3. Assign modules to directories and BFS levels ---
    let mut modules: Vec<PyModule> = Vec::with_capacity(PY_MODULE_COUNT);

    // Module 0 = entry point (app/__init__.py), BFS level 0, dir = "" (app root)
    modules.push(PyModule {
        dir_idx: usize::MAX, // sentinel: lives in app/ root
        bfs_level: 0,
    });

    // Modules 1..=SPINE_DEPTH form the spine chain (BFS levels 1..=10)
    for level in 1..=PY_SPINE_DEPTH {
        modules.push(PyModule {
            dir_idx: rng.next_usize(PY_DIRS.len()),
            bfs_level: level,
        });
    }

    // Remaining modules distributed across BFS levels 1..=9
    for _ in (PY_SPINE_DEPTH + 1)..PY_MODULE_COUNT {
        let level = rng.triangular(1, PY_SPINE_DEPTH - 1);
        modules.push(PyModule {
            dir_idx: rng.next_usize(PY_DIRS.len()),
            bfs_level: level,
        });
    }

    // --- 4. Build index of modules by BFS level ---
    let mut by_level: Vec<Vec<usize>> = vec![Vec::new(); PY_SPINE_DEPTH + 1];
    for (i, m) in modules.iter().enumerate() {
        by_level[m.bfs_level].push(i);
    }

    // --- 5. Generate edges ---
    let edges = generate_py_edges(&mut rng, &modules, &by_level);

    // --- 6. Write __init__.py for every directory ---
    for dir in PY_DIRS {
        let parts: Vec<&str> = dir.split('/').collect();
        for depth in 0..parts.len() {
            let sub: String = parts[..=depth].join("/");
            let init_path = app.join(&sub).join("__init__.py");
            if !init_path.exists() {
                fs::write(&init_path, "").expect("failed to write __init__.py");
            }
        }
    }

    // --- 7. Write module files ---
    write_py_module_files(&mut rng, &app, &modules, &edges);
}

pub fn ts_corpus() -> (PathBuf, PathBuf) {
    let root = corpus_root().join("ts");
    if !is_cached("ts") {
        generate_ts_corpus(&root);
        mark_cached("ts");
    }
    let entry = root.join("src/index.ts");
    (root, entry)
}

pub fn py_corpus() -> (PathBuf, PathBuf) {
    let root = corpus_root().join("py");
    if !is_cached("py") {
        generate_py_corpus(&root);
        mark_cached("py");
    }
    let entry = root.join("app/__init__.py");
    (root, entry)
}
