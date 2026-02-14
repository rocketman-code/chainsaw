use criterion::{black_box, criterion_group, criterion_main, Criterion};
use std::path::PathBuf;

use chainsaw::cache::ParseCache;
use chainsaw::lang::python::PythonSupport;
use chainsaw::lang::typescript::TypeScriptSupport;
use chainsaw::lang::LanguageSupport;

fn ts_root() -> PathBuf {
    std::env::var("TS_BENCH_ROOT")
        .map(PathBuf::from)
        .unwrap_or_else(|_| PathBuf::from("/tmp/openclaw-fresh"))
}

fn py_root() -> PathBuf {
    std::env::var("PY_BENCH_ROOT")
        .map(PathBuf::from)
        .unwrap_or_else(|_| PathBuf::from("/Users/hlal/dev/aws/aws-cli"))
}

fn bench_ts_parse_file(c: &mut Criterion) {
    let root = ts_root();
    let entry = root.join("packages/web-app/src/index.tsx");
    if !entry.exists() {
        eprintln!("Skipping: {} not found", root.display());
        return;
    }
    let lang = TypeScriptSupport::new(&root);
    c.bench_function("ts_parse_file", |b| {
        b.iter(|| lang.parse(black_box(&entry)))
    });
}

fn bench_py_parse_file(c: &mut Criterion) {
    let root = py_root();
    let entry = root.join("awscli/__init__.py");
    if !entry.exists() {
        eprintln!("Skipping: {} not found", root.display());
        return;
    }
    let lang = PythonSupport::new(&root);
    c.bench_function("py_parse_file", |b| {
        b.iter(|| lang.parse(black_box(&entry)))
    });
}

fn bench_ts_resolve(c: &mut Criterion) {
    let root = ts_root();
    if !root.join("package.json").exists() {
        eprintln!("Skipping: {} not found", root.display());
        return;
    }
    let lang = TypeScriptSupport::new(&root);
    let from_dir = root.join("packages/web-app/src");
    c.bench_function("ts_resolve", |b| {
        b.iter(|| lang.resolve(black_box(&from_dir), black_box("./index")))
    });
}

fn bench_py_resolve(c: &mut Criterion) {
    let root = py_root();
    if !root.join("pyproject.toml").exists() && !root.join("setup.py").exists() {
        eprintln!("Skipping: {} not found", root.display());
        return;
    }
    let lang = PythonSupport::new(&root);
    c.bench_function("py_resolve", |b| {
        b.iter(|| lang.resolve(black_box(&root), black_box("awscli")))
    });
}

fn bench_build_graph_ts(c: &mut Criterion) {
    let root = ts_root();
    let entry = root.join("packages/web-app/src/index.tsx");
    if !entry.exists() {
        eprintln!("Skipping: {} not found", root.display());
        return;
    }
    let lang = TypeScriptSupport::new(&root);
    let mut group = c.benchmark_group("build_graph");
    group.sample_size(10);
    group.bench_function("ts_cold", |b| {
        b.iter(|| {
            let mut cache = ParseCache::new();
            chainsaw::walker::build_graph(black_box(&entry), black_box(&root), &lang, &mut cache)
        })
    });
    group.finish();
}

fn bench_build_graph_py(c: &mut Criterion) {
    let root = py_root();
    let entry = root.join("awscli/__init__.py");
    if !entry.exists() {
        eprintln!("Skipping: {} not found", root.display());
        return;
    }
    let lang = PythonSupport::new(&root);
    let mut group = c.benchmark_group("build_graph");
    group.sample_size(10);
    group.bench_function("py_cold", |b| {
        b.iter(|| {
            let mut cache = ParseCache::new();
            chainsaw::walker::build_graph(black_box(&entry), black_box(&root), &lang, &mut cache)
        })
    });
    group.finish();
}

fn bench_cache_load_validate(c: &mut Criterion) {
    let root = ts_root();
    let entry = root.join("packages/web-app/src/index.tsx");
    if !entry.exists() {
        eprintln!("Skipping: {} not found", root.display());
        return;
    }
    let lang = TypeScriptSupport::new(&root);
    let mut cache = ParseCache::new();
    let result = chainsaw::walker::build_graph(&entry, &root, &lang, &mut cache);
    let unresolvable_count: usize = result.unresolvable_dynamic.iter().map(|(_, c)| c).sum();
    cache.save(&root, &entry, &result.graph, result.unresolved_specifiers, unresolvable_count);

    c.bench_function("cache_load_validate_ts", |b| {
        b.iter(|| {
            let loaded = ParseCache::load(black_box(&root));
            let resolve_fn = |_: &str| false;
            loaded.try_load_graph(black_box(&entry), &resolve_fn)
        })
    });
}

criterion_group!(
    benches,
    bench_ts_parse_file,
    bench_py_parse_file,
    bench_ts_resolve,
    bench_py_resolve,
    bench_build_graph_ts,
    bench_build_graph_py,
    bench_cache_load_validate,
);
criterion_main!(benches);
