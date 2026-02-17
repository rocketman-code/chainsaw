// Standalone test for corpus generation.
// This is a separate bench target with harness = true, so #[test] functions work.

mod corpus;

#[test]
fn ts_corpus_builds_successfully() {
    let (root, entry) = corpus::ts_corpus();
    assert!(entry.exists(), "entry point should exist");

    let lang = chainsaw::lang::typescript::TypeScriptSupport::new(&root);
    let mut cache = chainsaw::cache::ParseCache::new();
    let result = chainsaw::walker::build_graph(&entry, &root, &lang, &mut cache);

    let g = &result.graph;
    eprintln!(
        "TS corpus: {} modules, {} edges, {} packages",
        g.module_count(),
        g.edges.len(),
        g.package_map.len()
    );
    eprintln!(
        "  unresolved specifiers: {}",
        result.unresolved_specifiers.len()
    );

    // Bounds derived from spec: 3000 modules, ~10K edges, 20 packages
    assert!(
        g.module_count() >= 2900 && g.module_count() <= 3200,
        "expected 2900-3200 modules, got {}",
        g.module_count()
    );
    assert!(
        g.edges.len() >= 9000,
        "expected 9000+ edges, got {}",
        g.edges.len()
    );
    assert_eq!(g.package_map.len(), 20, "expected exactly 20 packages");
}

#[test]
fn py_corpus_builds_successfully() {
    let (root, entry) = corpus::py_corpus();
    assert!(entry.exists(), "entry point should exist");

    let lang = chainsaw::lang::python::PythonSupport::new(&root);
    let mut cache = chainsaw::cache::ParseCache::new();
    let result = chainsaw::walker::build_graph(&entry, &root, &lang, &mut cache);

    let g = &result.graph;
    eprintln!(
        "PY corpus: {} modules, {} edges",
        g.module_count(),
        g.edges.len()
    );
    eprintln!(
        "  unresolved specifiers: {}",
        result.unresolved_specifiers.len()
    );

    // Bounds derived from spec: 600 modules, ~2K edges
    assert_eq!(
        g.module_count(),
        600,
        "expected exactly 600 modules"
    );
    assert!(
        g.edges.len() >= 1800,
        "expected 1800+ edges, got {}",
        g.edges.len()
    );
}
