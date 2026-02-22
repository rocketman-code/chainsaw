mod common;

use chainsaw::query::TraceOptions;
use chainsaw::session::Session;

#[test]
fn trace_then_diff_then_trace_produces_same_results() {
    let p = common::TestProject::new();

    // First trace: build graph (writes cache)
    let mut s1 = Session::open(&p.entry, false).unwrap();
    let r1 = s1.trace_report(&TraceOptions::default(), 20);
    let weight_before = r1.static_weight_bytes;
    let count_before = r1.static_module_count;
    drop(s1);

    // Diff: compare index.ts vs b.ts (exercises diff code path)
    let mut s2 = Session::open(&p.entry, false).unwrap();
    let b = p.root().join("b.ts");
    let _ = s2.diff_report(&b, &TraceOptions::default(), 10);
    drop(s2);

    // Second trace: should load from cache, results must match
    let mut s3 = Session::open(&p.entry, false).unwrap();
    let r3 = s3.trace_report(&TraceOptions::default(), 20);
    assert_eq!(
        r3.static_weight_bytes, weight_before,
        "cache corruption: weight changed after diff"
    );
    assert_eq!(
        r3.static_module_count, count_before,
        "cache corruption: module count changed after diff"
    );
}

/// Multiple diff/trace cycles must produce stable results from cache.
///
/// This is a stronger variant of the single-cycle test above: it runs three
/// diff operations interleaved with traces to verify that repeated diffs
/// don't cause progressive cache degradation.
#[test]
fn repeated_diff_cycles_produce_stable_trace() {
    let p = common::TestProject::new();
    let b = p.root().join("b.ts");

    // Initial trace (writes cache)
    let mut s = Session::open(&p.entry, false).unwrap();
    let baseline = s.trace_report(&TraceOptions::default(), 20);
    drop(s);

    for cycle in 0..3 {
        // Diff operation
        let mut s = Session::open(&p.entry, false).unwrap();
        let _ = s.diff_report(&b, &TraceOptions::default(), 10);
        drop(s);

        // Verify trace from cache matches baseline
        let mut s = Session::open(&p.entry, false).unwrap();
        let r = s.trace_report(&TraceOptions::default(), 20);
        assert_eq!(
            r.static_weight_bytes, baseline.static_weight_bytes,
            "cycle {cycle}: weight diverged from baseline"
        );
        assert_eq!(
            r.static_module_count, baseline.static_module_count,
            "cycle {cycle}: module count diverged from baseline"
        );
        drop(s);
    }
}
