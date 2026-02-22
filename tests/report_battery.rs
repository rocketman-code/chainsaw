mod common;

use chainsaw::query::TraceOptions;
use chainsaw::session::Session;

/// Build all report types from a single session for reuse.
struct Reports {
    _project: common::TestProject,
    trace_json: String,
    trace_terminal: String,
    chain_json: String,
    chain_terminal: String,
    cut_json: String,
    cut_terminal: String,
    diff_json: String,
    diff_terminal: String,
    packages_json: String,
    packages_terminal: String,
}

fn build_reports() -> Reports {
    let p = common::TestProject::new();
    let entry = p.entry.clone();
    let b = p.root().join("b.ts");
    let mut session = Session::open(&entry, true).unwrap();

    let trace = session.trace_report(&TraceOptions::default(), 20);
    let chain = session.chain_report("lodash", false);
    let cut = session.cut_report("lodash", 10, false);
    let diff = session
        .diff_report(&b, &TraceOptions::default(), 10)
        .unwrap();
    let packages = session.packages_report(10);

    Reports {
        _project: p,
        trace_json: trace.to_json(),
        trace_terminal: trace.to_terminal(false),
        chain_json: chain.to_json(),
        chain_terminal: chain.to_terminal(false),
        cut_json: cut.to_json(),
        cut_terminal: cut.to_terminal(false),
        diff_json: diff.to_json(),
        diff_terminal: diff.to_terminal(false),
        packages_json: packages.to_json(),
        packages_terminal: packages.to_terminal(false),
    }
}

macro_rules! report_battery {
    ($name:ident, $json_field:ident, $terminal_field:ident) => {
        mod $name {
            use super::*;

            #[test]
            fn json_is_valid() {
                let r = build_reports();
                let v: serde_json::Value = serde_json::from_str(&r.$json_field).unwrap();
                assert!(v.is_object(), "top-level should be an object");
            }

            #[test]
            fn json_has_no_null_values() {
                let r = build_reports();
                let v: serde_json::Value = serde_json::from_str(&r.$json_field).unwrap();
                for (key, val) in v.as_object().unwrap() {
                    assert!(!val.is_null(), "field '{key}' is null");
                }
            }

            #[test]
            fn terminal_is_nonempty() {
                let r = build_reports();
                assert!(!r.$terminal_field.is_empty());
            }

            #[test]
            fn terminal_no_ansi_when_color_false() {
                let r = build_reports();
                // ESC character (0x1B) starts ANSI escape sequences
                assert!(
                    !r.$terminal_field.contains('\x1b'),
                    "terminal output with color=false should have no ANSI codes"
                );
            }
        }
    };
}

report_battery!(trace, trace_json, trace_terminal);
report_battery!(chain, chain_json, chain_terminal);
report_battery!(cut, cut_json, cut_terminal);
report_battery!(diff, diff_json, diff_terminal);
report_battery!(packages, packages_json, packages_terminal);
