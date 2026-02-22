use proptest::prelude::*;

use chainsaw::repl::Command;
use chainsaw::report::format_size;

proptest! {
    /// Command::parse must never panic, regardless of input.
    #[test]
    fn command_parse_never_panics(input in "\\PC{0,200}") {
        let _ = Command::parse(&input);
    }

    /// Command::parse with structured REPL-like input.
    #[test]
    fn command_parse_structured(
        cmd in "(trace|chain|cut|diff|packages|imports|importers|entry|set|unset|show|help|quit|info)",
        arg in "[a-zA-Z0-9_./@-]{0,50}",
        flag in "(--json|--include-dynamic|--top|--top-modules|--ignore|--no-include-dynamic|)",
        flag_val in "[a-zA-Z0-9-]{0,10}",
    ) {
        let line = format!("{cmd} {arg} {flag} {flag_val}");
        let _ = Command::parse(&line);
    }

    /// format_size produces non-empty output for any u64.
    #[test]
    fn format_size_never_empty(n: u64) {
        let s = format_size(n);
        prop_assert!(!s.is_empty());
    }

    /// format_size never panics for any u64.
    #[test]
    fn format_size_no_panic(n in 0u64..u64::MAX) {
        let _ = format_size(n);
    }
}
