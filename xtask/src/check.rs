use std::process::Command;

/// Run all local CI checks: format, lint, test.
/// Returns exit code (0 = all passed).
pub fn run() -> i32 {
    let steps: &[(&str, &[&str], &str)] = &[
        (
            "Checking formatting...",
            &["cargo", "fmt", "--check"],
            "Formatting issues found. Run 'cargo fmt' to fix.",
        ),
        (
            "Running clippy...",
            &[
                "cargo",
                "clippy",
                "--workspace",
                "--all-targets",
                "--",
                "-D",
                "warnings",
            ],
            "Clippy found warnings. Fix the issues above.",
        ),
        (
            "Running tests...",
            &["cargo", "test", "--workspace"],
            "Tests failed. Fix the failing tests above.",
        ),
    ];

    // No .stdout()/.stderr() — subprocesses inherit the terminal so
    // cargo's colored output streams directly to the user.
    for (label, args, failure_msg) in steps {
        eprintln!("\n{label}");
        let status = match Command::new(args[0]).args(&args[1..]).status() {
            Ok(s) => s,
            Err(e) => {
                eprintln!("Failed to run {}: {e}", args[0]);
                return 1;
            }
        };

        if !status.success() {
            eprintln!("\nFAILED: {failure_msg}");
            return 1;
        }
    }

    eprintln!("\nAll checks passed.");
    0
}
