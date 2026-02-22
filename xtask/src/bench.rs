use std::env;
use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;
use std::time::SystemTime;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum TargetKind {
    /// All scenarios (cold, warm, edit)
    Primary,
    /// Warm only (too small for meaningful cold/edit)
    Sentinel,
}

#[derive(Debug, Clone, Copy)]
struct BenchTarget {
    name: &'static str,
    /// Entry file path with ~ for $HOME
    entry: &'static str,
    root: &'static str,
    kind: TargetKind,
}

const TARGETS: &[BenchTarget] = &[
    BenchTarget {
        name: "typeorm",
        entry: "~/dev/typeorm/typeorm/src/index.ts",
        root: "~/dev/typeorm/typeorm",
        kind: TargetKind::Primary,
    },
    BenchTarget {
        name: "excalidraw",
        entry: "~/dev/excalidraw/excalidraw/packages/excalidraw/index.tsx",
        root: "~/dev/excalidraw/excalidraw",
        kind: TargetKind::Primary,
    },
    BenchTarget {
        name: "wrangler",
        entry: "~/dev/cloudflare/workers-sdk/packages/wrangler/src/index.ts",
        root: "~/dev/cloudflare/workers-sdk",
        kind: TargetKind::Primary,
    },
    BenchTarget {
        name: "openclaw",
        entry: "~/dev/openclaw/openclaw/src/index.ts",
        root: "~/dev/openclaw/openclaw",
        kind: TargetKind::Primary,
    },
    BenchTarget {
        name: "aws-cli",
        entry: "~/dev/aws/aws-cli/awscli/__init__.py",
        root: "~/dev/aws/aws-cli",
        kind: TargetKind::Primary,
    },
    BenchTarget {
        name: "sentry-python",
        entry: "~/dev/getsentry/sentry-python/sentry_sdk/__init__.py",
        root: "~/dev/getsentry/sentry-python",
        kind: TargetKind::Sentinel,
    },
    BenchTarget {
        name: "ansible",
        entry: "~/dev/ansible/ansible/lib/ansible/__init__.py",
        root: "~/dev/ansible/ansible",
        kind: TargetKind::Sentinel,
    },
];

/// Expand ~ to $HOME in a path string.
fn expand_home(path: &str) -> PathBuf {
    if let Some(rest) = path.strip_prefix("~/") {
        let home = env::var("HOME").expect("HOME not set");
        PathBuf::from(home).join(rest)
    } else {
        PathBuf::from(path)
    }
}

/// A resolved target with expanded paths, confirmed to exist on disk.
#[derive(Debug)]
struct ResolvedTarget {
    name: &'static str,
    entry: PathBuf,
    root: PathBuf,
    kind: TargetKind,
}

/// Resolve all targets, returning (available, skipped_names).
fn resolve_targets(filter: Option<&str>) -> (Vec<ResolvedTarget>, Vec<&'static str>) {
    let mut available = Vec::new();
    let mut skipped = Vec::new();

    let targets = match filter {
        Some(name) => match TARGETS.iter().find(|t| t.name == name) {
            Some(t) => vec![*t],
            None => {
                eprintln!("Unknown target: {name}");
                eprintln!(
                    "Available: {}",
                    TARGETS
                        .iter()
                        .map(|t| t.name)
                        .collect::<Vec<_>>()
                        .join(", ")
                );
                return (vec![], vec![]);
            }
        },
        None => TARGETS.to_vec(),
    };

    for t in &targets {
        let entry = expand_home(t.entry);
        let root = expand_home(t.root);
        if entry.exists() {
            available.push(ResolvedTarget {
                name: t.name,
                entry,
                root,
                kind: t.kind,
            });
        } else {
            skipped.push(t.name);
        }
    }

    (available, skipped)
}

/// Print the target availability summary.
fn print_target_summary(available: &[ResolvedTarget], skipped: &[&str]) {
    eprintln!("Benchmark targets:");
    for t in available {
        eprintln!("  [ok]   {:<16} {}", t.name, t.root.display());
    }
    for name in skipped {
        let target = TARGETS.iter().find(|t| t.name == *name).unwrap();
        eprintln!("  [skip] {:<16} not found: {}", name, target.entry);
    }
    if !skipped.is_empty() {
        eprintln!(
            "\nRunning {} of {} targets ({} skipped)",
            available.len(),
            available.len() + skipped.len(),
            skipped.len()
        );
    }
    eprintln!();
}

// --- Scenario runner ---

#[derive(Debug, Clone, Copy, PartialEq, Eq, clap::ValueEnum)]
pub enum Scenario {
    Cold,
    Warm,
    Edit,
}

impl Scenario {
    fn label(self) -> &'static str {
        match self {
            Self::Cold => "cold",
            Self::Warm => "warm",
            Self::Edit => "edit",
        }
    }

    fn warmup(self) -> u32 {
        match self {
            Self::Cold => 1,
            Self::Warm => 5,
            Self::Edit => 3,
        }
    }

    fn min_runs(self) -> u32 {
        match self {
            Self::Cold => 30,
            Self::Warm => 50,
            Self::Edit => 30,
        }
    }

    /// The --prepare command for hyperfine, if any.
    fn prepare(self, target: &ResolvedTarget) -> Option<String> {
        match self {
            Self::Cold => Some(format!("rm -f {}/.chainsaw.cache", target.root.display())),
            Self::Warm => None,
            Self::Edit => Some(format!("touch {}", target.entry.display())),
        }
    }

    /// Scenarios applicable to a target kind.
    fn for_kind(kind: TargetKind) -> &'static [Scenario] {
        match kind {
            TargetKind::Primary => &[Scenario::Cold, Scenario::Warm, Scenario::Edit],
            TargetKind::Sentinel => &[Scenario::Warm],
        }
    }
}

/// Prime the cache for a binary by running a trace.
fn prime_cache(binary: &str, entry: &Path) {
    let _ = Command::new(binary)
        .args(["trace", &entry.display().to_string(), "--quiet"])
        .stdout(std::process::Stdio::null())
        .stderr(std::process::Stdio::null())
        .status();
}

/// Run a single benchmark scenario for one target.
/// Returns true on success.
fn run_scenario(
    scenario: Scenario,
    target: &ResolvedTarget,
    binary_a: &str,
    binary_b: Option<&str>,
    output_dir: Option<&Path>,
) -> bool {
    let entry = target.entry.display().to_string();

    eprintln!(
        "=== {}: {} ===",
        scenario.label().to_uppercase(),
        target.name
    );

    // Warm/edit need primed caches
    if scenario != Scenario::Cold {
        prime_cache(binary_a, &target.entry);
        if let Some(b) = binary_b {
            prime_cache(b, &target.entry);
        }
    }

    let mut args: Vec<String> = vec![
        "--warmup".into(),
        scenario.warmup().to_string(),
        "--min-runs".into(),
        scenario.min_runs().to_string(),
    ];

    if let Some(prepare) = scenario.prepare(target) {
        args.push("--prepare".into());
        args.push(prepare);
    }

    if binary_b.is_some() {
        args.push("-n".into());
        args.push("baseline".into());
    }
    args.push(format!("{binary_a} trace {entry} --quiet"));

    if let Some(b) = binary_b {
        args.push("-n".into());
        args.push("candidate".into());
        args.push(format!("{b} trace {entry} --quiet"));
    }

    if let Some(dir) = output_dir {
        args.push("--export-json".into());
        args.push(
            dir.join(format!("{}-{}.json", scenario.label(), target.name))
                .display()
                .to_string(),
        );
    }

    let status = Command::new("hyperfine").args(&args).status();

    match status {
        Ok(s) if s.success() => true,
        Ok(s) => {
            eprintln!("hyperfine exited with code {}", s.code().unwrap_or(-1));
            false
        }
        Err(e) => {
            eprintln!("Failed to run hyperfine: {e}");
            eprintln!("Install with: cargo install hyperfine");
            false
        }
    }
}

// --- Entry point ---

/// Main entry point for `cargo xtask bench`.
pub fn run(
    scenario: Option<Scenario>,
    target: Option<&str>,
    baseline: Option<&str>,
    matrix: bool,
) -> i32 {
    let binary_a = baseline.unwrap_or("target/release/chainsaw");
    let binary_b: Option<&str> = if baseline.is_some() {
        Some("target/release/chainsaw")
    } else {
        None
    };

    // Resolve available targets
    let filter = if matrix { None } else { target };
    let (available, skipped) = resolve_targets(filter);

    if available.is_empty() {
        eprintln!("No benchmark targets available.");
        return 1;
    }

    print_target_summary(&available, &skipped);

    // Create output directory for JSON results
    let timestamp = chrono_free_timestamp();
    let output_dir = PathBuf::from(format!("/tmp/bench-{timestamp}"));
    let output_dir = match fs::create_dir_all(&output_dir) {
        Ok(()) => {
            eprintln!("Results: {}\n", output_dir.display());
            Some(output_dir)
        }
        Err(e) => {
            eprintln!(
                "Warning: cannot create output directory {}: {e}",
                output_dir.display()
            );
            eprintln!("JSON export disabled.\n");
            None
        }
    };

    let mut failures = 0;

    for t in &available {
        let scenarios = match scenario {
            Some(s) => vec![s],
            None => Scenario::for_kind(t.kind).to_vec(),
        };

        for s in scenarios {
            if !run_scenario(s, t, binary_a, binary_b, output_dir.as_deref()) {
                failures += 1;
            }
        }
        eprintln!();
    }

    if failures > 0 {
        eprintln!("{failures} scenario(s) failed.");
        1
    } else {
        if let Some(dir) = &output_dir {
            eprintln!("All benchmarks passed. Results: {}", dir.display());
        } else {
            eprintln!("All benchmarks passed.");
        }
        0
    }
}

/// Generate a timestamp string without pulling in chrono.
fn chrono_free_timestamp() -> String {
    let secs = SystemTime::now()
        .duration_since(SystemTime::UNIX_EPOCH)
        .map(|d| d.as_secs())
        .unwrap_or(0);
    format!("{secs}")
}
