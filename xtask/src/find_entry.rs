use std::path::Path;
use std::process::Command;
use walkdir::WalkDir;

/// Entry point candidates by filename.
const ENTRY_NAMES: &[&str] = &["index.ts", "index.tsx", "__init__.py"];

/// Directories to skip during traversal.
const SKIP_DIRS: &[&str] = &["node_modules", ".venv", "venv", "__pycache__", ".git"];

pub fn run(root: &str, binary: Option<&str>) -> i32 {
    let root = Path::new(root);
    let binary = binary.unwrap_or("target/release/chainsaw");

    if !root.is_dir() {
        eprintln!("Not a directory: {}", root.display());
        return 1;
    }

    let root = match root.canonicalize() {
        Ok(p) => p,
        Err(e) => {
            eprintln!("Cannot resolve path: {e}");
            return 1;
        }
    };

    // Collect candidate entry points
    let candidates: Vec<_> = WalkDir::new(&root)
        .into_iter()
        .filter_entry(|e| {
            if e.file_type().is_dir() {
                let name = e.file_name().to_string_lossy();
                !SKIP_DIRS.iter().any(|s| *s == name.as_ref())
            } else {
                true
            }
        })
        .filter_map(|e| e.ok())
        .filter(|e| {
            e.file_type().is_file()
                && ENTRY_NAMES
                    .iter()
                    .any(|n| e.file_name().to_string_lossy() == *n)
        })
        .map(|e| e.into_path())
        .collect();

    if candidates.is_empty() {
        eprintln!("No candidate entry points found in {}", root.display());
        return 1;
    }

    eprintln!(
        "Scanning {} candidates in {}...",
        candidates.len(),
        root.display()
    );

    let mut best_count: u64 = 0;
    let mut best_entry = None;

    for path in &candidates {
        let output = Command::new(binary)
            .args([
                "trace",
                &path.display().to_string(),
                "--quiet",
                "--no-cache",
            ])
            .output();

        let count = match output {
            Ok(o) => parse_module_count(&String::from_utf8_lossy(&o.stdout)),
            Err(_) => 0,
        };

        if count > best_count {
            best_count = count;
            best_entry = Some(path.clone());
        }
    }

    match best_entry {
        Some(entry) => {
            println!("{best_count} {}", entry.display());
            0
        }
        None => {
            eprintln!("No valid entry points found");
            1
        }
    }
}

/// Extract the module count from chainsaw trace output.
fn parse_module_count(output: &str) -> u64 {
    // Look for "N modules" pattern
    output
        .split_whitespace()
        .zip(output.split_whitespace().skip(1))
        .find(|(_, word)| *word == "modules")
        .and_then(|(num, _)| num.parse().ok())
        .unwrap_or(0)
}
