use oxc_resolver::{ResolveError, ResolveOptions, Resolver};
use std::fs;
use std::path::{Path, PathBuf};

const NODE_BUILTINS: &[&str] = &[
    "assert",
    "async_hooks",
    "buffer",
    "child_process",
    "cluster",
    "console",
    "constants",
    "crypto",
    "dgram",
    "diagnostics_channel",
    "dns",
    "domain",
    "events",
    "fs",
    "http",
    "http2",
    "https",
    "inspector",
    "module",
    "net",
    "os",
    "path",
    "perf_hooks",
    "process",
    "punycode",
    "querystring",
    "readline",
    "repl",
    "stream",
    "string_decoder",
    "sys",
    "test",
    "timers",
    "tls",
    "trace_events",
    "tty",
    "url",
    "util",
    "v8",
    "vm",
    "wasi",
    "worker_threads",
    "zlib",
];

pub fn is_node_builtin(specifier: &str) -> bool {
    if let Some(stripped) = specifier.strip_prefix("node:") {
        return NODE_BUILTINS.contains(&stripped) || stripped.starts_with("internal/");
    }
    NODE_BUILTINS.contains(&specifier)
}

/// Detect the pnpm virtual store directory by reading .modules.yaml or falling
/// back to the default node_modules/.pnpm location.
fn detect_pnpm_virtual_store(root: &Path) -> Option<PathBuf> {
    let modules_yaml = root.join("node_modules/.modules.yaml");
    if !modules_yaml.exists() {
        return None;
    }

    // Parse virtualStoreDir from .modules.yaml if present
    if let Ok(content) = fs::read_to_string(&modules_yaml) {
        for line in content.lines() {
            let trimmed = line.trim();
            if let Some(value) = trimmed.strip_prefix("virtualStoreDir:") {
                let dir = value.trim().trim_matches('\'').trim_matches('"');
                let path = if Path::new(dir).is_absolute() {
                    PathBuf::from(dir)
                } else {
                    root.join("node_modules").join(dir)
                };
                if path.is_dir() {
                    return Some(path);
                }
            }
        }
    }

    // Default pnpm virtual store location
    let default = root.join("node_modules/.pnpm");
    if default.is_dir() {
        Some(default)
    } else {
        None
    }
}

pub struct ImportResolver {
    resolver: Resolver,
    /// Path to pnpm virtual store (e.g. /project/node_modules/.pnpm), if detected
    pnpm_virtual_store: Option<PathBuf>,
}

impl ImportResolver {
    pub fn new(root: &Path) -> Self {
        let pnpm_virtual_store = detect_pnpm_virtual_store(root);
        let mut modules = vec!["node_modules".into()];

        // Add pnpm hoisted phantom deps directory as a module resolution path.
        // With default hoist-pattern=['*'], most transitive deps get symlinks here.
        if let Some(ref vs) = pnpm_virtual_store {
            let hoisted = vs.join("node_modules");
            if hoisted.is_dir() {
                modules.push(hoisted.to_string_lossy().into_owned());
            }
        }

        let resolver = Resolver::new(ResolveOptions {
            extensions: vec![
                ".ts".into(),
                ".tsx".into(),
                ".d.ts".into(),
                ".js".into(),
                ".jsx".into(),
                ".mjs".into(),
                ".cjs".into(),
                ".json".into(),
                ".node".into(),
            ],
            extension_alias: vec![
                (
                    ".js".into(),
                    vec![".ts".into(), ".tsx".into(), ".js".into()],
                ),
                (".mjs".into(), vec![".mts".into(), ".mjs".into()]),
                (".cjs".into(), vec![".cts".into(), ".cjs".into()]),
            ],
            condition_names: vec![
                "node".into(),
                "import".into(),
                "require".into(),
                "default".into(),
            ],
            main_fields: vec!["module".into(), "main".into()],
            modules,
            ..ResolveOptions::default()
        });

        Self {
            resolver,
            pnpm_virtual_store,
        }
    }

    pub fn resolve(&self, source_dir: &Path, specifier: &str) -> Option<PathBuf> {
        if is_node_builtin(specifier) {
            return None;
        }

        // Try standard resolution first (handles npm, yarn, pnpm with symlinks,
        // relative imports, and .pnpm/node_modules/ hoisted deps)
        match self.resolver.resolve(source_dir, specifier) {
            Ok(resolution) => return Some(resolution.into_path_buf()),
            Err(ResolveError::NotFound(_)) => {}
            Err(_) => {}
        }

        // Fallback: glob the pnpm virtual store directly.
        // Handles cases where packages aren't in .pnpm/node_modules/ (e.g. custom
        // hoist-pattern, missing top-level symlinks, or strict isolation edge cases).
        if let Some(ref vs) = self.pnpm_virtual_store {
            if let Some(path) = self.resolve_pnpm_virtual_store(vs, specifier) {
                return Some(path);
            }
        }

        None
    }

    /// Search for a bare specifier in the pnpm virtual store by globbing
    /// .pnpm/<encoded_name>@*/node_modules/<name>/
    fn resolve_pnpm_virtual_store(&self, virtual_store: &Path, specifier: &str) -> Option<PathBuf> {
        // Only try for bare specifiers (not relative/absolute paths)
        if specifier.starts_with('.') || specifier.starts_with('/') {
            return None;
        }

        // Extract package name from specifier (e.g. "@aws-sdk/client-bedrock/types" -> "@aws-sdk/client-bedrock")
        let (pkg_name, subpath) = split_package_specifier(specifier);

        // Encode the package name for pnpm directory format: @scope/name -> @scope+name
        let encoded = pkg_name.replace('/', "+");

        // Scan the virtual store for matching directories
        let entries = fs::read_dir(virtual_store).ok()?;
        for entry in entries.flatten() {
            let dir_name = entry.file_name();
            let dir_name = dir_name.to_string_lossy();

            // Match: <encoded_name>@<version> (possibly with peer dep suffixes)
            if !dir_name.starts_with(&encoded) {
                continue;
            }
            // Ensure we match at the @ separator (not a prefix of a longer name)
            let rest = &dir_name[encoded.len()..];
            if !rest.starts_with('@') {
                continue;
            }

            // The actual package lives at .pnpm/<dir>/node_modules/<pkg_name>/
            let pkg_dir = entry.path().join("node_modules").join(pkg_name);
            if !pkg_dir.is_dir() {
                continue;
            }

            // If there's a subpath, resolve it within the package
            if let Some(sub) = subpath {
                let full = pkg_dir.join(sub);
                // Try resolving the subpath with oxc_resolver for proper extension handling
                if let Ok(resolution) = self.resolver.resolve(&pkg_dir, &format!("./{sub}")) {
                    return Some(resolution.into_path_buf());
                }
                // Direct file check as fallback
                if full.exists() {
                    return Some(full);
                }
            } else {
                // Resolve the package entry point using oxc_resolver
                // (reads package.json exports/main/module fields)
                if let Ok(resolution) = self.resolver.resolve(pkg_dir.parent()?, pkg_name) {
                    return Some(resolution.into_path_buf());
                }
                // If that fails, check for package.json to at least confirm it exists
                if pkg_dir.join("package.json").exists() {
                    // Try resolving from within the package's own node_modules context
                    if let Ok(resolution) = self.resolver.resolve(&pkg_dir, ".") {
                        return Some(resolution.into_path_buf());
                    }
                }
            }
        }

        None
    }
}

/// Split a bare specifier into (package_name, optional_subpath).
/// "@aws-sdk/client-bedrock" -> ("@aws-sdk/client-bedrock", None)
/// "@aws-sdk/client-bedrock/types" -> ("@aws-sdk/client-bedrock", Some("types"))
/// "lodash/fp" -> ("lodash", Some("fp"))
/// "lodash" -> ("lodash", None)
fn split_package_specifier(specifier: &str) -> (&str, Option<&str>) {
    if let Some(scoped) = specifier.strip_prefix('@') {
        // Scoped: @scope/name or @scope/name/subpath
        // Find the second '/' (after scope/name)
        if let Some(first_slash) = scoped.find('/') {
            let after_name = &scoped[first_slash + 1..];
            if let Some(second_slash) = after_name.find('/') {
                let pkg_end = 1 + first_slash + 1 + second_slash; // @scope/name
                return (&specifier[..pkg_end], Some(&specifier[pkg_end + 1..]));
            }
        }
        (specifier, None)
    } else {
        // Unscoped: name or name/subpath
        if let Some(slash) = specifier.find('/') {
            (&specifier[..slash], Some(&specifier[slash + 1..]))
        } else {
            (specifier, None)
        }
    }
}

/// Extract the package name from a resolved file path.
/// Handles both standard node_modules and pnpm virtual store layouts.
/// e.g. "/project/node_modules/@aws-sdk/client-bedrock/dist/index.js" -> Some("@aws-sdk/client-bedrock")
/// e.g. "/project/node_modules/.pnpm/@aws-sdk+client-bedrock@3.986.0/node_modules/@aws-sdk/client-bedrock/dist/index.js" -> Some("@aws-sdk/client-bedrock")
pub fn package_name_from_path(path: &Path) -> Option<String> {
    let components: Vec<&str> = path
        .components()
        .map(|c| c.as_os_str().to_str().unwrap_or(""))
        .collect();

    // Find the last "node_modules" segment (handles both standard and pnpm layouts,
    // since pnpm paths always end with .../node_modules/<pkg>/...)
    let nm_idx = components.iter().rposition(|c| *c == "node_modules")?;

    if nm_idx + 1 >= components.len() {
        return None;
    }

    let first = components[nm_idx + 1];

    // Skip .pnpm directory itself — walk into it
    if first == ".pnpm" {
        return None;
    }

    if first.starts_with('@') {
        // Scoped package: @scope/name
        if nm_idx + 2 < components.len() {
            Some(format!("{}/{}", first, components[nm_idx + 2]))
        } else {
            None
        }
    } else {
        Some(first.to_string())
    }
}
