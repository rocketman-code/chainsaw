use oxc_resolver::{ResolveOptions, Resolver};
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

#[derive(Debug)]
pub struct ImportResolver {
    resolver: Resolver,
}

impl ImportResolver {
    pub fn new(_root: &Path) -> Self {
        let resolver = Resolver::new(ResolveOptions {
            modules: vec!["node_modules".into()],
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
            ..ResolveOptions::default()
        });

        Self { resolver }
    }

    pub fn resolve(&self, source_dir: &Path, specifier: &str) -> Option<PathBuf> {
        if is_node_builtin(specifier) {
            return None;
        }

        if let Ok(resolution) = self.resolver.resolve(source_dir, specifier) {
            return Some(resolution.into_path_buf());
        }

        None
    }
}

/// Extract the package name from a resolved file path.
/// Handles both standard `node_modules` and pnpm virtual store layouts.
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

pub(super) fn read_package_name(pkg_json: &Path) -> Option<String> {
    let content = std::fs::read_to_string(pkg_json).ok()?;
    let parsed: serde_json::Value = serde_json::from_str(&content).ok()?;
    parsed.get("name")?.as_str().map(str::to_string)
}
