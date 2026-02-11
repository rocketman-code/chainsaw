use oxc_resolver::{ResolveError, ResolveOptions, Resolver};
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

pub fn create_resolver() -> Resolver {
    Resolver::new(ResolveOptions {
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
        condition_names: vec!["node".into(), "import".into(), "require".into(), "default".into()],
        main_fields: vec!["module".into(), "main".into()],
        ..ResolveOptions::default()
    })
}

pub fn resolve_import(
    resolver: &Resolver,
    source_dir: &Path,
    specifier: &str,
) -> Option<PathBuf> {
    if is_node_builtin(specifier) {
        return None;
    }

    match resolver.resolve(source_dir, specifier) {
        Ok(resolution) => Some(resolution.into_path_buf()),
        Err(ResolveError::NotFound(_)) => None,
        Err(_) => None,
    }
}

/// Extract the package name from a node_modules path.
/// e.g. "/project/node_modules/@aws-sdk/client-bedrock/dist/index.js" -> Some("@aws-sdk/client-bedrock")
pub fn package_name_from_path(path: &Path) -> Option<String> {
    let components: Vec<&str> = path
        .components()
        .map(|c| c.as_os_str().to_str().unwrap_or(""))
        .collect();

    // Find the last "node_modules" segment
    let nm_idx = components.iter().rposition(|c| *c == "node_modules")?;

    if nm_idx + 1 >= components.len() {
        return None;
    }

    let first = components[nm_idx + 1];
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
