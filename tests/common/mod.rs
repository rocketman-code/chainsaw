use std::path::{Path, PathBuf};

/// A self-contained TypeScript project for integration tests.
///
/// Structure:
///   index.ts  -> imports ./a, ./b; dynamic import("./c")
///   a.ts      -> imports lodash
///   b.ts      -> imports ./a  (creates a diamond: index->b->a->lodash)
///   c.ts      -> standalone  (dynamic-only)
///   node_modules/lodash/package.json + index.js
///
/// Properties:
///   - 4 first-party modules + 1 third-party package (lodash)
///   - Static chain to lodash: index->a->lodash, index->b->a->lodash
///   - Cut point for lodash: a.ts (all static chains pass through it)
///   - Dynamic-only module: c.ts
///   - Diamond dependency: a.ts reachable via index->a and index->b->a
pub struct TestProject {
    pub dir: tempfile::TempDir,
    pub entry: PathBuf,
}

impl TestProject {
    /// Create the fixture. Caller must keep the returned value alive
    /// (dropping `TempDir` deletes the files).
    pub fn new() -> Self {
        let dir = tempfile::tempdir().unwrap();
        let root = dir.path();

        std::fs::write(
            root.join("package.json"),
            r#"{"name":"test-project","version":"1.0.0"}"#,
        )
        .unwrap();

        // index.ts: static imports + one dynamic import
        std::fs::write(
            root.join("index.ts"),
            concat!(
                "import { a } from './a';\n",
                "import { b } from './b';\n",
                "const c = import('./c');\n",
            ),
        )
        .unwrap();

        // a.ts: imports a third-party package
        std::fs::write(
            root.join("a.ts"),
            "import _ from 'lodash';\nexport const a = 1;\n",
        )
        .unwrap();

        // b.ts: imports ./a (diamond)
        std::fs::write(
            root.join("b.ts"),
            "import { a } from './a';\nexport const b = a + 1;\n",
        )
        .unwrap();

        // c.ts: standalone, only reachable via dynamic import
        std::fs::write(root.join("c.ts"), "export const c = 'dynamic only';\n").unwrap();

        // node_modules/lodash — minimal third-party package
        let lodash = root.join("node_modules/lodash");
        std::fs::create_dir_all(&lodash).unwrap();
        std::fs::write(
            lodash.join("package.json"),
            r#"{"name":"lodash","version":"4.17.21","main":"index.js"}"#,
        )
        .unwrap();
        std::fs::write(
            lodash.join("index.js"),
            "module.exports = { identity: function(x) { return x; } };\n",
        )
        .unwrap();

        let entry = root.join("index.ts");
        Self { dir, entry }
    }

    pub fn root(&self) -> &Path {
        self.dir.path()
    }
}
