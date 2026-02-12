use std::fs;
use std::path::Path;

use tree_sitter::Parser;

use crate::graph::EdgeKind;
use crate::lang::RawImport;

pub fn parse_file(path: &Path) -> Result<Vec<RawImport>, String> {
    let source =
        fs::read_to_string(path).map_err(|e| format!("{}: {e}", path.display()))?;
    parse_source(&source)
}

fn parse_source(source: &str) -> Result<Vec<RawImport>, String> {
    let mut parser = Parser::new();
    parser
        .set_language(&tree_sitter_python::LANGUAGE.into())
        .map_err(|e| format!("Failed to set Python language: {e}"))?;

    let tree = parser
        .parse(source, None)
        .ok_or_else(|| "tree-sitter returned no parse tree".to_string())?;

    let mut imports = Vec::new();
    collect_imports(tree.root_node(), source.as_bytes(), &mut imports, false, false);
    Ok(imports)
}

/// Recursively walk the tree-sitter AST, collecting import statements.
/// `in_type_checking` is true when we are inside an `if TYPE_CHECKING:` block.
/// `in_function` is true when we are inside a function/method body.
fn collect_imports(
    node: tree_sitter::Node,
    source: &[u8],
    imports: &mut Vec<RawImport>,
    in_type_checking: bool,
    in_function: bool,
) {
    let kind_str = node.kind();

    match kind_str {
        "import_statement" => {
            let edge_kind = if in_type_checking {
                EdgeKind::TypeOnly
            } else if in_function {
                EdgeKind::Dynamic
            } else {
                EdgeKind::Static
            };
            // Each named child is a dotted_name or aliased_import
            for i in 0..node.named_child_count() {
                if let Some(child) = node.named_child(i) {
                    let specifier = match child.kind() {
                        "dotted_name" => text(child, source),
                        "aliased_import" => {
                            // field "name" holds the actual module name
                            child
                                .child_by_field_name("name")
                                .map(|n| text(n, source))
                                .unwrap_or_default()
                        }
                        _ => continue,
                    };
                    if !specifier.is_empty() {
                        imports.push(RawImport {
                            specifier,
                            kind: edge_kind,
                        });
                    }
                }
            }
            return;
        }

        "import_from_statement" => {
            // Skip `from __future__ import ...`
            if let Some(module_node) = node.child_by_field_name("module_name") {
                let module_text = text(module_node, source);
                if module_text == "__future__" {
                    return;
                }
            }

            let edge_kind = if in_type_checking {
                EdgeKind::TypeOnly
            } else if in_function {
                EdgeKind::Dynamic
            } else {
                EdgeKind::Static
            };

            // Determine if this is a relative import with bare dots (no module after dots)
            let (dot_prefix, module_name) = extract_from_module(node, source);
            let is_bare_relative = !dot_prefix.is_empty() && module_name.is_empty();

            if is_bare_relative {
                // `from . import foo, bar` -> specifiers are `.foo`, `.bar`
                for i in 0..node.named_child_count() {
                    if let Some(child) = node.named_child(i) {
                        let name = match child.kind() {
                            "dotted_name" => text(child, source),
                            "aliased_import" => child
                                .child_by_field_name("name")
                                .map(|n| text(n, source))
                                .unwrap_or_default(),
                            _ => continue,
                        };
                        // Skip the module_name field child (which would be a relative_import)
                        if child.kind() == "relative_import" {
                            continue;
                        }
                        if !name.is_empty() {
                            imports.push(RawImport {
                                specifier: format!("{dot_prefix}{name}"),
                                kind: edge_kind,
                            });
                        }
                    }
                }
            } else {
                // `from foo import bar` or `from ..foo import bar`
                // Specifier is the module (possibly with dot prefix)
                let specifier = if dot_prefix.is_empty() {
                    module_name
                } else {
                    format!("{dot_prefix}{module_name}")
                };
                if !specifier.is_empty() {
                    imports.push(RawImport {
                        specifier,
                        kind: edge_kind,
                    });
                }
            }
            return;
        }

        "if_statement" => {
            let is_tc = is_type_checking_guard(node, source);
            let propagated = in_type_checking || is_tc;

            for i in 0..node.named_child_count() {
                if let Some(child) = node.named_child(i) {
                    collect_imports(child, source, imports, propagated, in_function);
                }
            }
            return;
        }

        "function_definition" => {
            for i in 0..node.named_child_count() {
                if let Some(child) = node.named_child(i) {
                    collect_imports(child, source, imports, in_type_checking, true);
                }
            }
            return;
        }

        "call" => {
            if let Some(specifier) = extract_dynamic_import(node, source) {
                imports.push(RawImport {
                    specifier,
                    kind: EdgeKind::Dynamic,
                });
            }
            // Don't return -- fall through to generic recursion for nested calls
        }

        _ => {}
    }

    // Generic recursion: visit all named children
    for i in 0..node.named_child_count() {
        if let Some(child) = node.named_child(i) {
            collect_imports(child, source, imports, in_type_checking, in_function);
        }
    }
}

/// Extract the dot prefix and module name from an import_from_statement.
/// Returns (dots, module_name) where dots is like "." or ".." or "..."
/// and module_name is the dotted name after the dots (possibly empty for bare relative).
fn extract_from_module(node: tree_sitter::Node, source: &[u8]) -> (String, String) {
    if let Some(module_node) = node.child_by_field_name("module_name") {
        match module_node.kind() {
            "dotted_name" => {
                // Absolute: `from foo.bar import baz`
                return (String::new(), text(module_node, source));
            }
            "relative_import" => {
                // Relative: `from ..foo import bar` or `from . import foo`
                let dot_prefix = extract_dot_prefix(module_node, source);
                let module_name = extract_module_name(module_node, source)
                    .unwrap_or_default();
                return (dot_prefix, module_name);
            }
            _ => {}
        }
    }
    (String::new(), String::new())
}

/// Extract the dotted_name child of a relative_import node (if present).
fn extract_module_name(node: tree_sitter::Node, source: &[u8]) -> Option<String> {
    // Look for a dotted_name among named children
    for i in 0..node.named_child_count() {
        if let Some(child) = node.named_child(i) {
            if child.kind() == "dotted_name" {
                return Some(text(child, source));
            }
        }
    }
    None
}

/// Extract the dot prefix (e.g., ".", "..", "...") from a relative_import node.
/// The import_prefix node is not a named node, so we iterate over all children.
fn extract_dot_prefix(node: tree_sitter::Node, source: &[u8]) -> String {
    for i in 0..node.child_count() {
        if let Some(child) = node.child(i) {
            if child.kind() == "import_prefix" {
                return text(child, source);
            }
        }
    }
    String::new()
}

/// Check if an if_statement is a `if TYPE_CHECKING:` guard.
fn is_type_checking_guard(node: tree_sitter::Node, source: &[u8]) -> bool {
    if let Some(condition) = node.child_by_field_name("condition") {
        let cond_text = text(condition, source);
        return cond_text == "TYPE_CHECKING" || cond_text.ends_with(".TYPE_CHECKING");
    }
    false
}

/// Extract the specifier from a dynamic import call:
/// `importlib.import_module("x")` or `__import__("x")`.
fn extract_dynamic_import(node: tree_sitter::Node, source: &[u8]) -> Option<String> {
    let function_node = node.child_by_field_name("function")?;
    let is_dynamic = match function_node.kind() {
        "attribute" => {
            // importlib.import_module(...)
            let func_text = text(function_node, source);
            func_text == "importlib.import_module"
        }
        "identifier" => {
            // __import__(...)
            text(function_node, source) == "__import__"
        }
        _ => false,
    };

    if !is_dynamic {
        return None;
    }

    // Extract the first string argument
    let args_node = node.child_by_field_name("arguments")?;
    for i in 0..args_node.named_child_count() {
        if let Some(arg) = args_node.named_child(i) {
            if arg.kind() == "string" {
                let raw = text(arg, source);
                // Strip surrounding quotes (single, double, or triple)
                let stripped = strip_string_quotes(&raw);
                if !stripped.is_empty() {
                    return Some(stripped.to_string());
                }
            }
        }
    }
    None
}

/// Strip surrounding quotes from a Python string literal.
fn strip_string_quotes(s: &str) -> &str {
    // Handle triple quotes first
    for q in &[r#"""""#, "'''"] {
        if s.starts_with(q) && s.ends_with(q) && s.len() >= q.len() * 2 {
            return &s[q.len()..s.len() - q.len()];
        }
    }
    // Single/double quotes
    for q in &["\"", "'"] {
        if s.starts_with(q) && s.ends_with(q) && s.len() >= 2 {
            return &s[q.len()..s.len() - q.len()];
        }
    }
    s
}

/// Get the text content of a tree-sitter node.
fn text(node: tree_sitter::Node, source: &[u8]) -> String {
    node.utf8_text(source).unwrap_or("").to_string()
}

#[cfg(test)]
mod tests {
    use super::*;

    fn parse_py(source: &str) -> Vec<RawImport> {
        parse_source(source).expect("parse failed")
    }

    #[test]
    fn import_simple() {
        let imports = parse_py("import foo");
        assert_eq!(imports.len(), 1);
        assert_eq!(imports[0].specifier, "foo");
        assert_eq!(imports[0].kind, EdgeKind::Static);
    }

    #[test]
    fn import_dotted() {
        let imports = parse_py("import foo.bar.baz");
        assert_eq!(imports.len(), 1);
        assert_eq!(imports[0].specifier, "foo.bar.baz");
        assert_eq!(imports[0].kind, EdgeKind::Static);
    }

    #[test]
    fn import_aliased() {
        let imports = parse_py("import foo.bar as fb");
        assert_eq!(imports.len(), 1);
        assert_eq!(imports[0].specifier, "foo.bar");
        assert_eq!(imports[0].kind, EdgeKind::Static);
    }

    #[test]
    fn import_multiple() {
        let imports = parse_py("import os, sys, json");
        assert_eq!(imports.len(), 3);
        assert_eq!(imports[0].specifier, "os");
        assert_eq!(imports[1].specifier, "sys");
        assert_eq!(imports[2].specifier, "json");
    }

    #[test]
    fn from_import() {
        let imports = parse_py("from foo import bar");
        assert_eq!(imports.len(), 1);
        assert_eq!(imports[0].specifier, "foo");
        assert_eq!(imports[0].kind, EdgeKind::Static);
    }

    #[test]
    fn from_dotted_import() {
        let imports = parse_py("from foo.bar import baz");
        assert_eq!(imports.len(), 1);
        assert_eq!(imports[0].specifier, "foo.bar");
        assert_eq!(imports[0].kind, EdgeKind::Static);
    }

    #[test]
    fn from_relative_dot() {
        let imports = parse_py("from . import foo");
        assert_eq!(imports.len(), 1);
        assert_eq!(imports[0].specifier, ".foo");
        assert_eq!(imports[0].kind, EdgeKind::Static);
    }

    #[test]
    fn from_relative_dot_multiple() {
        let imports = parse_py("from . import foo, bar");
        assert_eq!(imports.len(), 2);
        assert_eq!(imports[0].specifier, ".foo");
        assert_eq!(imports[1].specifier, ".bar");
    }

    #[test]
    fn from_relative_dotdot() {
        let imports = parse_py("from .. import foo");
        assert_eq!(imports.len(), 1);
        assert_eq!(imports[0].specifier, "..foo");
        assert_eq!(imports[0].kind, EdgeKind::Static);
    }

    #[test]
    fn from_relative_dotdot_module() {
        let imports = parse_py("from ..foo import bar");
        assert_eq!(imports.len(), 1);
        assert_eq!(imports[0].specifier, "..foo");
        assert_eq!(imports[0].kind, EdgeKind::Static);
    }

    #[test]
    fn from_relative_deep() {
        let imports = parse_py("from ...foo.bar import baz");
        assert_eq!(imports.len(), 1);
        assert_eq!(imports[0].specifier, "...foo.bar");
        assert_eq!(imports[0].kind, EdgeKind::Static);
    }

    #[test]
    fn skip_future_import() {
        let imports = parse_py("from __future__ import annotations");
        assert!(imports.is_empty());
    }

    #[test]
    fn type_checking_block() {
        let imports = parse_py(
            "from typing import TYPE_CHECKING\nif TYPE_CHECKING:\n    from foo import Bar",
        );
        assert_eq!(imports.len(), 2);
        assert_eq!(imports[0].specifier, "typing");
        assert_eq!(imports[0].kind, EdgeKind::Static);
        assert_eq!(imports[1].specifier, "foo");
        assert_eq!(imports[1].kind, EdgeKind::TypeOnly);
    }

    #[test]
    fn importlib_dynamic() {
        let imports = parse_py("import importlib\nimportlib.import_module(\"foo\")");
        assert_eq!(imports.len(), 2);
        assert_eq!(imports[0].specifier, "importlib");
        assert_eq!(imports[0].kind, EdgeKind::Static);
        assert_eq!(imports[1].specifier, "foo");
        assert_eq!(imports[1].kind, EdgeKind::Dynamic);
    }

    #[test]
    fn dunder_import_dynamic() {
        let imports = parse_py("__import__(\"foo.bar\")");
        assert_eq!(imports.len(), 1);
        assert_eq!(imports[0].specifier, "foo.bar");
        assert_eq!(imports[0].kind, EdgeKind::Dynamic);
    }

    #[test]
    fn wildcard_import() {
        let imports = parse_py("from foo import *");
        assert_eq!(imports.len(), 1);
        assert_eq!(imports[0].specifier, "foo");
        assert_eq!(imports[0].kind, EdgeKind::Static);
    }

    #[test]
    fn import_inside_function_is_dynamic() {
        let imports = parse_py("def foo():\n    import bar");
        assert_eq!(imports.len(), 1);
        assert_eq!(imports[0].specifier, "bar");
        assert_eq!(imports[0].kind, EdgeKind::Dynamic);
    }

    #[test]
    fn from_import_inside_function_is_dynamic() {
        let imports = parse_py("def foo():\n    from bar import baz");
        assert_eq!(imports.len(), 1);
        assert_eq!(imports[0].specifier, "bar");
        assert_eq!(imports[0].kind, EdgeKind::Dynamic);
    }

    #[test]
    fn import_inside_method_is_dynamic() {
        let imports = parse_py("class Foo:\n    def bar(self):\n        from baz import qux");
        assert_eq!(imports.len(), 1);
        assert_eq!(imports[0].specifier, "baz");
        assert_eq!(imports[0].kind, EdgeKind::Dynamic);
    }

    #[test]
    fn import_inside_nested_function_is_dynamic() {
        let imports = parse_py("def outer():\n    def inner():\n        import foo\n    import bar");
        assert_eq!(imports.len(), 2);
        assert_eq!(imports[0].kind, EdgeKind::Dynamic);
        assert_eq!(imports[1].kind, EdgeKind::Dynamic);
    }

    #[test]
    fn top_level_static_function_level_dynamic() {
        let imports =
            parse_py("import os\nfrom pathlib import Path\ndef foo():\n    import requests");
        assert_eq!(imports.len(), 3);
        assert_eq!(imports[0].specifier, "os");
        assert_eq!(imports[0].kind, EdgeKind::Static);
        assert_eq!(imports[1].specifier, "pathlib");
        assert_eq!(imports[1].kind, EdgeKind::Static);
        assert_eq!(imports[2].specifier, "requests");
        assert_eq!(imports[2].kind, EdgeKind::Dynamic);
    }

    #[test]
    fn mixed_imports_in_file() {
        let imports = parse_py(
            "import os\nfrom pathlib import Path\nfrom . import utils\nfrom ..config import settings",
        );
        assert_eq!(imports.len(), 4);
        assert_eq!(imports[0].specifier, "os");
        assert_eq!(imports[1].specifier, "pathlib");
        assert_eq!(imports[2].specifier, ".utils");
        assert_eq!(imports[3].specifier, "..config");
    }
}
