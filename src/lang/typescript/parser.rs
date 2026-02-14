use std::path::Path;

use oxc_allocator::Allocator;
use oxc_ast::ast::{Argument, ArrayExpressionElement, Expression, ObjectPropertyKind, Statement};
use oxc_parser::Parser;
use oxc_span::SourceType;

use crate::graph::EdgeKind;
use crate::lang::{ParseResult, RawImport};

/// A raw import tagged with its byte offset in the source for ordering.
struct PositionedImport {
    offset: u32,
    import: RawImport,
}

fn source_type_for_path(path: &Path) -> SourceType {
    match path.extension().and_then(|e| e.to_str()) {
        Some("ts") => SourceType::ts(),
        Some("tsx") => SourceType::tsx(),
        Some("jsx") => SourceType::jsx(),
        // .js, .mjs, .cjs, and anything else: parse as ESM JS
        _ => SourceType::mjs(),
    }
}

pub fn parse_file(path: &Path, source: &str) -> Result<ParseResult, String> {
    let source_type = source_type_for_path(path);
    extract_all(source, source_type)
}

fn extract_all(source: &str, source_type: SourceType) -> Result<ParseResult, String> {
    let allocator = Allocator::default();
    let ret = Parser::new(&allocator, source, source_type).parse();

    let mut positioned: Vec<PositionedImport> = Vec::new();
    let mut unresolvable_dynamic: usize = 0;

    // --- Static imports from ModuleRecord ---
    extract_import_entries(&ret.module_record.import_entries, &mut positioned);

    // --- Re-exports from ModuleRecord ---
    extract_export_entries(&ret.module_record.star_export_entries, &mut positioned);
    extract_export_entries(&ret.module_record.indirect_export_entries, &mut positioned);

    // --- Dynamic imports from ModuleRecord ---
    for di in &ret.module_record.dynamic_imports {
        let start = di.module_request.start as usize;
        let end = di.module_request.end as usize;
        if start < end && end <= source.len() {
            let text = &source[start..end];
            if text.starts_with('"') || text.starts_with('\'') {
                // String literal — strip quotes
                let specifier = &text[1..text.len() - 1];
                positioned.push(PositionedImport {
                    offset: di.span.start,
                    import: RawImport {
                        specifier: specifier.to_string(),
                        kind: EdgeKind::Dynamic,
                    },
                });
            } else {
                unresolvable_dynamic += 1;
            }
        } else {
            unresolvable_dynamic += 1;
        }
    }

    // --- require() calls from AST walking ---
    for stmt in &ret.program.body {
        walk_stmt(stmt, &mut positioned, &mut unresolvable_dynamic);
    }

    // Sort all collected imports by source position
    positioned.sort_by_key(|p| p.offset);

    // Deduplicate: ModuleRecord may produce entries that overlap with AST walking
    // (shouldn't happen since ModuleRecord handles ESM and we only walk for require,
    // but sort is needed for interleaving)
    let imports = positioned.into_iter().map(|p| p.import).collect();

    Ok(ParseResult { imports, unresolvable_dynamic })
}

/// Process ModuleRecord import_entries, grouping by module_request to determine
/// whether all bindings for a given specifier are type-only.
fn extract_import_entries(
    entries: &[oxc_syntax::module_record::ImportEntry<'_>],
    positioned: &mut Vec<PositionedImport>,
) {
    // Group entries by (module_request name, statement_span.start) to handle
    // multiple import statements from the same module.
    // We use statement_span.start as a unique key for each import statement.
    let mut seen: Vec<(u32, &str, bool)> = Vec::new(); // (stmt_start, specifier, all_type_so_far)

    for entry in entries {
        let specifier = entry.module_request.name.as_str();
        let stmt_start = entry.statement_span.start;

        if let Some(existing) = seen.iter_mut().find(|(s, spec, _)| *s == stmt_start && *spec == specifier) {
            // Another binding from the same import statement — AND the is_type flags
            if !entry.is_type {
                existing.2 = false;
            }
        } else {
            seen.push((stmt_start, specifier, entry.is_type));
        }
    }

    for (stmt_start, specifier, all_type) in seen {
        let kind = if all_type { EdgeKind::TypeOnly } else { EdgeKind::Static };
        positioned.push(PositionedImport {
            offset: stmt_start,
            import: RawImport {
                specifier: specifier.to_string(),
                kind,
            },
        });
    }
}

/// Process ModuleRecord export entries (star_export_entries or indirect_export_entries),
/// grouping by module_request to determine type-only status.
fn extract_export_entries(
    entries: &[oxc_syntax::module_record::ExportEntry<'_>],
    positioned: &mut Vec<PositionedImport>,
) {
    let mut seen: Vec<(u32, &str, bool)> = Vec::new();

    for entry in entries {
        let Some(ref module_request) = entry.module_request else {
            continue;
        };
        let specifier = module_request.name.as_str();
        let stmt_start = entry.statement_span.start;

        if let Some(existing) = seen.iter_mut().find(|(s, spec, _)| *s == stmt_start && *spec == specifier) {
            if !entry.is_type {
                existing.2 = false;
            }
        } else {
            seen.push((stmt_start, specifier, entry.is_type));
        }
    }

    for (stmt_start, specifier, all_type) in seen {
        let kind = if all_type { EdgeKind::TypeOnly } else { EdgeKind::Static };
        positioned.push(PositionedImport {
            offset: stmt_start,
            import: RawImport {
                specifier: specifier.to_string(),
                kind,
            },
        });
    }
}

// --- AST walking for require() calls ---

fn walk_stmt(stmt: &Statement<'_>, imports: &mut Vec<PositionedImport>, unresolvable: &mut usize) {
    match stmt {
        Statement::ExpressionStatement(expr_stmt) => {
            walk_expr(&expr_stmt.expression, imports, unresolvable);
        }
        Statement::VariableDeclaration(var_decl) => {
            for decl in &var_decl.declarations {
                if let Some(init) = &decl.init {
                    walk_expr(init, imports, unresolvable);
                }
            }
        }
        Statement::FunctionDeclaration(fn_decl) => {
            if let Some(body) = &fn_decl.body {
                for s in &body.statements {
                    walk_stmt(s, imports, unresolvable);
                }
            }
        }
        Statement::BlockStatement(block) => {
            for s in &block.body {
                walk_stmt(s, imports, unresolvable);
            }
        }
        Statement::IfStatement(if_stmt) => {
            walk_stmt(&if_stmt.consequent, imports, unresolvable);
            if let Some(alt) = &if_stmt.alternate {
                walk_stmt(alt, imports, unresolvable);
            }
        }
        Statement::SwitchStatement(switch) => {
            walk_expr(&switch.discriminant, imports, unresolvable);
            for case in &switch.cases {
                for s in &case.consequent {
                    walk_stmt(s, imports, unresolvable);
                }
            }
        }
        Statement::TryStatement(try_stmt) => {
            for s in &try_stmt.block.body {
                walk_stmt(s, imports, unresolvable);
            }
            if let Some(catch) = &try_stmt.handler {
                for s in &catch.body.body {
                    walk_stmt(s, imports, unresolvable);
                }
            }
            if let Some(finalizer) = &try_stmt.finalizer {
                for s in &finalizer.body {
                    walk_stmt(s, imports, unresolvable);
                }
            }
        }
        Statement::WhileStatement(while_stmt) => {
            walk_stmt(&while_stmt.body, imports, unresolvable);
        }
        Statement::DoWhileStatement(do_while) => {
            walk_stmt(&do_while.body, imports, unresolvable);
        }
        Statement::ForStatement(for_stmt) => {
            walk_stmt(&for_stmt.body, imports, unresolvable);
        }
        Statement::ForInStatement(for_in) => {
            walk_stmt(&for_in.body, imports, unresolvable);
        }
        Statement::ForOfStatement(for_of) => {
            walk_stmt(&for_of.body, imports, unresolvable);
        }
        Statement::ReturnStatement(ret) => {
            if let Some(arg) = &ret.argument {
                walk_expr(arg, imports, unresolvable);
            }
        }
        Statement::LabeledStatement(labeled) => {
            walk_stmt(&labeled.body, imports, unresolvable);
        }
        _ => {}
    }
}

fn walk_expr(expr: &Expression<'_>, imports: &mut Vec<PositionedImport>, unresolvable: &mut usize) {
    match expr {
        Expression::CallExpression(call) => {
            // require("...")
            if let Some(str_lit) = call.common_js_require() {
                imports.push(PositionedImport {
                    offset: call.span.start,
                    import: RawImport {
                        specifier: str_lit.value.to_string(),
                        kind: EdgeKind::Static,
                    },
                });
                return;
            }
            // require(variable) — unresolvable
            if call.callee.is_specific_id("require") && !call.arguments.is_empty() {
                *unresolvable += 1;
                return;
            }
            // Walk callee and arguments for nested require/import calls
            walk_expr(&call.callee, imports, unresolvable);
            for arg in &call.arguments {
                walk_argument(arg, imports, unresolvable);
            }
        }
        Expression::ArrowFunctionExpression(arrow) => {
            for s in &arrow.body.statements {
                walk_stmt(s, imports, unresolvable);
            }
        }
        Expression::FunctionExpression(fn_expr) => {
            if let Some(body) = &fn_expr.body {
                for s in &body.statements {
                    walk_stmt(s, imports, unresolvable);
                }
            }
        }
        Expression::AssignmentExpression(assign) => {
            walk_expr(&assign.right, imports, unresolvable);
        }
        Expression::SequenceExpression(seq) => {
            for e in &seq.expressions {
                walk_expr(e, imports, unresolvable);
            }
        }
        Expression::ParenthesizedExpression(paren) => {
            walk_expr(&paren.expression, imports, unresolvable);
        }
        Expression::AwaitExpression(await_expr) => {
            walk_expr(&await_expr.argument, imports, unresolvable);
        }
        Expression::ConditionalExpression(cond) => {
            walk_expr(&cond.test, imports, unresolvable);
            walk_expr(&cond.consequent, imports, unresolvable);
            walk_expr(&cond.alternate, imports, unresolvable);
        }
        Expression::BinaryExpression(bin) => {
            walk_expr(&bin.left, imports, unresolvable);
            walk_expr(&bin.right, imports, unresolvable);
        }
        Expression::LogicalExpression(logical) => {
            walk_expr(&logical.left, imports, unresolvable);
            walk_expr(&logical.right, imports, unresolvable);
        }
        Expression::UnaryExpression(unary) => {
            walk_expr(&unary.argument, imports, unresolvable);
        }
        Expression::StaticMemberExpression(member) => {
            walk_expr(&member.object, imports, unresolvable);
        }
        Expression::ComputedMemberExpression(member) => {
            walk_expr(&member.object, imports, unresolvable);
        }
        Expression::ArrayExpression(array) => {
            for elem in &array.elements {
                if let Some(expr) = elem.as_expression() {
                    walk_expr(expr, imports, unresolvable);
                } else if let ArrayExpressionElement::SpreadElement(spread) = elem {
                    walk_expr(&spread.argument, imports, unresolvable);
                }
            }
        }
        Expression::ObjectExpression(object) => {
            for prop in &object.properties {
                match prop {
                    ObjectPropertyKind::ObjectProperty(p) => {
                        walk_expr(&p.value, imports, unresolvable);
                    }
                    ObjectPropertyKind::SpreadProperty(spread) => {
                        walk_expr(&spread.argument, imports, unresolvable);
                    }
                }
            }
        }
        Expression::TemplateLiteral(tpl) => {
            for expr in &tpl.expressions {
                walk_expr(expr, imports, unresolvable);
            }
        }
        // Dynamic import expressions are already handled by ModuleRecord,
        // so we don't need to walk into ImportExpression here.
        _ => {}
    }
}

fn walk_argument(arg: &Argument<'_>, imports: &mut Vec<PositionedImport>, unresolvable: &mut usize) {
    if let Some(expr) = arg.as_expression() {
        walk_expr(expr, imports, unresolvable);
    } else if let Argument::SpreadElement(spread) = arg {
        walk_expr(&spread.argument, imports, unresolvable);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Parse TypeScript source and extract imports without touching the filesystem.
    fn parse_ts(source: &str) -> Vec<RawImport> {
        let source_type = SourceType::ts();
        extract_all(source, source_type)
            .expect("test source should parse")
            .imports
    }

    // --- Static imports ---

    #[test]
    fn static_named_import() {
        let imports = parse_ts(r#"import { foo } from "bar";"#);
        assert_eq!(imports.len(), 1);
        assert_eq!(imports[0].specifier, "bar");
        assert_eq!(imports[0].kind, EdgeKind::Static);
    }

    #[test]
    fn static_default_import() {
        let imports = parse_ts(r#"import foo from "bar";"#);
        assert_eq!(imports.len(), 1);
        assert_eq!(imports[0].specifier, "bar");
        assert_eq!(imports[0].kind, EdgeKind::Static);
    }

    #[test]
    fn static_reexport_named() {
        let imports = parse_ts(r#"export { foo } from "bar";"#);
        assert_eq!(imports.len(), 1);
        assert_eq!(imports[0].specifier, "bar");
        assert_eq!(imports[0].kind, EdgeKind::Static);
    }

    #[test]
    fn static_reexport_star() {
        let imports = parse_ts(r#"export * from "bar";"#);
        assert_eq!(imports.len(), 1);
        assert_eq!(imports[0].specifier, "bar");
        assert_eq!(imports[0].kind, EdgeKind::Static);
    }

    #[test]
    fn static_require() {
        let imports = parse_ts(r#"const x = require("bar");"#);
        assert_eq!(imports.len(), 1);
        assert_eq!(imports[0].specifier, "bar");
        assert_eq!(imports[0].kind, EdgeKind::Static);
    }

    // --- Type-only imports ---

    #[test]
    fn type_only_import() {
        let imports = parse_ts(r#"import type { Foo } from "bar";"#);
        assert_eq!(imports.len(), 1);
        assert_eq!(imports[0].specifier, "bar");
        assert_eq!(imports[0].kind, EdgeKind::TypeOnly);
    }

    #[test]
    fn type_only_all_specifiers() {
        let imports = parse_ts(r#"import { type Foo, type Bar } from "baz";"#);
        assert_eq!(imports.len(), 1);
        assert_eq!(imports[0].specifier, "baz");
        assert_eq!(imports[0].kind, EdgeKind::TypeOnly);
    }

    #[test]
    fn mixed_type_and_value_specifiers() {
        let imports = parse_ts(r#"import { type Foo, bar } from "baz";"#);
        assert_eq!(imports.len(), 1);
        assert_eq!(imports[0].specifier, "baz");
        assert_eq!(imports[0].kind, EdgeKind::Static);
    }

    #[test]
    fn type_only_reexport() {
        let imports = parse_ts(r#"export type { Foo } from "bar";"#);
        assert_eq!(imports.len(), 1);
        assert_eq!(imports[0].specifier, "bar");
        assert_eq!(imports[0].kind, EdgeKind::TypeOnly);
    }

    // --- Dynamic imports ---

    #[test]
    fn dynamic_await_import() {
        let imports = parse_ts(r#"const m = await import("bar");"#);
        assert_eq!(imports.len(), 1);
        assert_eq!(imports[0].specifier, "bar");
        assert_eq!(imports[0].kind, EdgeKind::Dynamic);
    }

    #[test]
    fn dynamic_import_expression_position() {
        // Bug #8: import().then() was missed because Expr::Member wasn't walked
        let imports = parse_ts(r#"import("./run-main.js").then(({ run }) => run());"#);
        assert_eq!(imports.len(), 1);
        assert_eq!(imports[0].specifier, "./run-main.js");
        assert_eq!(imports[0].kind, EdgeKind::Dynamic);
    }

    #[test]
    fn dynamic_import_in_arrow() {
        let imports = parse_ts(r#"const load = () => import("bar");"#);
        assert_eq!(imports.len(), 1);
        assert_eq!(imports[0].specifier, "bar");
        assert_eq!(imports[0].kind, EdgeKind::Dynamic);
    }

    #[test]
    fn require_in_if_block() {
        let imports = parse_ts(r#"if (cond) { const x = require("bar"); }"#);
        assert_eq!(imports.len(), 1);
        assert_eq!(imports[0].specifier, "bar");
        assert_eq!(imports[0].kind, EdgeKind::Static);
    }

    // --- Edge cases ---

    #[test]
    fn dynamic_import_in_try_catch() {
        let imports = parse_ts(
            r#"try { await import("bar"); } catch (e) { console.error(e); }"#,
        );
        assert_eq!(imports.len(), 1);
        assert_eq!(imports[0].specifier, "bar");
        assert_eq!(imports[0].kind, EdgeKind::Dynamic);
    }

    #[test]
    fn multiple_imports_all_kinds() {
        let imports = parse_ts(
            r#"
            import { a } from "static-dep";
            import type { B } from "type-dep";
            const c = await import("dynamic-dep");
            const d = require("require-dep");
            "#,
        );
        assert_eq!(imports.len(), 4);

        assert_eq!(imports[0].specifier, "static-dep");
        assert_eq!(imports[0].kind, EdgeKind::Static);

        assert_eq!(imports[1].specifier, "type-dep");
        assert_eq!(imports[1].kind, EdgeKind::TypeOnly);

        assert_eq!(imports[2].specifier, "dynamic-dep");
        assert_eq!(imports[2].kind, EdgeKind::Dynamic);

        assert_eq!(imports[3].specifier, "require-dep");
        assert_eq!(imports[3].kind, EdgeKind::Static);
    }

    // --- Negative cases (should NOT extract) ---

    #[test]
    fn require_resolve_not_extracted() {
        let imports = parse_ts(r#"const p = require.resolve("pkg");"#);
        assert!(imports.is_empty());
    }

    #[test]
    fn require_concatenated_arg_not_extracted() {
        let imports = parse_ts(r#"const x = require("base-" + "path");"#);
        assert!(imports.is_empty());
    }

    #[test]
    fn dynamic_import_variable_not_extracted() {
        let imports = parse_ts(r#"const p = "./foo"; const m = import(p);"#);
        assert!(imports.is_empty());
    }

    #[test]
    fn dynamic_import_template_with_expressions_not_extracted() {
        let imports = parse_ts(r#"const x = "foo"; const m = import(`./dir/${x}`);"#);
        assert!(imports.is_empty());
    }

    // --- Statement position coverage ---

    #[test]
    fn require_in_switch_case() {
        let imports = parse_ts(
            r#"switch (env) { case "a": const x = require("switch-dep"); break; }"#,
        );
        assert_eq!(imports.len(), 1);
        assert_eq!(imports[0].specifier, "switch-dep");
    }

    #[test]
    fn require_in_try_catch_finally() {
        let imports = parse_ts(
            r#"
            try { require("try-dep"); }
            catch (e) { require("catch-dep"); }
            finally { require("finally-dep"); }
            "#,
        );
        assert_eq!(imports.len(), 3);
        assert_eq!(imports[0].specifier, "try-dep");
        assert_eq!(imports[1].specifier, "catch-dep");
        assert_eq!(imports[2].specifier, "finally-dep");
    }

    // --- Expression position coverage ---

    #[test]
    fn require_in_spread_and_array() {
        let imports = parse_ts(
            r#"
            const merged = { ...require("spread-dep") };
            const arr = [require("array-dep")];
            "#,
        );
        assert_eq!(imports.len(), 2);
        assert_eq!(imports[0].specifier, "spread-dep");
        assert_eq!(imports[1].specifier, "array-dep");
    }

    #[test]
    fn import_in_sequence_expression() {
        let imports = parse_ts(r#"const x = (0, import("seq-dep"));"#);
        assert_eq!(imports.len(), 1);
        assert_eq!(imports[0].specifier, "seq-dep");
        assert_eq!(imports[0].kind, EdgeKind::Dynamic);
    }

    // --- Unresolvable dynamic imports ---

    #[test]
    fn dynamic_import_variable_unresolvable() {
        let source_type = SourceType::ts();
        let result = extract_all("const m = import(someVar);", source_type)
            .expect("test source should parse");
        assert_eq!(result.imports.len(), 0);
        assert_eq!(result.unresolvable_dynamic, 1);
    }

    #[test]
    fn require_variable_unresolvable() {
        let source_type = SourceType::mjs();
        let result = extract_all("const m = require(moduleName);", source_type)
            .expect("test source should parse");
        assert_eq!(result.imports.len(), 0);
        assert_eq!(result.unresolvable_dynamic, 1);
    }

    #[test]
    fn dynamic_import_literal_still_works_ts() {
        let source_type = SourceType::ts();
        let result = extract_all(r#"const m = import("./foo");"#, source_type)
            .expect("test source should parse");
        assert_eq!(result.imports.len(), 1);
        assert_eq!(result.imports[0].specifier, "./foo");
        assert_eq!(result.unresolvable_dynamic, 0);
    }
}
