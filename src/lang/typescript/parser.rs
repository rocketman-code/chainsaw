use std::path::Path;
use std::sync::Arc;
use swc_common::SourceMap;
use swc_ecma_ast::{
    Callee, EsVersion, Expr, ExportSpecifier, ImportSpecifier, Lit, Module, ModuleDecl,
    ModuleItem, Stmt,
};
use swc_ecma_parser::{parse_file_as_module, EsSyntax, Syntax, TsSyntax};

use crate::graph::EdgeKind;
use crate::lang::RawImport;

/// Convert a WTF-8 atom (from SWC's Str.value) to a String.
/// Import specifiers are always valid UTF-8, so lossy conversion is safe.
fn wtf8_to_string(atom: &swc_ecma_ast::Str) -> String {
    atom.value.to_string_lossy().into_owned()
}

fn syntax_for_path(path: &Path) -> Syntax {
    match path.extension().and_then(|e| e.to_str()) {
        Some("ts") => Syntax::Typescript(TsSyntax {
            tsx: false,
            decorators: true,
            ..Default::default()
        }),
        Some("tsx") => Syntax::Typescript(TsSyntax {
            tsx: true,
            decorators: true,
            ..Default::default()
        }),
        Some("jsx") => Syntax::Es(EsSyntax {
            jsx: true,
            ..Default::default()
        }),
        // .js, .mjs, .cjs, and anything else: parse as JS
        _ => Syntax::Es(EsSyntax {
            jsx: false,
            ..Default::default()
        }),
    }
}

pub fn parse_file(path: &Path) -> Result<Vec<RawImport>, String> {
    let cm = Arc::<SourceMap>::default();
    let fm = cm
        .load_file(path)
        .map_err(|e| format!("Failed to read {}: {e}", path.display()))?;

    let syntax = syntax_for_path(path);
    let mut errors = vec![];

    let module = parse_file_as_module(&fm, syntax, EsVersion::EsNext, None, &mut errors)
        .map_err(|e| format!("Parse error in {}: {e:?}", path.display()))?;

    Ok(extract_imports(&module))
}

fn extract_imports(module: &Module) -> Vec<RawImport> {
    let mut imports = Vec::new();

    for item in &module.body {
        match item {
            ModuleItem::ModuleDecl(decl) => extract_from_decl(decl, &mut imports),
            ModuleItem::Stmt(stmt) => walk_stmt(stmt, &mut imports),
        }
    }

    imports
}

fn extract_from_decl(decl: &ModuleDecl, imports: &mut Vec<RawImport>) {
    match decl {
        // import { x } from "y"  /  import type { x } from "y"
        ModuleDecl::Import(import_decl) => {
            let kind = if import_decl.type_only {
                EdgeKind::TypeOnly
            } else {
                // Check if ALL specifiers are type-only
                let all_type = !import_decl.specifiers.is_empty()
                    && import_decl.specifiers.iter().all(|s| match s {
                        ImportSpecifier::Named(n) => n.is_type_only,
                        _ => false,
                    });
                if all_type {
                    EdgeKind::TypeOnly
                } else {
                    EdgeKind::Static
                }
            };
            imports.push(RawImport {
                specifier: wtf8_to_string(&import_decl.src),
                kind,
            });
        }

        // export * from "y"  /  export type * from "y"
        ModuleDecl::ExportAll(export_all) => {
            let kind = if export_all.type_only {
                EdgeKind::TypeOnly
            } else {
                EdgeKind::Static
            };
            imports.push(RawImport {
                specifier: wtf8_to_string(&export_all.src),
                kind,
            });
        }

        // export { x } from "y"  /  export type { x } from "y"
        ModuleDecl::ExportNamed(named) => {
            if let Some(src) = &named.src {
                let kind = if named.type_only {
                    EdgeKind::TypeOnly
                } else {
                    let all_type = !named.specifiers.is_empty()
                        && named.specifiers.iter().all(|s| matches!(
                            s,
                            ExportSpecifier::Named(n) if n.is_type_only
                        ));
                    if all_type {
                        EdgeKind::TypeOnly
                    } else {
                        EdgeKind::Static
                    }
                };
                imports.push(RawImport {
                    specifier: wtf8_to_string(src),
                    kind,
                });
            }
        }

        _ => {}
    }
}

/// Recursively walk statements to find require() and import() calls.
fn walk_stmt(stmt: &Stmt, imports: &mut Vec<RawImport>) {
    match stmt {
        Stmt::Expr(expr_stmt) => walk_expr(&expr_stmt.expr, imports),
        Stmt::Decl(decl) => walk_decl(decl, imports),
        Stmt::Block(block) => {
            for s in &block.stmts {
                walk_stmt(s, imports);
            }
        }
        Stmt::If(if_stmt) => {
            walk_stmt(&if_stmt.cons, imports);
            if let Some(alt) = &if_stmt.alt {
                walk_stmt(alt, imports);
            }
        }
        Stmt::Switch(switch) => {
            walk_expr(&switch.discriminant, imports);
            for case in &switch.cases {
                for s in &case.cons {
                    walk_stmt(s, imports);
                }
            }
        }
        Stmt::Try(try_stmt) => {
            for s in &try_stmt.block.stmts {
                walk_stmt(s, imports);
            }
            if let Some(catch) = &try_stmt.handler {
                for s in &catch.body.stmts {
                    walk_stmt(s, imports);
                }
            }
            if let Some(finalizer) = &try_stmt.finalizer {
                for s in &finalizer.stmts {
                    walk_stmt(s, imports);
                }
            }
        }
        Stmt::While(while_stmt) => {
            walk_stmt(&while_stmt.body, imports);
        }
        Stmt::DoWhile(do_while) => {
            walk_stmt(&do_while.body, imports);
        }
        Stmt::For(for_stmt) => {
            walk_stmt(&for_stmt.body, imports);
        }
        Stmt::ForIn(for_in) => {
            walk_stmt(&for_in.body, imports);
        }
        Stmt::ForOf(for_of) => {
            walk_stmt(&for_of.body, imports);
        }
        Stmt::Return(ret) => {
            if let Some(arg) = &ret.arg {
                walk_expr(arg, imports);
            }
        }
        Stmt::Labeled(labeled) => {
            walk_stmt(&labeled.body, imports);
        }
        _ => {}
    }
}

fn walk_decl(decl: &swc_ecma_ast::Decl, imports: &mut Vec<RawImport>) {
    match decl {
        swc_ecma_ast::Decl::Var(var_decl) => {
            for decl in &var_decl.decls {
                if let Some(init) = &decl.init {
                    walk_expr(init, imports);
                }
            }
        }
        swc_ecma_ast::Decl::Fn(fn_decl) => {
            if let Some(body) = &fn_decl.function.body {
                for s in &body.stmts {
                    walk_stmt(s, imports);
                }
            }
        }
        _ => {}
    }
}

fn walk_expr(expr: &Expr, imports: &mut Vec<RawImport>) {
    match expr {
        Expr::Call(call) => {
            // Dynamic import()
            if let Callee::Import(_) = &call.callee {
                if let Some(arg) = call.args.first()
                    && let Expr::Lit(Lit::Str(s)) = &*arg.expr
                {
                    imports.push(RawImport {
                        specifier: wtf8_to_string(s),
                        kind: EdgeKind::Dynamic,
                    });
                }
                return;
            }
            // require("...")
            if let Callee::Expr(callee_expr) = &call.callee {
                if let Expr::Ident(ident) = &**callee_expr
                    && ident.sym.as_str() == "require"
                {
                    if let Some(arg) = call.args.first()
                        && let Expr::Lit(Lit::Str(s)) = &*arg.expr
                    {
                        imports.push(RawImport {
                            specifier: wtf8_to_string(s),
                            kind: EdgeKind::Static,
                        });
                    }
                    return;
                }
                walk_expr(callee_expr, imports);
            }
            for arg in &call.args {
                walk_expr(&arg.expr, imports);
            }
        }
        Expr::Arrow(arrow) => match &*arrow.body {
            swc_ecma_ast::BlockStmtOrExpr::BlockStmt(block) => {
                for s in &block.stmts {
                    walk_stmt(s, imports);
                }
            }
            swc_ecma_ast::BlockStmtOrExpr::Expr(e) => walk_expr(e, imports),
        },
        Expr::Fn(fn_expr) => {
            if let Some(body) = &fn_expr.function.body {
                for s in &body.stmts {
                    walk_stmt(s, imports);
                }
            }
        }
        Expr::Assign(assign) => {
            walk_expr(&assign.right, imports);
        }
        Expr::Seq(seq) => {
            for e in &seq.exprs {
                walk_expr(e, imports);
            }
        }
        Expr::Paren(paren) => walk_expr(&paren.expr, imports),
        Expr::Await(await_expr) => walk_expr(&await_expr.arg, imports),
        Expr::Cond(cond) => {
            walk_expr(&cond.test, imports);
            walk_expr(&cond.cons, imports);
            walk_expr(&cond.alt, imports);
        }
        Expr::Bin(bin) => {
            walk_expr(&bin.left, imports);
            walk_expr(&bin.right, imports);
        }
        Expr::Unary(unary) => walk_expr(&unary.arg, imports),
        Expr::Member(member) => walk_expr(&member.obj, imports),
        Expr::Array(array) => {
            for elem in array.elems.iter().flatten() {
                walk_expr(&elem.expr, imports);
            }
        }
        Expr::Object(object) => {
            for prop in &object.props {
                match prop {
                    swc_ecma_ast::PropOrSpread::Prop(p) => {
                        if let swc_ecma_ast::Prop::KeyValue(kv) = &**p {
                            walk_expr(&kv.value, imports);
                        }
                    }
                    swc_ecma_ast::PropOrSpread::Spread(spread) => {
                        walk_expr(&spread.expr, imports);
                    }
                }
            }
        }
        Expr::Tpl(tpl) => {
            for expr in &tpl.exprs {
                walk_expr(expr, imports);
            }
        }
        _ => {}
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Parse TypeScript source and extract imports without touching the filesystem.
    fn parse_ts(source: &str) -> Vec<RawImport> {
        let cm = Arc::<SourceMap>::default();
        let fm = cm.new_source_file(
            swc_common::FileName::Custom("test.ts".into()).into(),
            source.to_string(),
        );
        let syntax = Syntax::Typescript(TsSyntax {
            tsx: false,
            decorators: true,
            ..Default::default()
        });
        let mut errors = vec![];
        let module = parse_file_as_module(&fm, syntax, EsVersion::EsNext, None, &mut errors)
            .expect("test source should parse");
        extract_imports(&module)
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
}
