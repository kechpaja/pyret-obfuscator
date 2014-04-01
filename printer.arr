#lang pyret

import ast as A

fun str-prog(prog :: A.Program) -> String:
  cases (A.Program) prog: 
    | s_program(l, imports, block) => 
        for fold(s from "", h from imports): 
          s + str-header(h) + "\n"
        end + "\n" + str-expr(block, 0)
  end
end

fun str-header(header :: A.Header) -> String:
  cases (A.Header) header:
    | s_import(l, file, name) => 
        "import " + str-import-type(file) + " as " + name
    | s_provide(l, block) => "provide " + str-expr(block, 0)
    | s_provide_all(l) => "provide *"
  end
end

fun str-import-type(it :: A.ImportType) -> String: 
  cases (A.ImportType) it:
    | s_file_import(file) => file
    | s_const_import(module) => module
  end
end

fun n-spaces(n :: Number) -> String: 
  if n <= 0: "" else n-spaces(n - 1) + "  " end
end

fun str-expr(expr :: A.Expr, depth :: Number) -> String:
  cases (A.Expr) expr:
    | s_hint_expr(l, hints, e) => # TODO
    | s_block(l, stmts) => 
        for fold(acc from "", s from stmts):
          acc + n-spaces(depth) + str-expr(s, depth)
        end
    | s_user_block(l, ex) => 
        "block:\n" + str-expr(ex, depth + 1) + "\n" + n-spaces(depth) + "end\n"
    | s_var(l, name, value) => 
        "var " + str-bind(name) + " = " + str-expr(value, depth)
    | s_let(l, name, value) => 
        str-bind(name) + " = " + str-expr(value, depth) 
    | s_assign(l, id, value) => 
        id + " := " + str-expr(value, depth)
    | s_if_else(l, branches, _else) => 
        sstr = n-spaces(depth) + "if " + str-expr(branches.first.test, depth)
          + ":\n" + n-spaces(depth + 1) 
          + str-expr(branches.first.body, depth + 1) + "\n"
        for fold(acc from sstr, b from branches.rest):
          acc + n-spaces(depth) + "else if " + str-expr(b.test, depth) + ":\n"
            + n-spaces(depth + 1) + str-expr(b.body, depth + 1) + "\n"
        end + n-spaces(depth) + "else:\n" + n-spaces(depth + 1) 
          + str-expr(_else, depth + 1) + n-spaces(depth) + "end\n"
    | s_try(l, body, id, _except) =>
    | s_lam(l, params, args, ann, doc, body, check) => 
        "fun(" + () + ") -> " + str-ann(ann) + "\n"
          + str-expr(body, depth + 1)
          + n-spaces(depth) + "end"
    | s_method(l, args, ann, doc, body, check) => 
    | s_extend(l, super, fields) => 
    | s_obj(l, fields) => 
        "{" + (for map(f from fields): 
                 str-field(f, depth) 
               end).join-str(",") + "}"
    | s_app(l, _fun, args) => 
        str-expr(_fun, depth) + "(" 
          + (for map(a from args): str-expr(a, depth) end).join-str(",") + ")"
    | s_id(l, id) => id
    | s_num(l, n) => n.tostring()
    | s_bool(l, b) => b.tostring()
    | s_str(l, s) => "\"" + s + "\""
    | s_bracket(l, obj, field) => 
        str-expr(obj, depth) + ".[" + str-expr(field, depth) + "]"
    | s_colon_bracket(l, obj, field) => 
        str-expr(obj, depth) + ":[" + str-expr(field, depth) + "]"
    | s_get_bang(l, obj, field) => str-expr(obj, depth) + "!" + field
    | s_update(l, super, fields) => 
        str-expr(super, depth) + "!" + 
    | else => raise("Missed case in printer: " + expr.to-repr())
  end
end

fun str-field(f :: A.Field, depth :: Number) -> String:
  # TODO
end

fun str-ann(ann :: A.Ann) -> String: 
  cases (A.Ann) ann: 
    | a_blank => ""
    | a_any => ""
    | a_name(l, id) => id
    | a_arrow(l, args, ret) => 
    | a_method(l, args, ret) => 
    | a_record(l, fields) => 
    | a_app(l, an, args) => 
    | a_pred(l, an, expr) => 
    | a_dot(l, obj, field) => 
  end
end

fun str-bind(bind :: A.Bind) -> String: 
  bind.id # TODO this is good enough for now
end
