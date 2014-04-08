#lang pyret

provide *

import ast as A


fun convert-op(op :: String) -> String: 
  if op == "opis":
    "is"
  else:
    raise("Not a real operation: " + op)
  end
end

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
  if n <= 0: "" else: n-spaces(n - 1) + "  " end
end

fun str-expr(expr :: A.Expr, depth :: Number) -> String:
  cases (A.Expr) expr:
    | s_hint_exp(l, hints, e) => 
        print(hints.first)
        ""
    | s_block(l, stmts) => 
        for fold(acc from "", s from stmts):
          acc + n-spaces(depth) + str-expr(s, depth) + "\n"
        end
    | s_user_block(l, ex) => 
        "block:\n" + str-expr(ex, depth + 1) + "\n" + n-spaces(depth) + "end\n"
    | s_fun(l, name, params, args, ann, doc, body, check) => 
        "fun " + name + "("
          + (for map(a from args): str-bind(a) end).join-str(",")
          + ")" + if A.is-a_any(ann) or A.is-a_blank(ann): 
                    "" 
                  else: 
                    " -> " + str-ann(ann) 
                  end
          + ":\n" + str-expr(body, depth + 1)
          + n-spaces(depth) + "end"
    | s_var(l, name, value) => 
        "var " + str-bind(name) + " = " + str-expr(value, depth)
    | s_let(l, name, value) => 
        str-bind(name) + " = " + str-expr(value, depth)
    | s_when(l, test, block) => 
        n-spaces(depth) + "when " + str-expr(test, depth) + ":\n"
          + str-expr(block, depth + 1)
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
    | s_try(l, body, id, _except) => "TODO" # TODO
    | s_check_test(l, op, left, right) => 
        str-expr(left, depth + 1) + " "
          + convert-op(op) + " " + str-expr(right, depth + 1)
    | s_lam(l, params, args, ann, doc, body, check) => 
        "fun(" + (for map(a from args): str-bind(a) end).join-str(",")
          + ")" + if A.is-a_blank(ann) or A.is-a_any(ann):
                    " -> " + str-ann(ann) 
                  else:
                    ""
                  end
          + "\n" + str-expr(body, depth + 1)
          + n-spaces(depth) + "end"
    | s_method(l, args, ann, doc, body, check) => 
        "fun(" + (for map(a from args): str-bind(a) end).join-str(",")
          + ")" + if A.is-a_blank(ann) or A.is-a_any(ann):
                    " -> " + str-ann(ann)
                  else:
                    ""
                  end
          + "\n" + str-expr(body, depth + 1)
          + n-spaces(depth) + "end"
    | s_extend(l, super, fields) => "EXTEND" 
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
    | s_dot(l, obj, field) =>
        str-expr(obj, depth) + "." + field
    | s_get_bang(l, obj, field) => str-expr(obj, depth) + "!" + field
    | s_update(l, super, fields) => 
        str-expr(super, depth) + "!" + "SOMETHING"
    | s_check(l, body) => 
        n-spaces(depth) + "check:\n" + str-expr(body, depth + 1)
          + n-spaces(depth) + "end"
    | else => raise("Missed case in printer: " + expr.to-repr())
  end
end

fun str-field(f :: A.Member, depth :: Number) -> String:
  cases (A.Member) f:
    | s_data_field(l, name, value) => 
        str-expr(name, depth) + " : " + str-expr(value, depth)
    | s_mutable_field(l, name, ann, value) => 
    | s_once_field(l, name, ann, value) => 
    | s_method_field(l, name, args, ann, doc, body, check) => 
  end
end

fun str-ann(ann :: A.Ann) -> String: 
  cases (A.Ann) ann: 
    | a_blank => ""
    | a_any => ""
    | a_name(l, id) => id
    | a_arrow(l, args, ret) => 
        "(" + args.map(str-ann).join-str(",") + ") -> " + str-ann(ret)
    | a_method(l, args, ret) => "METHOD"
    | a_record(l, fields) => "RECORD"
    | a_app(l, an, args) => "APP"
    | a_pred(l, an, expr) => "PRED"
    | a_dot(l, obj, field) => "DOT"
  end
end

fun str-bind(bind :: A.Bind) -> String: 
  bind.id + if A.is-a_blank(bind.ann) or A.is-a_any(bind.ann):
              ""
            else:
              " : " + str-ann(bind.ann)
            end
end


fun test-func(prog :: String) -> String: 
  str-prog(A.parse(prog, "", {check : true}).pre-desugar)
end


##print(test-func("fun f(x): x end\nf(8)\ncheck: f(8) is 8\nend"))
