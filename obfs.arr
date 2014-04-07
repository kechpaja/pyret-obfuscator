#lang pyret

import ast as A

fun n-underscores(n :: Number) -> String: 
  if n <= 2: "__" else: n-underscores(n - 1) + "_" end
end

# Functions to create new identifiers. They will be as visually similar 
# to one another as possible. Normal obfuscators usually try and replace
# everything with A, but I think it makes more sense to use underscores
# here, especially since a single underscore is a reserved word (making
# a program all of whose identifiers are underscores that much harder
# for a human to read). 
next-name = (fun() -> (() -> String):
  var n = 1
  fun() -> String:
    n := n + 1
    n-underscores(n)
  end
end)()


data Subst:
  new-sub(old :: String, new :: String)
end


fun replace(id :: String) -> String: 
  replace-from-subst(id, subs)
end


fun replace-from-susbt(id :: String, s :: List<Subst>) -> String: 
  cases (List<Subst>) s:
    | link(f, r) => if f.old == id: f.new else: replace-from-subst(id, r) end
    | empty => raise("Error: can't replace unbound ID " + id)
  end
end


fun obfs-prog(prog :: A.Program) -> A.Program:
  cases (A.Program) prog: 
    | s_program(l, imports, block) => 
        s_program(l, 
                  for map(i from imports): obfs-header(i) end, 
                  obfs-expr(block))
                  # TODO probably takes a map as argument as well
  end
end

fun obfs-header(header :: A.Header) -> A.Header:
  cases (A.Header) header:
    | s_import(l, file, name) => 
    | s_provide(l, block) => 
    | s_provide_all(l) => 
  end
end

fun obfs-import-type(it :: A.ImportType) -> A.ImportType:
  cases (A.ImportType) it: 
    | s_file_import(file) => # TODO
    | s_const_import(module) => # TODO
  end
end


# TODO make sure that we don't assume names from imports are defined here.
# We shouldn't replace the field names in lookups, unless they are defined in
# the current program, and won't be used elsewhere.
# Or maybe we should just assume that the obfuscator runs on a complete
# program, and any problems that that causes can be dealt with later. 



# TODO this program will work much like filter-lets in the compiler. 
# It will need to pass a substitution set along with each call, and replace
# identifiers as it encounters them. 
fun obfs-expr(expr :: A.Expr) -> A.Expr:
  cases (A.Expr) expr: 
    | s_hint_expr(l, hints, e) => # TODO
    | s_block(l, stmts) => A.s_block(l, stmts.map(obfs-expr))
    | s_user_block(l, ex) => A.s_user_block(l, obfs-expr(ex))
    | s_var(l, name, value) => # TODO
    | s_let(l, name, value) => # TODO
    | s_assign(l, id, value) => 
        A.s_assign(l, replace(id), 
    | s_if_else(l, branches, _else) => # TODO
    | s_try(l, body, id, _except) => # TODO
    | s_lam(l, params, args, ann, doc, body, check) => # TODO
    | s_method(l, args, ann, doc, body, check) => # TODO
    | s_extend(l, super, fields) => 
        A.s_extend(l, obfs-expr(super), fields.map(obfs-field))
    | s_obj(l, fields) => 
        A.s_obj(l, fields.map(obfs-field))
    | s_app(l, _fun, args) => # TODO
    | s_id(l, id) => A.s_id(l, replace(id))
    | s_num(l, n) => expr # TODO lift numbers and strings and such? 
    | s_bool(l, b) => expr
    | s_str(l, s) => # TODO
    | s_bracket(l, obj, field) => # TODO
    | s_colon_bracket(l, obj, field) => # TODO
    | s_get_bang(l, obj, field) => # TODO
    | s_update(l, super, fields) => # TODO
    | else => raise("Missed case in obfs-expr: " + expr.to-repr())
  end
end


fun obfs-field(field :: A.Member) -> A.Member:
  # TODO
end
