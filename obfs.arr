#lang pyret

import ast as A
import cmdline as C
import file as F
import "printer.arr" as P

fun n-underscores(n :: Number) -> String: 
  if n <= 2: "__" else: n-underscores(n - 1) + "_" end
end

# Functions to create new identifiers. They will be as visually similar 
# to one another as possible. Normal obfuscators usually try and replace
# everything with A, but I think it makes more sense to use underscores
# here, especially since a single underscore is a reserved word (making
# a program all of whose identifiers are underscores that much harder
# for a human to read). 
next-name = (fun():
  var n = 1
  fun() -> String:
    n := n + 1
    n-underscores(n)
  end
end)()

default-loc = A.loc("", -1, -1)

data Subst:
  | new-sub(old :: String, new :: String)
end

data Let:
  | new-let(val :: A.Expr, name :: String)
end

# Mutable Globals
var subs = []
var lets = []


fun update-subs(old :: String, new :: String) -> Nothing: 
  when not contains(subs, old):
    subs := link(new-sub(old, new), subs)
  end
end

fun replace(id :: String) -> String: 
  replace-from-subst(id, subs)
end

fun replace-from-subst(id :: String, s :: List<Subst>) -> String: 
  cases (List<Subst>) s:
    | link(f, r) => if f.old == id: f.new else: replace-from-subst(id, r) end
    | empty => id
    #raise("Error: can't replace unbound ID " + id)
  end
end

fun contains(lst :: List, s) -> Boolean: 
  cases (List) lst: 
    | link(f, r) => if f == s: true else: contains(r, s) end
    | empty => false
  end
end

# We are going to lift strings and numbers as well. 
fun update-lets(value :: A.Expr, name :: String) -> Nothing: 
  when not contains(lets, value):
    lets := link(new-let(value, name), lets)
  end
end

fun replace-literal(val :: A.Expr) -> String:
  replace-literal-from-lets(val, lets)
end

fun replace-literal-from-lets(val :: A.Expr, l :: List<Let>) -> String:
  cases (List<Let>) l: 
    | link(f, r) => 
        if f.val == val: f.name else: replace-literal-from-lets(val, r) end
    | empty => raise("Error")
  end
end

fun add-lifted-stuff(stmts :: List<A.Expr>) -> List<A.Expr>:
  for fold(s from stmts, l from lets):
    link(A.s_let(default-loc, 
                 A.s_bind(default-loc, l.name, A.a_blank), 
                 l.val), 
         s)
  end
end




fun obfs-prog(prog :: A.Program) -> A.Program:
  cases (A.Program) prog: 
    | s_program(l, imports, block) => 
        expr = obfs-expr(block)
        cases (A.Expr) expr:
          | s_block(lo, stmts) => 
                A.s_program(l, 
                            for map(i from imports): obfs-header(i) end, 
                            A.s_block(lo, add-lifted-stuff(stmts)))
                            # TODO probably takes a map as argument as well
          | else => raise("Problem!")
        end
  end
end

fun obfs-header(header :: A.Header) -> A.Header:
  cases (A.Header) header:
    | s_import(l, file, name) => header
    | s_provide(l, block) => header
    | s_provide_all(l) => header
  end
end

fun obfs-import-type(it :: A.ImportType) -> A.ImportType:
  cases (A.ImportType) it: 
    | s_file_import(file) => it # TODO
    | s_const_import(module) => it # TODO
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
    | s_hint_exp(l, hints, e) => 
        A.s_hint_exp(l, hints, obfs-expr(e))
    | s_block(l, stmts) => A.s_block(l, stmts.map(obfs-expr))
    | s_user_block(l, ex) => A.s_user_block(l, obfs-expr(ex))
    | s_fun(l, name, params, args, ann, doc, body, ch) => 
        update-subs(name, next-name())
        A.s_fun(l, 
                replace(name),
                for map(p from params): 
                  update-subs(p, next-name())
                  replace(p)
                end,
                for map(a from args):
                  update-subs(a.id, next-name())
                  A.s_bind(a.l, replace(a.id), obfs-ann(a.ann))
                end,
                ann, 
                "",
                obfs-expr(body),
                obfs-expr(ch))
    | s_var(l, name, value) =>
        update-subs(name.id, next-name())
        A.s_var(l, 
                A.s_bind(name.l, replace(name.id), name.ann), 
                obfs-expr(value))
    | s_let(l, name, value) => 
        update-subs(name.id, next-name())
        A.s_let(l, 
                A.s_bind(name.l, replace(name.id), name.ann), 
                obfs-expr(value))
    | s_when(l, test, block) => 
        A.s_when(l, obfs-expr(test), obfs-expr(block))
    | s_assign(l, id, value) => 
        A.s_assign(l, replace(id), obfs-expr(value))
    | s_if(l, branches) => 
        A.s_if(l, for map(b from branches): obfs-branch(b) end)
    | s_if_else(l, branches, _else) => 
        A.s_if_else(l, 
                    for map(b from branches): obfs-branch(b) end,
                    obfs-expr(_else))
    | s_for(l, _fun, binds, ann, ex) => 
        A.s_for(l,
                obfs-expr(_fun),
                for map(b from binds):
                  cases (A.ForBind) b:
                    | s_for_bind(lo, arg, col) => 
                        update-subs(arg.id, next-name())
                        A.s_for_bind(lo, 
                                     A.s_bind(arg.l, replace(arg.id), arg.ann),
                                     obfs-expr(col))
                  end
                end,
                ann,
                obfs-expr(ex))
    | s_cases(l, type, val, branches) => 
        A.s_cases(l, 
                  obfs-ann(type), 
                  obfs-expr(val), 
                  branches.map(obfs-case-branch))
    | s_cases_else(l, type, val, branches, _else) => 
        A.s_cases_else(l, 
                       obfs-ann(type), 
                       obfs-expr(val), 
                       branches.map(obfs-case-branch),
                       obfs-expr(_else))
    | s_try(l, body, id, _except) => 
        update-subs(id, next-name())
        A.s_try(l, obfs-expr(body), replace(id), obfs-expr(_except))
    | s_op(l, op, left, right) => 
        A.s_op(l, op, obfs-expr(left), obfs-expr(right))
    | s_check_test(l, op, left, right) => 
        A.s_check_test(l, op, obfs-expr(left), obfs-expr(right))
    | s_not(l, ex) => 
        A.s_not(l, obfs-expr(ex))
    | s_paren(l, ex) => 
        A.s_paren(l, obfs-expr(ex))
    | s_lam(l, params, args, ann, doc, body, ch) => 
        A.s_lam(l,
                for map(p from params):
                  update-subs(p, next-name())
                  replace(p)
                end,
                for map(a from args): 
                  update-subs(a.id, next-name())
                  A.s_bind(a.l, replace(a.id), obfs-ann(a.ann))
                end,
                ann,
                "",
                obfs-expr(body),
                obfs-expr(ch))
    | s_method(l, args, ann, doc, body, ch) => 
        A.s_method(l, 
                   for map(a from args):
                     update-subs(a.id, next-name())
                     A.s_bind(a.l, replace(a.id), obfs-ann(a.ann))
                   end,
                   ann,
                   "",
                   obfs-expr(body),
                   obfs-expr(ch))
    | s_extend(l, super, fields) => 
        A.s_extend(l, obfs-expr(super), fields.map(obfs-field))
    | s_obj(l, fields) => 
        A.s_obj(l, fields.map(obfs-field))
    | s_list(l, values) => 
        A.s_list(l, values.map(obfs-expr))
    | s_app(l, _fun, args) =>
        A.s_app(l, obfs-expr(_fun), for map(a from args): obfs-expr(a) end)
    | s_id(l, id) => A.s_id(l, replace(id))
    | s_num(l, n) => 
        update-lets(expr, next-name()) 
        A.s_id(l, replace-literal(expr))
    | s_bool(l, b) => 
        update-lets(expr, next-name())
        A.s_id(l, replace-literal(expr))
    | s_str(l, s) => 
        update-lets(expr, next-name())
        A.s_id(l, replace-literal(expr))
    | s_bracket(l, obj, field) => 
        A.s_bracket(l, obfs-expr(obj), obfs-expr(field))
    | s_colon_bracket(l, obj, field) => 
        A.s_colon_bracket(l, obfs-expr(obj), obfs-expr(field))
    | s_data(l, name, params, mixins, variants, shared_members, ch) => 
        update-subs(name, next-name())
        A.s_data(l,
                 replace(name),
                 for map(p from params): 
                   update-subs(p, next-name())
                   replace(p)
                 end,
                 mixins.map(obfs-expr),
                 variants, # TODO replace this
                 shared_members.map(obfs-field),
                 obfs-expr(ch))
    | s_dot(l, obj, field) => 
        A.s_dot(l, obfs-expr(obj), field)
    | s_get_bang(l, obj, field) => 
        A.s_get_bang(l, obfs-expr(obj), obfs-expr(field))
    | s_update(l, super, fields) => 
        A.s_update(l, obfs-expr(super), fields.map(obfs-field))
    | else => raise("Missed case in obfs-expr: " + expr.torepr())
  end
end


fun obfs-field(field :: A.Member) -> A.Member:
  cases (A.Member) field: 
    | s_data_field(l, name, value) => field
  end
end

fun obfs-branch(branch :: A.IfBranch) -> A.IfBranch:
  cases (A.IfBranch) branch:
    | s_if_branch(l, test, body) => 
        A.s_if_branch(l, obfs-expr(test), obfs-expr(body))
  end
end

fun obfs-ann(ann :: A.Ann) -> A.Ann:
  cases (A.Ann) ann:
    | a_blank => A.a_blank
    | a_any => A.a_any
    | a_name(l, id) => A.a_name(l, replace(id))
    | a_arrow(l, args, ret) => 
    | a_record(l, fields) => 
    | a_app(l, an, args) => 
    | a_pred(l, an, expr) =>
    | a_dot(l, obj, field) => 
  end
end

fun obfs-case-branch(branch :: A.CasesBranch) -> A.CasesBranch:
  cases (A.CasesBranch) branch: 
    | s_cases_branch(l, name, args, body) => 
        A.s_cases_branch(l, 
                         replace(name), 
                         for map(a from args):
                           update-subs(a.id, next-name())
                           A.s_bind(a.l, replace(a.id), a.ann)
                         end,
                         obfs-expr(body))
  end
end



###########################
#                         #
# Handle File IO et al... #
#                         #
###########################


infile = F.input-file(C.args.first)
program = infile.read-file()
infile.close-file()


result = P.str-prog(obfs-prog(A.parse(program, "", {check : true}).pre-desugar))

outfile = F.output-file(C.args.rest.first, false)
outfile.display(result)
outfile.close-file()
