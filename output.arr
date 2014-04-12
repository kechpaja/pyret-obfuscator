#lang pyret

import ast as A
import cmdline as C
import file as F
import "printer.arr" as P

O00O = 0
OO0O = "0"
O0OO = 1
OOOO = "O"
O000O = 2
OO00O = 0
O0O0O = "0"
OOO0O = 2
O00OO = "O"
OO0OO = 1
O0OOO = 2
OO000O = "O"
O00O0O = 1
OO0O0O = 1
OOOO0O = ""
O000OO = -1
OO00OO = -1
O0OOO0O = true
OOOOO0O = false
OO0OOOO = "Error"
O000OOOO = "jkl;"
O0OO0O00O = ""
O0O0O0O0O = "op=="
OOO0O0O0O = 8
O00OO0O0O = 3
OO0OO0O0O = 0
O0O0OO0OO = ""
O0O000OOO = ""
OO0OO0000O = "Missed case in obfs-expr: "
OOOO0O000O = "Problem!"
OOO0OO000O = ""
OO0OOO000O = false
fun O0O(OOO :: Number) -> String:   if 8  ==  3:     0
   else if OOO  ==  O00O:         OO0O
  else if OOO  ==  O0OO:         OOOO
  else if OOO.modulo(O000O)  ==  OO00O:         O0O0O  +  O0O(OOO  /  OOO0O)
  else:     O00OO  +  O0O((OOO  -  OO0OO)  /  O0OOO)
  end 
end
fun OOOOO(O0000O :: Number) -> String:   OO000O  +  O0O(O0000O)
end
O0O00O = (fun():   var OOO00O = O00O0O
  fun() -> String:     OOO00O := OOO00O  +  OO0O0O
    OOOOO(OOO00O)
  end
end)()
O0OO0O = A.loc(OOOO0O,O000OO,OO00OO)
data O0O0OO:   | OOO0OO(O00OOO :: String,OO0OOO :: String) end 
data O0OOOO:   | OOOOOO(O00000O :: A.Expr,OO0000O :: String) end 
var O0O000O = []
var OOO000O = []
fun O00O00O(OO0O00O :: String,O0OO00O :: List<O0O0OO>) -> String:   cases (List<O0O0OO>) O0OO00O:     | link(OOOO00O,O000O0O) =>         if 8  ==  3:           0
         else if OOOO00O.old  ==  OO0O00O:                     OOOO00O.new
        else:           O00O00O(OO0O00O,O000O0O)
        end 
    | empty() =>         OO0O00O
  end
end
fun OO00O0O(O0O0O0O :: List,OOO0O0O) -> Boolean:   cases (List) O0O0O0O:     | link(O00OO0O,OO0OO0O) =>         if 8  ==  3:           0
         else if O00OO0O  ==  OOO0O0O:                     O0OOO0O
        else:           OO00O0O(OO0OO0O,OOO0O0O)
        end 
    | empty() =>         OOOOO0O
  end
end
fun O0000OO(OO000OO :: String,O0O00OO :: String) -> Nothing:     when not OO00O0O(O0O000O,OO000OO):    O0O000O := link(OOO0OO(OO000OO,O0O00OO),O0O000O)
  end 
end
fun OOO00OO(O00O0OO :: String) -> String:   O00O00O(O00O0OO,O0O000O)
end
fun OO0O0OO(O0OO0OO :: A.Expr,OOOO0OO :: String) -> Nothing:     when not OO00O0O(OOO000O,O0OO0OO):    OOO000O := link(OOOOOO(O0OO0OO,OOOO0OO),OOO000O)
  end 
end
fun O000OOO(OO00OOO :: A.Expr,O0O0OOO :: List<O0OOOO>) -> String:   cases (List<O0OOOO>) O0O0OOO:     | link(OOO0OOO,O00OOOO) =>         if 8  ==  3:           0
         else if OOO0OOO.val  ==  OO00OOO:                     OOO0OOO.name
        else:           O000OOO(OO00OOO,O00OOOO)
        end 
    | empty() =>         raise(OO0OOOO)
  end
end
fun O0OOOOO(OOOOOOO :: A.Expr) -> String:   O000OOO(OOOOOOO,OOO000O)
end
fun O000000O(OO00000O :: List<A.Expr>) -> List<A.Expr>:   for fold(O0O0000O from OO00000O,OOO0000O from OOO000O):    link(A.s_let(O0OO0O,A.s_bind(O0OO0O,OOO0000O.name,A.a_blank),OOO0000O.val),O0O0000O)
  end
end
fun O00O000O(OO0O000O :: A.Header) -> A.Header:   cases (A.Header) OO0O000O:     | s_import(O0OO000O,OOOO000O,O000O00O) =>         OO0O000O
    | s_provide(OO00O00O,O0O0O00O) =>         OO0O000O
    | s_provide_all(OOO0O00O) =>         OO0O000O
  end
end
fun O00OO00O(OO0OO00O :: A.ImportType) -> A.ImportType:   cases (A.ImportType) OO0OO00O:     | s_file_import(O0OOO00O) =>         OO0OO00O
    | s_const_import(OOOOO00O) =>         OO0OO00O
  end
end
fun O0000O0O(OO000O0O :: A.Variant) -> A.Variant:   cases (A.Variant) OO000O0O:     | s_variant(O0O00O0O,OOO00O0O,O00O0O0O,OO0O0O0O) =>         O0000OO(OOO00O0O,O0O00O())
        A.s_variant(O0O00O0O,OOO00OO(OOO00O0O),O00O0O0O.map(obfs-variant-member),OO0O0O0O)
    | s_singleton_variant(O0OO0O0O,OOOO0O0O,O000OO0O) =>         O0000OO(OOOO0O0O,O0O00O())
        A.s_singleton_variant(O0OO0O0O,OOO00OO(OOOO0O0O),O000OO0O)
  end
end
fun OO00OO0O(O0O0OO0O :: A.VariantMember) -> A.VariantMember:   cases (A.VariantMember) O0O0OO0O:     | s_variant_member(OOO0OO0O,O00OOO0O,OO0OOO0O) =>         O0000OO(OO0OOO0O.id,O0O00O())
        A.s_variant_member(OOO0OO0O,O00OOO0O,A.s_bind(OO0OOO0O.l,OOO00OO(OO0OOO0O.id),OO0OOO0O.ann))
  end
end
fun O0OOOO0O(OOOOOO0O :: A.Member) -> A.Member:   cases (A.Member) OOOOOO0O:     | s_data_field(O00000OO,OO0000OO,O0O000OO) =>         OOOOOO0O
  end
end
fun OOO000OO(O00O00OO :: A.IfBranch) -> A.IfBranch:   cases (A.IfBranch) O00O00OO:     | s_if_branch(OO0O00OO,O0OO00OO,OOOO00OO) =>         A.s_if_branch(OO0O00OO,obfs-expr(O0OO00OO),obfs-expr(OOOO00OO))
  end
end
fun O000O0OO(OO00O0OO :: A.Ann) -> A.Ann:   cases (A.Ann) OO00O0OO:     | a_blank() =>         A.a_blank
    | a_any() =>         A.a_any
    | a_name(O0O0O0OO,OOO0O0OO) =>         A.a_name(O0O0O0OO,OOO00OO(OOO0O0OO))
    | a_arrow(O00OO0OO,OO0OO0OO,O0OOO0OO) =>         A.a_arrow(O00OO0OO,OO0OO0OO.map(O000O0OO),O000O0OO(O0OOO0OO))
    | a_record(OOOOO0OO,O0000OOO) =>         A.a_arrow(OOOOO0OO,for map(OO000OOO from O0000OOO):          O0000OO(OO000OOO.name,O0O00O())
          A.a_field(OO000OOO.l,OOO00OO(OO000OOO.name),O000O0OO(OO000OOO.ann))
        end)
    | a_app(O0O00OOO,OOO00OOO,O00O0OOO) =>         A.a_app(O0O00OOO,O000O0OO(OOO00OOO),O00O0OOO.map(O000O0OO))
    | a_pred(OO0O0OOO,O0OO0OOO,OOOO0OOO) =>         raise(O000OOOO)
    | a_dot(OO00OOOO,O0O0OOOO,OOO0OOOO) =>         A.a_dot(OO00OOOO,OOO00OO(O0O0OOOO),OOO00OO(OOO0OOOO))
  end
end
fun O00OOOOO(OO0OOOOO :: A.CasesBranch) -> A.CasesBranch:   cases (A.CasesBranch) OO0OOOOO:     | s_cases_branch(O0OOOOOO,OOOOOOOO,O0000000O,OO000000O) =>         A.s_cases_branch(O0OOOOOO,OOO00OO(OOOOOOOO),for map(O0O00000O from O0000000O):          O0000OO(O0O00000O.id,O0O00O())
          A.s_bind(O0O00000O.l,OOO00OO(O0O00000O.id),O000O0OO(O0O00000O.ann))
        end,obfs-expr(OO000000O))
  end
end
fun OOO00000O(O00O0000O :: A.Expr) -> A.Expr:   cases (A.Expr) O00O0000O:     | s_hint_exp(OO0O0000O,O0OO0000O,OOOO0000O) =>         A.s_hint_exp(OO0O0000O,O0OO0000O,OOO00000O(OOOO0000O))
    | s_block(O000O000O,OO00O000O) =>         A.s_block(O000O000O,OO00O000O.map(OOO00000O))
    | s_user_block(O0O0O000O,OOO0O000O) =>         A.s_user_block(O0O0O000O,OOO00000O(OOO0O000O))
    | s_fun(O00OO000O,OO0OO000O,O0OOO000O,OOOOO000O,O0000O00O,OO000O00O,O0O00O00O,OOO00O00O) =>         O0000OO(OO0OO000O,O0O00O())
        A.s_fun(O00OO000O,OOO00OO(OO0OO000O),for map(O00O0O00O from O0OOO000O):          O0000OO(O00O0O00O,O0O00O())
          OOO00OO(O00O0O00O)
        end,for map(OO0O0O00O from OOOOO000O):          O0000OO(OO0O0O00O.id,O0O00O())
          A.s_bind(OO0O0O00O.l,OOO00OO(OO0O0O00O.id),O000O0OO(OO0O0O00O.ann))
        end,O0000O00O,O0OO0O00O,OOO00000O(O0O00O00O),OOO00000O(OOO00O00O))
    | s_var(OOOO0O00O,O000OO00O,OO00OO00O) =>         O0000OO(O000OO00O.id,O0O00O())
        A.s_var(OOOO0O00O,A.s_bind(O000OO00O.l,OOO00OO(O000OO00O.id),O000OO00O.ann),OOO00000O(OO00OO00O))
    | s_let(O0O0OO00O,OOO0OO00O,O00OOO00O) =>         O0000OO(OOO0OO00O.id,O0O00O())
        A.s_let(O0O0OO00O,A.s_bind(OOO0OO00O.l,OOO00OO(OOO0OO00O.id),OOO0OO00O.ann),OOO00000O(O00OOO00O))
    | s_when(OO0OOO00O,O0OOOO00O,OOOOOO00O) =>         A.s_when(OO0OOO00O,OOO00000O(O0OOOO00O),OOO00000O(OOOOOO00O))
    | s_assign(O00000O0O,OO0000O0O,O0O000O0O) =>         A.s_assign(O00000O0O,OOO00OO(OO0000O0O),OOO00000O(O0O000O0O))
    | s_if(OOO000O0O,O00O00O0O) =>         A.s_if(OOO000O0O,for map(OO0O00O0O from O00O00O0O):          OOO000OO(OO0O00O0O)
        end)
    | s_if_else(O0OO00O0O,OOOO00O0O,O000O0O0O) =>         OO00O0O0O = [A.s_if_branch(O0OO00O0O,A.s_op(O0OO00O0O,O0O0O0O0O,A.s_num(O0OO00O0O,OOO0O0O0O),A.s_num(O0OO00O0O,O00OO0O0O)),A.s_block(O0OO00O0O,[A.s_num(O0OO00O0O,OO0OO0O0O)]))]
        A.s_if_else(O0OO00O0O,OO00O0O0O  +  (for map(O0OOO0O0O from OOOO00O0O):          OOO000OO(O0OOO0O0O)
        end),OOO00000O(O000O0O0O))
    | s_for(OOOOO0O0O,O0000OO0O,OO000OO0O,O0O00OO0O,OOO00OO0O) =>         A.s_for(OOOOO0O0O,OOO00000O(O0000OO0O),for map(O00O0OO0O from OO000OO0O):          cases (A.ForBind) O00O0OO0O:             | s_for_bind(OO0O0OO0O,O0OO0OO0O,OOOO0OO0O) =>                 O0000OO(O0OO0OO0O.id,O0O00O())
                A.s_for_bind(OO0O0OO0O,A.s_bind(O0OO0OO0O.l,OOO00OO(O0OO0OO0O.id),O0OO0OO0O.ann),OOO00000O(OOOO0OO0O))
          end
        end,O0O00OO0O,OOO00000O(OOO00OO0O))
    | s_cases(O000OOO0O,OO00OOO0O,O0O0OOO0O,OOO0OOO0O) =>         A.s_cases(O000OOO0O,O000O0OO(OO00OOO0O),OOO00000O(O0O0OOO0O),OOO0OOO0O.map(O00OOOOO))
    | s_cases_else(O00OOOO0O,OO0OOOO0O,O0OOOOO0O,OOOOOOO0O,O000000OO) =>         A.s_cases_else(O00OOOO0O,O000O0OO(OO0OOOO0O),OOO00000O(O0OOOOO0O),OOOOOOO0O.map(O00OOOOO),OOO00000O(O000000OO))
    | s_try(OO00000OO,O0O0000OO,OOO0000OO,O00O000OO) =>         O0000OO(OOO0000OO,O0O00O())
        A.s_try(OO00000OO,OOO00000O(O0O0000OO),OOO00OO(OOO0000OO),OOO00000O(O00O000OO))
    | s_op(OO0O000OO,O0OO000OO,OOOO000OO,O000O00OO) =>         A.s_op(OO0O000OO,O0OO000OO,OOO00000O(OOOO000OO),OOO00000O(O000O00OO))
    | s_check_test(OO00O00OO,O0O0O00OO,OOO0O00OO,O00OO00OO) =>         A.s_check_test(OO00O00OO,O0O0O00OO,OOO00000O(OOO0O00OO),OOO00000O(O00OO00OO))
    | s_not(OO0OO00OO,O0OOO00OO) =>         A.s_not(OO0OO00OO,OOO00000O(O0OOO00OO))
    | s_paren(OOOOO00OO,O0000O0OO) =>         A.s_paren(OOOOO00OO,OOO00000O(O0000O0OO))
    | s_lam(OO000O0OO,O0O00O0OO,OOO00O0OO,O00O0O0OO,OO0O0O0OO,O0OO0O0OO,OOOO0O0OO) =>         A.s_lam(OO000O0OO,for map(O000OO0OO from O0O00O0OO):          O0000OO(O000OO0OO,O0O00O())
          OOO00OO(O000OO0OO)
        end,for map(OO00OO0OO from OOO00O0OO):          O0000OO(OO00OO0OO.id,O0O00O())
          A.s_bind(OO00OO0OO.l,OOO00OO(OO00OO0OO.id),O000O0OO(OO00OO0OO.ann))
        end,O00O0O0OO,O0O0OO0OO,OOO00000O(O0OO0O0OO),OOO00000O(OOOO0O0OO))
    | s_method(OOO0OO0OO,O00OOO0OO,OO0OOO0OO,O0OOOO0OO,OOOOOO0OO,O00000OOO) =>         A.s_method(OOO0OO0OO,for map(OO0000OOO from O00OOO0OO):          O0000OO(OO0000OOO.id,O0O00O())
          A.s_bind(OO0000OOO.l,OOO00OO(OO0000OOO.id),O000O0OO(OO0000OOO.ann))
        end,OO0OOO0OO,O0O000OOO,OOO00000O(OOOOOO0OO),OOO00000O(O00000OOO))
    | s_extend(OOO000OOO,O00O00OOO,OO0O00OOO) =>         A.s_extend(OOO000OOO,OOO00000O(O00O00OOO),OO0O00OOO.map(O0OOOO0O))
    | s_obj(O0OO00OOO,OOOO00OOO) =>         A.s_obj(O0OO00OOO,OOOO00OOO.map(O0OOOO0O))
    | s_list(O000O0OOO,OO00O0OOO) =>         A.s_list(O000O0OOO,OO00O0OOO.map(OOO00000O))
    | s_app(O0O0O0OOO,OOO0O0OOO,O00OO0OOO) =>         A.s_app(O0O0O0OOO,OOO00000O(OOO0O0OOO),for map(OO0OO0OOO from O00OO0OOO):          OOO00000O(OO0OO0OOO)
        end)
    | s_id(O0OOO0OOO,OOOOO0OOO) =>         A.s_id(O0OOO0OOO,OOO00OO(OOOOO0OOO))
    | s_num(O0000OOOO,OO000OOOO) =>         OO0O0OO(O00O0000O,O0O00O())
        A.s_id(O0000OOOO,O0OOOOO(O00O0000O))
    | s_bool(O0O00OOOO,OOO00OOOO) =>         OO0O0OO(O00O0000O,O0O00O())
        A.s_id(O0O00OOOO,O0OOOOO(O00O0000O))
    | s_str(O00O0OOOO,OO0O0OOOO) =>         OO0O0OO(O00O0000O,O0O00O())
        A.s_id(O00O0OOOO,O0OOOOO(O00O0000O))
    | s_bracket(O0OO0OOOO,OOOO0OOOO,O000OOOOO) =>         A.s_bracket(O0OO0OOOO,OOO00000O(OOOO0OOOO),OOO00000O(O000OOOOO))
    | s_colon_bracket(OO00OOOOO,O0O0OOOOO,OOO0OOOOO) =>         A.s_colon_bracket(OO00OOOOO,OOO00000O(O0O0OOOOO),OOO00000O(OOO0OOOOO))
    | s_data(O00OOOOOO,OO0OOOOOO,O0OOOOOOO,OOOOOOOOO,O00000000O,OO0000000O,O0O000000O) =>         O0000OO(OO0OOOOOO,O0O00O())
        A.s_data(O00OOOOOO,OOO00OO(OO0OOOOOO),for map(OOO000000O from O0OOOOOOO):          O0000OO(OOO000000O,O0O00O())
          OOO00OO(OOO000000O)
        end,OOOOOOOOO.map(OOO00000O),O00000000O.map(O0000O0O),OO0000000O.map(O0OOOO0O),OOO00000O(O0O000000O))
    | s_dot(O00O00000O,OO0O00000O,O0OO00000O) =>         A.s_dot(O00O00000O,OOO00000O(OO0O00000O),O0OO00000O)
    | s_get_bang(OOOO00000O,O000O0000O,OO00O0000O) =>         A.s_get_bang(OOOO00000O,OOO00000O(O000O0000O),OOO00000O(OO00O0000O))
    | s_update(O0O0O0000O,OOO0O0000O,O00OO0000O) =>         A.s_update(O0O0O0000O,OOO00000O(OOO0O0000O),O00OO0000O.map(O0OOOO0O))
    | else =>         raise(OO0OO0000O  +  O00O0000O.torepr())
  end
end
fun O0OOO0000O(OOOOO0000O :: A.Program) -> A.Program:   cases (A.Program) OOOOO0000O:     | s_program(O0000O000O,OO000O000O,O0O00O000O) =>         OOO00O000O = OOO00000O(O0O00O000O)
        cases (A.Expr) OOO00O000O:           | s_block(O00O0O000O,OO0O0O000O) =>               A.s_program(O0000O000O,for map(O0OO0O000O from OO000O000O):                O00O000O(O0OO0O000O)
              end,A.s_block(O00O0O000O,O000000O(OO0O0O000O)))
          | else =>               raise(OOOO0O000O)
        end
  end
end
O000OO000O = F.input-file(C.args.first)
OO00OO000O = O000OO000O.read-file()
O000OO000O.close-file()
O0O0OO000O = P.str-prog(O0OOO0000O(A.parse(OO00OO000O,OOO0OO000O,{["check"] : true}).pre-desugar))
O00OOO000O = F.output-file(C.args.rest.first,OO0OOO000O)
O00OOO000O.display(O0O0OO000O)
O00OOO000O.close-file()
