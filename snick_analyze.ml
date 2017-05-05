open Snick_ast
open Snick_symbol

let isValid = ref true

(*i guess we dont need to use to return result of gen_sym_table, we could just update it in Snick_symbol.ml*)
let rec analyse prog =
    gen_sym_table prog;
    List.iter analyse_proc prog;
    check_unused_symbols prog;    (*???????*)
    check_main                    (*???????*)

and analyse_proc ((proc_id,_),prog_body) =
    let scope = Hashtbl.find ht_scopes proc_id
    in
    let scope_st = get_scope_st scope 
    in
    analyse_statements scope_st prog_body.stmts

and analyse_statements scope_st stmts =
    List.iter (analyse_statement scope_st) stmts

and analyse_statement scope_st stmt = match stmt with
    | Assign(elem,expr) ->
        let l_type = get_elem_type scope_st elem
        and r_type = get_expr_type scope_st expr
        in
            if l_type = r_type then
                ()
            else
                raise (Failure "type unmatch!")
    | Read(_) -> analyse_read scope_st stmt
    | Write(_) -> analyse_write scope_st stmt
    | Call(_) -> analyse_call scope_st stmt
    | If_then(_) -> analyse_if_then scope_st stmt
    | If_then_else(_) -> analyse_if_then_else scope_st stmt
    | While(_) -> analyse_while scope_st stmt

and analyse_assign scope_st stmt = ()

and analyse_read scope_st stmt = ()

and analyse_write scope_st stmt = ()

and analyse_call scope_st stmt = ()

and analyse_if_then scope_st stmt = ()

and analyse_if_then_else scope_st stmt = ()

and analyse_while scope_st stmt = ()

and get_expr_type scope_st = function
    | Ebool(_) -> Bool
    | Eint(_) -> Int
    | Efloat(_) -> Float
    | Eparen(expr) -> get_expr_type scope_st expr
    | Ebinop(lexpr,optr,rexpr) -> get_expr_type scope_st lexpr
    | Eunop(optr,expr) -> get_expr_type scope_st expr
    | Eelem(elem) -> get_elem_type scope_st elem

and get_elem_type scope_st (Elem(id,_)) =
    let (_,sym_type,_,_) = Hashtbl.find scope_st id
    in
    ast_type_from_sym_type sym_type

and check_unused_symbols prog = ()

and check_main = ()