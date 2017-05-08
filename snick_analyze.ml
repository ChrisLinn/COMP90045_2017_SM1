open Snick_ast
open Snick_symbol

let isValid = ref true

(*i guess we dont need to use to return result of gen_sym_table, we could just update it in Snick_symbol.ml*)
let rec analyse prog =
    gen_sym_table prog;
    List.iter analyse_proc prog;
    check_unused_symbols prog;    (*???????*)
    check_main prog                  (*???????*)

and analyse_proc ((proc_id,_),prog_body) =
    let scope = Hashtbl.find ht_scopes proc_id
    in
    let scope_st = get_scope_st scope 
    in
    analyse_statements scope_st prog_body.stmts

and analyse_statements scope_st stmts =
    List.iter (analyse_statement scope_st) stmts

and analyse_statement scope_st stmt = match stmt with
    | Assign(elem,expr) -> analyse_assign scope_st elem expr
    | Read(_) -> analyse_read scope_st stmt
    | Write(_) -> analyse_write scope_st stmt
    | Call(_) -> analyse_call scope_st stmt
    | If_then(_) -> analyse_if_then scope_st stmt
    | If_then_else(_) -> analyse_if_then_else scope_st stmt
    | While(_) -> analyse_while scope_st stmt

and analyse_assign scope_st elem expr = 
    let l_type = get_elem_type scope_st elem
    and r_type = get_expr_type scope_st expr
    in
        if ((l_type = r_type)
        || ((l_type = SYM_REAL)&&(r_type = SYM_INT))) then
            ()
        else
            raise (Failure "assign type unmatch!")

and analyse_read scope_st stmt = ()

and analyse_write scope_st stmt = ()

and analyse_call scope_st stmt = ()

and analyse_if_then scope_st stmt = ()

and analyse_if_then_else scope_st stmt = ()

and analyse_while scope_st stmt = ()

and get_expr_type scope_st = function
    | Eelem(elem) -> get_elem_type scope_st elem
    | Ebool(_) -> SYM_BOOL
    | Eint(_) -> SYM_INT
    | Efloat(_) -> SYM_REAL
    | Eparen(expr) -> get_expr_type scope_st expr
    | Ebinop(lexpr,optr,rexpr) ->
    (
        match optr with
        | Op_or| Op_and | Op_eq | Op_ne | Op_lt | Op_gt | Op_le | Op_ge ->
            SYM_BOOL
        | Op_add | Op_sub | Op_mul | Op_div ->
        (
            if (((get_expr_type scope_st lexpr)=SYM_REAL)
            ||((get_expr_type scope_st rexpr)=SYM_REAL)) then
                SYM_REAL
            else
                SYM_INT
        )
        | _ -> raise (Failure "invalid optr in Ebinop")
    )
    | Eunop(optr,expr) -> get_expr_type scope_st expr

and get_elem_type scope_st (Elem(id,_)) =
    let (_,sym_type,_,_) = Hashtbl.find scope_st id
    in
    sym_type

and check_unused_symbols prog = ()

and check_main prog = ()

and try_get_expr_value = function
    | Eelem(elem) -> None
    | Ebool(bool_const) -> Some (Ebool(bool_const))
    | Eint(int_const) -> Some (Eint(int_const))
    | Efloat(float_const) -> Some (Efloat(float_const))
    | Eparen(expr) -> try_get_expr_value expr
    | Ebinop(lexpr,optr,rexpr) ->
    (
        let lexpr_value = try_get_expr_value lexpr
        and rexpr_value = try_get_expr_value rexpr
        in
        (
            None
        )
    )
    | Eunop(optr,expr) ->
    (
        match (try_get_expr_value expr) with
        | Some (Ebool(bool_const)) -> Some (Ebool( not bool_const))
        | Some (Eint(int_const)) -> Some (Eint(-int_const))
        | Some (Efloat(float_const)) -> Some (Efloat(-.float_const))
        | _ -> None
    )