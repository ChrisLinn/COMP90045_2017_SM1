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
    analyse_statements scope prog_body.stmts

and analyse_statements scope stmts =
    List.iter (analyse_statement scope) stmts

and analyse_statement scope = function
    | Assign(elem,expr) -> analyse_assign scope elem expr
    | Read(elem) -> analyse_read scope elem
    | Write(write_expr) -> analyse_write scope write_expr
    | Call(id,exprs) -> analyse_call scope id exprs
    | If_then(expr,stmts) -> analyse_if_then scope expr stmts
    | If_then_else(expr,then_stmts,else_stmts) ->
        analyse_if_then_else scope expr then_stmts else_stmts
    | While(expr,stmts) -> analyse_while scope expr stmts

and analyse_assign scope elem expr = 
    let l_type = get_elem_type scope elem
    and r_type = get_expr_type scope expr
    in
        if ((l_type = r_type)
        || ((l_type = SYM_REAL)&&(r_type = SYM_INT))) then
            ()
        else
            raise (Failure ("Error in proc_"^(get_scope_id scope)^
                        ": assign type unmatch!"))

and analyse_read scope elem = ()   (*todo*)

and analyse_write scope = function
    | Expr(expr) -> analyse_expr scope expr
    | String(string_const) -> ()

and analyse_call scope id exprs = () (*todo*)

and analyse_if_then scope expr stmts =
    analyse_expr scope expr;
    analyse_statements scope stmts

and analyse_if_then_else scope expr then_stmts else_stmts =
    analyse_expr scope expr;
    analyse_statements scope then_stmts;
    analyse_statements scope else_stmts

and analyse_while scope expr stmts =
    analyse_expr scope expr;
    analyse_statements scope stmts

and analyse_expr scope = function (*todo*)
    | Eelem(elem) -> ()
    | Ebool(_) -> ()
    | Eint(_) -> ()
    | Efloat(_) -> ()
    | Eparen(expr) -> analyse_expr scope expr
    | Ebinop(lexpr,optr,rexpr) ->
    (
        match optr with
        | Op_or| Op_and | Op_eq | Op_ne | Op_lt | Op_gt | Op_le | Op_ge ->
            ()
        | Op_add | Op_sub | Op_mul | Op_div ->
        (
            
        )
        | _ -> raise (Failure ("Error in proc_"^(get_scope_id scope)^
                        ": invalid optr in Ebinop."))
    )
    | Eunop(optr,expr) -> ()

and get_expr_type scope = function
    | Eelem(elem) -> get_elem_type scope elem
    | Ebool(_) -> SYM_BOOL
    | Eint(_) -> SYM_INT
    | Efloat(_) -> SYM_REAL
    | Eparen(expr) -> get_expr_type scope expr
    | Ebinop(lexpr,optr,rexpr) ->
    (
        match optr with
        | Op_or| Op_and | Op_eq | Op_ne | Op_lt | Op_gt | Op_le | Op_ge ->
            SYM_BOOL
        | Op_add | Op_sub | Op_mul | Op_div ->
        (
            if (((get_expr_type scope lexpr)=SYM_REAL)
            ||((get_expr_type scope rexpr)=SYM_REAL)) then
                SYM_REAL
            else
                SYM_INT
        )
        | _ -> raise (Failure ("Error in proc_"^(get_scope_id scope)^
                        ": invalid optr in Ebinop."))
    )
    | Eunop(optr,expr) -> get_expr_type scope expr

and get_elem_type scope (Elem(id,_)) =
    let (_,sym_type,_,_) = Hashtbl.find (get_scope_st scope) id
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