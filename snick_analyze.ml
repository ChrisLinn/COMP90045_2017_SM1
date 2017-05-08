open Snick_ast
open Snick_symbol

let isValid = ref true


let ht_inis = 20
let ht_scopes = Hashtbl.create ht_inis

let rec analyse prog =
    gen_sym_table prog;
    check_main prog;                  (*???????*)
    List.iter error_detect_proc prog;
    check_unused_symbols prog

and gen_sym_table prog =
    List.iter generate_scope prog

and generate_scope ((proc_id,params),proc_body) =
    create_scope proc_id params;
    generate_params_symbols (Hashtbl.find ht_scopes proc_id) params;
    generate_decls_symbols (Hashtbl.find ht_scopes proc_id) proc_body.decls;

and create_scope scope_id params =
    Hashtbl.add
        ht_scopes
        scope_id
        (Scope(scope_id, (Hashtbl.create ht_inis), params, 0))

and generate_params_symbols scope params =
    List.iter (generate_param_symbol scope) params

and generate_param_symbol
        (Scope(scopeid,ht_st,params,nslot)) (indc,paramtype,paramid) =
    let
        sym_kind = sym_kind_from_ast_indc indc
        and
        sym_type = sym_type_from_ast_type paramtype
    in
    Hashtbl.add ht_st paramid (sym_kind,sym_type,nslot,None); 
    Hashtbl.replace ht_scopes scopeid (Scope(scopeid,ht_st,params,nslot+1));

and generate_decls_symbols scope decls =
    List.iter (generate_decl_symbol scope) decls

and generate_decl_symbol
        (Scope(scopeid,ht_st,params,nslot))
        (decltype, Variable(declid,optn_intvls)) =
    let
        sym_type = sym_type_from_ast_type decltype
    in
    Hashtbl.add ht_st declid (SYM_LOCAL,sym_type,nslot,None); 
    Hashtbl.replace ht_scopes scopeid (Scope(scopeid,ht_st,params,nslot+1));
    (*array*)

and check_main prog = ()

and error_detect_proc ((proc_id,_),prog_body) =
    let scope = Hashtbl.find ht_scopes proc_id
    in
    error_detect_statements scope prog_body.stmts

and error_detect_statements scope stmts =
    List.iter (error_detect_statement scope) stmts

and error_detect_statement scope = function
    | Assign(elem,expr) -> error_detect_assign scope elem expr
    | Read(elem) -> error_detect_read scope elem
    | Write(write_expr) -> error_detect_write scope write_expr
    | Call(id,exprs) -> error_detect_call scope id exprs
    | If_then(expr,stmts) -> error_detect_if_then scope expr stmts
    | If_then_else(expr,then_stmts,else_stmts) ->
        error_detect_if_then_else scope expr then_stmts else_stmts
    | While(expr,stmts) -> error_detect_while scope expr stmts

and error_detect_assign scope elem expr = 
    let l_type = get_elem_type scope elem
    and r_type = get_expr_type scope expr
    in
        if ((l_type = r_type)
        || ((l_type = SYM_REAL)&&(r_type = SYM_INT))) then
            ()
        else
            raise (Failure ("Error in proc_"^(get_scope_id scope)^
                        ": assign type unmatch!"))

and error_detect_read scope elem = ()   (*todo*)

and error_detect_write scope = function
    | Expr(expr) -> error_detect_expr scope expr
    | String(string_const) -> ()

and error_detect_call scope id exprs = () (*todo*)

and error_detect_if_then scope expr stmts =
    error_detect_expr scope expr;
    error_detect_statements scope stmts

and error_detect_if_then_else scope expr then_stmts else_stmts =
    error_detect_expr scope expr;
    error_detect_statements scope then_stmts;
    error_detect_statements scope else_stmts

and error_detect_while scope expr stmts =
    error_detect_expr scope expr;
    error_detect_statements scope stmts

and error_detect_expr scope = function (*todo*)
    | Eelem(elem) -> ()
    | Ebool(_) -> ()
    | Eint(_) -> ()
    | Efloat(_) -> ()
    | Eparen(expr) -> error_detect_expr scope expr
    | Ebinop(lexpr,optr,rexpr) ->
    (
        match optr with
        | Op_or| Op_and | Op_eq | Op_ne | Op_lt | Op_gt | Op_le | Op_ge ->
        (

        )
        | Op_add | Op_sub | Op_mul | Op_div ->
        (
            
        )
        | _ -> raise (Failure ("Error in proc_"^(get_scope_id scope)^
                        ": invalid optr in Ebinop."))
    )
    | Eunop(optr,expr) -> ()

and check_unused_symbols prog = () (*todo*)

and reduce_prog prog =
    List.map reduce_proc prog

and reduce_proc ((proc_id,proc_params),proc_body) = 
    ((proc_id,proc_params),(reduce_proc_body proc_id proc_body))

and reduce_proc_body proc_id proc_body = 
    {decls=proc_body.decls;stmts=(reduce_stmts proc_id proc_body.stmts)}

and reduce_stmts proc_id stmts =
    List.map (reduce_stmt proc_id) stmts

and reduce_stmt proc_id = function
    | Assign(elem,expr) -> Assign(elem,(reduce_expr proc_id expr))
    | Read(elem) -> Read(elem)
    | Write(write_expr) -> Write(reduce_write_expr proc_id write_expr)
    | Call(id,exprs) -> Call(id,(List.map (reduce_expr proc_id) exprs))
    | If_then(expr,stmts) ->
        If_then(reduce_expr proc_id expr,reduce_stmts proc_id stmts)
    | If_then_else(expr,then_stmts,else_stmts) ->
                            If_then_else(reduce_expr proc_id expr,
                                reduce_stmts proc_id then_stmts,
                                reduce_stmts proc_id else_stmts)
    | While(expr,stmts) ->
        While(reduce_expr proc_id expr,reduce_stmts proc_id stmts)

and reduce_write_expr proc_id = function
    | Expr(expr) -> Expr(reduce_expr proc_id expr)
    | String(string_const) -> String(string_const)

and reduce_expr proc_id = function  (*todo*)
    | Eparen(expr) -> reduce_expr proc_id expr
    | Ebinop(lexpr,optr,rexpr) ->
    (
        match (reduce_expr proc_id lexpr) with
        | Ebool(lbool) ->
        (
            match (reduce_expr proc_id rexpr) with
            | Ebool(rbool) ->
            (
                match optr with
                | Op_or -> Ebool(lbool||rbool)
                | Op_and -> Ebool(lbool&&rbool)
                | Op_eq -> Ebool(lbool=rbool)
                | Op_ne -> Ebool(lbool<>rbool)
                | _ -> raise (Failure ("Weird error in proc_"^
                        proc_id^
                        ": invalid bioptr in Eunop. "^
                        "Should have been reported."))
            )
            | Eint(rint) -> raise (Failure ("Weird error in proc_"^
                        proc_id^
                        ": invalid bioptr in Eunop. "^
                        "Should have been reported."))
            | Efloat(rfloat) -> raise (Failure ("Weird error in proc_"^
                        proc_id^
                        ": invalid bioptr in Eunop. "^
                        "Should have been reported."))
            | _ -> rexpr
        )
        | Eint(lint) ->
        (
            match (reduce_expr proc_id rexpr) with
            | Ebool(rbool) -> raise (Failure ("Weird error in proc_"^
                        proc_id^
                        ": invalid bioptr in Eunop. "^
                        "Should have been reported."))
            | Eint(rint) ->
            (
                match optr with
                | Op_eq -> Ebool(lint=rint)
                | Op_ne -> Ebool(lint<>rint)
                | Op_lt -> Ebool(lint<rint)
                | Op_gt -> Ebool(lint>rint)
                | Op_le -> Ebool(lint<=rint)
                | Op_ge -> Ebool(lint>=rint)
                | Op_add -> Eint(lint+rint)
                | Op_sub -> Eint(lint-rint)
                | Op_mul -> Eint(lint*rint)
                | Op_div -> Eint(lint/rint)
                | _ -> raise (Failure ("Weird error in proc_"^
                        proc_id^
                        ": invalid bioptr in Eunop. "^
                        "Should have been reported."))
            )
            | Efloat(rfloat) ->
            (
                match optr with
                | Op_eq -> Ebool((float_of_int lint)=rfloat)
                | Op_ne -> Ebool((float_of_int lint)<>rfloat)
                | Op_lt -> Ebool((float_of_int lint)<rfloat)
                | Op_gt -> Ebool((float_of_int lint)>rfloat)
                | Op_le -> Ebool((float_of_int lint)<=rfloat)
                | Op_ge -> Ebool((float_of_int lint)>=rfloat)
                | Op_add -> Efloat((float_of_int lint)+.rfloat)
                | Op_sub -> Efloat((float_of_int lint)-.rfloat)
                | Op_mul -> Efloat((float_of_int lint)*.rfloat)
                | Op_div -> Efloat((float_of_int lint)/.rfloat)
                | _ -> raise (Failure ("Weird error in proc_"^
                        proc_id^
                        ": invalid bioptr in Eunop. "^
                        "Should have been reported."))
            )
            | _ -> rexpr
        )
        | Efloat(lfloat) ->
        (
            match (reduce_expr proc_id rexpr) with
            | Ebool(rbool) -> raise (Failure ("Weird error in proc_"^
                        proc_id^
                        ": invalid bioptr in Eunop. "^
                        "Should have been reported."))
            | Eint(rint) ->
            (
                match optr with
                | Op_eq -> Ebool(lfloat=(float_of_int rint))
                | Op_ne -> Ebool(lfloat<>(float_of_int rint))
                | Op_lt -> Ebool(lfloat<(float_of_int rint))
                | Op_gt -> Ebool(lfloat>(float_of_int rint))
                | Op_le -> Ebool(lfloat<=(float_of_int rint))
                | Op_ge -> Ebool(lfloat>=(float_of_int rint))
                | Op_add -> Efloat(lfloat+.(float_of_int rint))
                | Op_sub -> Efloat(lfloat-.(float_of_int rint))
                | Op_mul -> Efloat(lfloat*.(float_of_int rint))
                | Op_div -> Efloat(lfloat/.(float_of_int rint))
                | _ -> raise (Failure ("Weird error in proc_"^
                        proc_id^
                        ": invalid bioptr in Eunop. "^
                        "Should have been reported."))
            )
            | Efloat(rfloat) ->
            (
                match optr with
                | Op_eq -> Ebool(lfloat=rfloat)
                | Op_ne -> Ebool(lfloat<>rfloat)
                | Op_lt -> Ebool(lfloat<rfloat)
                | Op_gt -> Ebool(lfloat>rfloat)
                | Op_le -> Ebool(lfloat<=rfloat)
                | Op_ge -> Ebool(lfloat>=rfloat)
                | Op_add -> Efloat(lfloat+.rfloat)
                | Op_sub -> Efloat(lfloat-.rfloat)
                | Op_mul -> Efloat(lfloat*.rfloat)
                | Op_div -> Efloat(lfloat/.rfloat)
                | _ -> raise (Failure ("Weird error in proc_"^
                        proc_id^
                        ": invalid bioptr in Eunop. "^
                        "Should have been reported."))
            )
            | _ -> rexpr
        )
        | _ -> lexpr
    )
    | Eunop(optr,expr) ->
    (
        match (reduce_expr proc_id expr) with
        | Ebool(bool_const) -> Ebool(not bool_const)
        | Eint(int_const) -> Eint(-int_const)
        | Efloat(float_const) -> Efloat(-.float_const)
        | ori_expr -> ori_expr
    )
    | ori_expr -> ori_expr

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
        | _ -> raise (Failure ("Weird error in proc_"^(get_scope_id scope)^
                        ": invalid unoptr in Ebinop. "^
                        "Should have been reported."))
    )
    | Eunop(optr,expr) -> get_expr_type scope expr

and get_elem_type scope (Elem(id,_)) =
    let (_,sym_type,_,_) = Hashtbl.find (get_scope_st scope) id
    in
    sym_type