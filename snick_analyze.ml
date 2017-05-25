(*
** File:          snick_analyze.ml
** Description:   Semantic analyzer for a snick program,
**                also checks for any semantic errors in program.
** Last Modified: Tue. 16th May 2017 
** 
** Group name: Mainframe
** 
** Member names   | usernames
** Xianzhuo REN   | xianzhuor 
** Haoyu LIN      | haoyul3
** Zequn MA       | zequnm
*)

open Snick_ast
open Snick_symbol
open Snick_optimizer
open Snick_err
open Format

let ht_inis = 20
(* Symbol tables of the program to be analyzed *)
let ht_scopes = Hashtbl.create ht_inis

(* Start semantic analyzing for the program *)
let rec analyse prog =
    (* Get symbol tables of the program *)
    gen_sym_table prog;
    (* Verify we have a main procedure *)
    check_main prog;
    (* Detect for any other semantic errors in program *)
    List.iter error_detect_proc prog

(* Generate the symbol tables of program *)
and gen_sym_table prog =
    (* Symbol tables with different scopes are maintained for
    ** each procedure in program *)
    List.iter generate_scope prog

(* Generate the symbol table of a precedure *)
and generate_scope ((proc_id,params),proc_body) =
    create_scope proc_id params; (* Create new scope table *)
    (* Insert parameter symbols to the table *)
    generate_params_symbols (Hashtbl.find ht_scopes proc_id) params;
    (* Insert symbols in procedure declarations to the table *)
    generate_decls_symbols (Hashtbl.find ht_scopes proc_id) proc_body.decls;

(* Insert a empty table for a new scope as an entry to ht_scopes *)
and create_scope scope_id params =
    Hashtbl.add
        ht_scopes
        scope_id
        (Scope(scope_id, (Hashtbl.create ht_inis), params, 0))

(* Create symbols for procedure arguements *)
and generate_params_symbols scope params =
    let scope_id = get_scope_id scope
    in List.iter (generate_param_symbol scope_id) params

(* Create a symbol for a single procedure arguement *)
and generate_param_symbol scope_id (indc,paramtype,paramid) =
    (* scope of proc *)
    let (Scope(scopeid,ht_st,params,nslot)) = Hashtbl.find ht_scopes scope_id
    (* attribute: arguement indicator *)
    and sym_kind = sym_kind_from_ast_indc indc
    (* attribute: arguement type *)
    and sym_type = sym_type_from_ast_type paramtype
    in
    (
        (* Insert symbol with attribues to symbol table of current scope *)
        Hashtbl.add ht_st paramid (sym_kind,sym_type,nslot,None);
        Hashtbl.replace ht_scopes scopeid 
            (Scope(scopeid,ht_st,params,nslot+1))
    )

(* Create symbols for declarations in a procedure (scope) *)
and generate_decls_symbols scope decls =
    let scope_id = get_scope_id scope
    in 
    List.iter (generate_decl_symbol scope_id) decls

(* Create symbol for a single declaration of a procedure *)
and generate_decl_symbol scope_id (decltype, Variable(declid,optn_intvls)) =
    (* scopt of proc *)
    let (Scope(scopeid,ht_st,params,nslot)) = Hashtbl.find ht_scopes scope_id
    (* attribute: type of the declaration *)
    and sym_type = sym_type_from_ast_type decltype
    in
    (
        (* Check if this is a declaration of an array *)
        match optn_intvls with
        | None -> (* Not an array *)
        (
            Hashtbl.add ht_st declid (SYM_LOCAL,sym_type,nslot,None);
            Hashtbl.replace ht_scopes scopeid 
                (Scope(scopeid,ht_st,params,nslot+1))
        )
        | Some intvls -> (* Is an array *)
        (
            Hashtbl.add ht_st declid (SYM_LOCAL,sym_type,nslot,optn_intvls);
            
            let num = ref 1
            in
            (
                List.iter
                (* get number of slot required for the declared array size *)
                (fun (lo_bound,up_bound) ->
                    ( num := ((up_bound - lo_bound) +1)*(!num) )
                )
                intvls;
                Hashtbl.replace ht_scopes scopeid 
                    (Scope(scopeid,ht_st,params,nslot+(!num)))
            )
        )
    )

(* Check there is a main procedure in program *)
and check_main prog =
    let is_there_main =
            List.exists
            (fun ((proc_id,_),_) -> 
                ( proc_id = "main" )
            )
            prog
    in
    if is_there_main then ()
    (* raise error if main procedure not found *)
    else error_no_main ""

(* Error detection functions *)
(* Look for errors procedure by procedure *)
and error_detect_proc ((proc_id,_),prog_body) =
    let cnt = List.length (Hashtbl.find_all ht_scopes proc_id)
    in
    if (cnt > 1) then
        error_dup_proc proc_id
    else
        (* symbol table of curent scope (procedure) *)
        let scope = Hashtbl.find ht_scopes proc_id
        in
        (
            (* error detection in declarations *)
            error_detect_decls scope prog_body.decls;
            (* error detection in statements*)
            error_detect_stmts scope prog_body.stmts
        )

(* Look for errors declaration by declaration *)
and error_detect_decls scope decls =
    List.iter (error_detect_decl scope) decls

(* Look for possible errors in a single declaration:
**      illegal declarations of array *)
and error_detect_decl scope (_,(Variable(id,optn_intvls))) =
    let cnt = List.length (Hashtbl.find_all (get_scope_st scope) id)
    in
    if (cnt > 1) then
        error_dup_decl (get_scope_id scope) id
    else
        match optn_intvls with
        | None -> () (* element declaration *)
        | Some intvls -> (* array declaration *)
        (
            List.iter
            (* Check for index out of bound error *)
            (fun (lo_bound,up_bound) ->
                (
                    if (lo_bound>up_bound) || (lo_bound<0) then
                        error_illegal_bound (get_scope_id scope) id
                )
            )
            intvls
        )

(* Look for errors statement by statement *)
and error_detect_stmts scope stmts =
    List.iter (error_detect_stmt scope) stmts

(* Look for possible errors in a single statement,
** depending on type of statment *)
and error_detect_stmt scope = function
    | Assign(elem,expr) -> error_detect_assign scope elem expr
    | Read(elem) -> error_detect_elem scope elem
    | Write(write_expr) -> error_detect_write scope write_expr
    | Call(id,exprs) -> error_detect_call scope id exprs
    | If_then(expr,stmts) ->
    (
        error_detect_expr scope expr;
        error_detect_stmts scope stmts
    )
    | If_then_else(expr,then_stmts,else_stmts) ->
    (
        error_detect_expr scope expr;
        error_detect_stmts scope then_stmts;
        error_detect_stmts scope else_stmts
    )
    | While(expr,stmts) ->
    (
        error_detect_expr scope expr;
        error_detect_stmts scope stmts
    )

(* look for possible errors in an assignment:
**      assignment type mismatch *)
and error_detect_assign scope elem expr =
    (* First check for error on either side of assignment *)
    error_detect_elem scope elem;
    error_detect_expr scope expr;
    let l_type = get_elem_type scope elem (* type of LHS *)
    and r_type = get_expr_type scope expr (* type of RHS *)
    in
        if ((l_type = r_type)
        || ((l_type = SYM_REAL)&&(r_type = SYM_INT))) then
            ()
        else
            error_assign_type_mismatch (get_scope_id scope)

(* Look for possible errors in an element:
**      undeclared variable
**      illegal indexing of array;
**      indexing out of bound *)
and error_detect_elem scope (Elem(id,optn_idxs)) =
    if (Hashtbl.mem (get_scope_st scope) id) then
    (
        match optn_idxs with
        | None -> () (* singleton element *)
        | Some idxs -> (* array indexing element *)
        (
            List.iter
            (* check for illegal indexing *)
            (fun idx ->
                (
                    match (get_expr_type scope idx) with
                    | SYM_INT -> ()
                    (* error illegal indexing *)
                    | _ -> error_illegal_index (get_scope_id scope) id
                )
            )
            idxs;
            (* get size of array from the symbol table *)
            let (_,_,_,optn_intvls) = Hashtbl.find (get_scope_st scope) id
            in
            match optn_intvls with
            | Some intvls ->
            (
                List.iter2
                (* check for indexing out of bound *)
                (fun idx (lo_bound,up_bound) ->
                    (
                        match idx with
                        | Eint(int_idx) ->
                        (
                            if ((int_idx<lo_bound)||(int_idx>up_bound)) then
                                error_idx_out_of_bound (get_scope_id scope) id
                        )
                        | _ -> ()
                    )
                )
                idxs
                intvls
            )
            | _ -> ()
        )
    )
    (* error variable undeclared *)
    else error_undecl_var (get_scope_id scope) id

(* Look for possible errors in a write statement,
** error may only exist in the expression being written *)
and error_detect_write scope = function
    | Expr(expr) -> error_detect_expr scope expr
    | String(string_const) -> ()

(* Look for possible errors in a procedure call,
**      procedure undefined
**      incorrect parameter types
**      incoorect number of parameters *)
and error_detect_call scope id exprs =
    if (Hashtbl.mem ht_scopes id) then
    (
        List.iter (error_detect_expr scope) exprs;
        (* parameters in procedures symbol table *)
        let params = get_scope_params (Hashtbl.find ht_scopes id)
        in
        try
        (
            let scan_result = 
                (* check if arguements in procedure call matches
                ** procedure declaration in types,
                ** throws exception if mismatching number of arguement *)
                List.for_all2
                (fun (param_indc,param_type,_) arg ->
                    (
                        let arg_type = ast_type_from_sym_type
                                            (get_expr_type scope arg)
                        in
                        (
                            (param_type = arg_type)
                            ||
                            (
                                (param_indc = Val)
                                &&
                                ((param_type=Float)&&(arg_type=Int))
                            )
                        )
                    )
                )
                params
                exprs
            in
            if scan_result then ()
            (* error parameters type mismatch *)
            else error_arg_type_mismatch (get_scope_id scope) id
        )
        with
        (* error incorrect number of parameters *)
        | Invalid_argument(_) -> 
            error_arg_count_mismatch (get_scope_id scope) id
    )
    (* error procedure undefined *)
    else error_undef_proc (get_scope_id scope) id

(* Look for possible errors in an expression,
** depending on the type of expression *)
and error_detect_expr scope = function
    | Eparen(expr) -> error_detect_expr scope expr
    | Eelem(elem) -> error_detect_elem scope elem  
    | Ebinop(lexpr,optr,rexpr) -> error_detect_binop scope lexpr optr rexpr
    | Eunop(optr,expr) -> error_detect_unop scope optr expr
    | _ -> ()

(* Look for possible errors in a binary operation:
**      illeal operation on type
**      type missmatch for operation *)
and error_detect_binop scope lexpr optr rexpr = 
    (* First check for error on either side of binary operation *)
    error_detect_expr scope lexpr;
    error_detect_expr scope rexpr;
    (* Get types of expression on either side of operation *)
    let lexpr_type = get_expr_type scope lexpr
    and rexpr_type = get_expr_type scope rexpr
    in
    match lexpr_type with
    | SYM_BOOL -> (* LHS *)
    (
        match rexpr_type with (* RHS *)
        | SYM_BOOL -> (* BOOL binop BOOL *)
        (
            match optr with
            | Op_eq | Op_ne
            | Op_or | Op_and -> () 
            | _ -> error_illegal_optr (get_scope_id scope) "bool"
        )
        | SYM_INT | SYM_REAL -> error_optr_type_mismatch (get_scope_id scope)
    )
    | SYM_INT -> (* LHS *)
    (
        match rexpr_type with (* RHS *)
        | SYM_INT -> (* INT binop INT *)
        (
            match optr with
            | Op_eq | Op_ne 
            | Op_lt | Op_gt | Op_le | Op_ge 
            | Op_add | Op_sub | Op_mul | Op_div -> ()
            | _ -> error_illegal_optr (get_scope_id scope) "int"
        )
        | SYM_REAL -> (* INT binop FLOAT *)
        (
            match optr with
            | Op_lt | Op_gt | Op_le | Op_ge 
            | Op_add | Op_sub | Op_mul | Op_div -> ()
            | _ -> error_optr_type_mismatch (get_scope_id scope)
        )
        | SYM_BOOL -> error_optr_type_mismatch (get_scope_id scope)
    )
    | SYM_REAL -> (* LHS *)
    (
        match rexpr_type with (* RHS *)
        | SYM_INT -> (* FLOAT binop INT *)
        (
            match optr with
            | Op_lt | Op_gt | Op_le | Op_ge 
            | Op_add | Op_sub | Op_mul | Op_div -> ()
            | _ -> error_optr_type_mismatch (get_scope_id scope)
        )
        | SYM_REAL -> (* FLOAT binop FLOAT *)
        (
            match optr with
            | Op_eq | Op_ne 
            | Op_lt | Op_gt | Op_le | Op_ge 
            | Op_add | Op_sub | Op_mul | Op_div -> ()
            | _ -> error_illegal_optr (get_scope_id scope) "float"
        )
        | SYM_BOOL -> error_optr_type_mismatch (get_scope_id scope)
    )

(* Look for possible errors in a unary operation:
**      illeal operation on type *)
and error_detect_unop scope optr expr =
    (* check for errors in expression *)
    error_detect_expr scope expr;
    match (get_expr_type scope expr) with
    | SYM_BOOL ->
    (
        match optr with
        | Op_not -> ()
        | _ -> error_illegal_optr (get_scope_id scope) "bool"
    )
    | _ ->
    (
        match optr with
        | Op_minus -> ()
        | _ -> error_illegal_optr (get_scope_id scope) "numeric"
    )

(* Get the type of an expression given its scope (procedure),
** also may raise invalid operater error *)
and get_expr_type scope = function
    | Eelem(elem) -> get_elem_type scope elem
    | Ebool(_) -> SYM_BOOL
    | Eint(_) -> SYM_INT
    | Efloat(_) -> SYM_REAL
    | Eparen(expr) -> get_expr_type scope expr
    | Ebinop(lexpr,optr,rexpr) ->
    (
        match optr with
        | Op_or | Op_and | Op_eq | Op_ne | Op_lt | Op_gt | Op_le | Op_ge ->
            SYM_BOOL
        | Op_add | Op_sub | Op_mul | Op_div ->
        (
            if (((get_expr_type scope lexpr)=SYM_REAL)
            ||((get_expr_type scope rexpr)=SYM_REAL)) then
                SYM_REAL
            else
                SYM_INT
        )
        | _ -> error_invalid_operation (get_scope_id scope)
    )
    | Eunop(optr,expr) -> get_expr_type scope expr

(* Get the type of an element *)
and get_elem_type scope (Elem(id,_)) =
    let (_,sym_type,_,_) = Hashtbl.find (get_scope_st scope) id
    in sym_type

(* Print all symbol tables for debugging purposes *)
and print_all_sts = function
    | _ -> Hashtbl.iter
            (fun scope_id _ -> 
                ( print_st scope_id )
            )
            ht_scopes

and print_st scope_id = 
    let scope_st = get_scope_st (Hashtbl.find ht_scopes scope_id)
    and scope_nslot = get_scope_nslot (Hashtbl.find ht_scopes scope_id)
    in
    (
        fprintf std_formatter "symbol_table of scope: %s with slots: %d\n"
                                scope_id scope_nslot;
        fprintf std_formatter "-------------------------\n";
        Hashtbl.iter
        (fun id (_,_,nslot,_) ->
            (
                fprintf std_formatter "symbol:%s nslot:%d\n" id nslot
            )
        )
        scope_st;
        fprintf std_formatter "-------------------------\n\n";
    )