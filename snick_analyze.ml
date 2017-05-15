(*
** File:          snick_analyze.ml
** Description:   Semantic analyzer for a snick program.
** Last Modified: Mon. 15th May 2017 
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
open Format


(* let isValid = ref true *)

let ht_inis = 20
let ht_scopes = Hashtbl.create ht_inis

(* Start semantic analyzing for the program *)
let rec analyse prog =
    (* Get symbol tables of the program *)
    gen_sym_table prog;
    (* Verify we have a main procedure *)
    check_main prog;
    (* Detect for any other semantic errors in program *)
    List.iter error_detect_proc prog

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
    let scope_id = get_scope_id scope
    in 
    List.iter (generate_param_symbol scope_id) params

and generate_param_symbol scope_id (indc,paramtype,paramid) =
    let (Scope(scopeid,ht_st,params,nslot)) = Hashtbl.find ht_scopes scope_id
    and sym_kind = sym_kind_from_ast_indc indc
    and sym_type = sym_type_from_ast_type paramtype
    in
    (
        Hashtbl.add ht_st paramid (sym_kind,sym_type,nslot,None);
        Hashtbl.replace ht_scopes scopeid 
            (Scope(scopeid,ht_st,params,nslot+1))
    )

and generate_decls_symbols scope decls =
    let scope_id = get_scope_id scope
    in 
    List.iter (generate_decl_symbol scope_id) decls

and generate_decl_symbol scope_id (decltype, Variable(declid,optn_intvls)) =
    let (Scope(scopeid,ht_st,params,nslot)) = Hashtbl.find ht_scopes scope_id
    and sym_type = sym_type_from_ast_type decltype
    in
    (
        match optn_intvls with
        | None ->
        (
            Hashtbl.add ht_st declid (SYM_LOCAL,sym_type,nslot,None);
            Hashtbl.replace ht_scopes scopeid 
                (Scope(scopeid,ht_st,params,nslot+1))
        )
        | Some intvls ->
        (
            Hashtbl.add ht_st declid (SYM_LOCAL,sym_type,nslot,optn_intvls);
            
            let num = ref 1
            in
            (
                List.iter
                (fun (lo_bound,up_bound) ->
                    (
                        num := ((up_bound - lo_bound) +1)*(!num)
                    )
                )
                intvls;
                Hashtbl.replace ht_scopes scopeid 
                    (Scope(scopeid,ht_st,params,nslot+(!num)))
            )
        )
    )

and check_main prog =
    let is_there_main =
            List.exists
            (fun ((proc_id,_),_) -> 
                (
                    proc_id = "main"
                )
            )
            prog
    in
    match is_there_main with
    | true -> ()
    | false -> failwith ("No \'main\' procedure definition!")

(* Error detection functions *)
and error_detect_proc ((proc_id,_),prog_body) =
    let cnt = List.length (Hashtbl.find_all ht_scopes proc_id)
    in
    if (cnt > 1) then
        failwith ("Proc "^ proc_id^
                    " defined more than once!")
    else
        let scope = Hashtbl.find ht_scopes proc_id
        in
        (
            error_detect_decls scope prog_body.decls;
            error_detect_stmts scope prog_body.stmts
        )

and error_detect_decls scope decls =
    List.iter (error_detect_decl scope) decls

and error_detect_decl scope (_,(Variable(id,optn_intvls))) =
    let cnt = List.length (Hashtbl.find_all (get_scope_st scope) id)
    in
    if (cnt > 1) then
        failwith ("Declare "^
                    id^" more than once in proc: "^
                    (get_scope_id scope))
    else
        match optn_intvls with
        | None -> ()
        | Some intvls ->
        (
            List.iter
            (fun (lo_bound,up_bound) ->
                (
                    if (lo_bound > up_bound) then
                        failwith ("lo_bound > up_bound for "^
                                    id^" in proc: "^
                                    (get_scope_id scope))
                    else if (lo_bound < 0) then
                        failwith ("negative bound for "^
                                    id^" in proc: "^
                                    (get_scope_id scope))
                )
            )
            intvls
        )

and error_detect_stmts scope stmts =
    List.iter (error_detect_stmt scope) stmts

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

and error_detect_assign scope elem expr =
    error_detect_elem scope elem;
    error_detect_expr scope expr;
    let l_type = get_elem_type scope elem
    and r_type = get_expr_type scope expr
    in
        if ((l_type = r_type)
        || ((l_type = SYM_REAL)&&(r_type = SYM_INT))) then
            ()
        else
            failwith ("Error in proc \'"^(get_scope_id scope)^
                        "\': unmatched types for assignment!")

and error_detect_elem scope (Elem(id,optn_idxs)) =
    if (Hashtbl.mem (get_scope_st scope) id) then
    (
        match optn_idxs with
        | None -> ()
        | Some idxs ->
        (
            List.iter
            (fun idx ->
                (
                    match (get_expr_type scope idx) with
                    | SYM_INT -> ()
                    | _ -> failwith ("Array \'"^id^
                            "\' non-int index in proc: "
                                ^(get_scope_id scope))
                )
            )
            idxs;
            let (_,_,_,optn_intvls) = Hashtbl.find (get_scope_st scope) id
            in
            match optn_intvls with
            | Some intvls ->
            (
                List.iter2
                (fun idx (lo_bound,up_bound) ->
                    (
                        match idx with
                        | Eint(int_idx) ->
                        (
                            if ((int_idx<lo_bound)||(int_idx>up_bound)) then
                                failwith ("Array "^id^
                                    "index out of bound in proc: "^
                                    (get_scope_id scope))
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
    else
        failwith ("Elem \'"^id^"\' doesn't exist in proc: "^
                    (get_scope_id scope))
(*todo*)  
(*out of bounds*)

and error_detect_write scope = function
    | Expr(expr) -> error_detect_expr scope expr
    | String(string_const) -> ()

and error_detect_call scope id exprs =
    if (Hashtbl.mem ht_scopes id) then
    (
        List.iter (error_detect_expr scope) exprs;
        let params = get_scope_params (Hashtbl.find ht_scopes id)
        in
        try
        (
            let scan_result = 
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
            match scan_result with
            | true -> ()
            | false -> failwith ("params args type unmatch"^
                                    " for calling proc \'"^id^"\' in proc: "^
                                    (get_scope_id scope))
        )
        with
        | Invalid_argument(_) -> failwith ("params args num unmatch"^
                                    " for calling proc \'"^id^"\' in proc: "^
                                    (get_scope_id scope))
    )
    else failwith ("Calling non-existing proc \'"^id^"\' in proc: "^
                        (get_scope_id scope))

and error_detect_expr scope = function
    | Eparen(expr) -> error_detect_expr scope expr
    | Eelem(elem) -> error_detect_elem scope elem  
    | Ebinop(lexpr,optr,rexpr) ->
    (
        let lexpr_type = get_expr_type scope lexpr
        and rexpr_type = get_expr_type scope rexpr
        in
        match lexpr_type with
        | SYM_BOOL ->
        (
            match rexpr_type with
            | SYM_BOOL ->
            (
                match optr with
                | Op_eq | Op_ne
                | Op_or | Op_and -> () 
                | _ -> failwith ("Error in proc \'"^
                                (get_scope_id scope)^
                                "\': type unmatch for Ebinop.")
            )
            | SYM_INT | SYM_REAL -> failwith ("Error in proc \'"^
                                            (get_scope_id scope)^
                                            "\': type unmatch for Ebinop.")
        )
        | SYM_INT ->
        (
            match rexpr_type with
            | SYM_INT ->
            (
                match optr with
                | Op_eq | Op_ne 
                | Op_lt | Op_gt | Op_le | Op_ge 
                | Op_add | Op_sub | Op_mul | Op_div -> ()
                | _ -> failwith ("Error in proc \'"^
                                (get_scope_id scope)^
                                "\': type unmatch for Ebinop.")
            )
            | SYM_REAL ->
            (
                match optr with(* 
                | Op_eq | Op_ne  *)
                | Op_lt | Op_gt | Op_le | Op_ge 
                | Op_add | Op_sub | Op_mul | Op_div -> ()
                | _ -> failwith ("Error in proc \'"^
                                (get_scope_id scope)^
                                "\': type unmatch for Ebinop.")
            )
            | SYM_BOOL -> failwith ("Error in proc \'"^
                                    (get_scope_id scope)^
                                    "\': type unmatch for Ebinop.")
        )
        | SYM_REAL ->
        (
            match rexpr_type with
            | SYM_INT ->
            (
                match optr with(* 
                | Op_eq | Op_ne  *)
                | Op_lt | Op_gt | Op_le | Op_ge 
                | Op_add | Op_sub | Op_mul | Op_div -> ()
                | _ -> failwith ("Error in proc \'"^
                                (get_scope_id scope)^
                                "\': type unmatch for Ebinop.")
            )
            | SYM_REAL ->
            (
                match optr with
                | Op_eq | Op_ne 
                | Op_lt | Op_gt | Op_le | Op_ge 
                | Op_add | Op_sub | Op_mul | Op_div -> ()
                | _ -> failwith ("Error in proc \'"^
                                (get_scope_id scope)^
                                "\': type unmatch for Ebinop.")
            )
            | SYM_BOOL -> failwith ("Error in proc \'"^
                                    (get_scope_id scope)^
                                    "\': type unmatch for Ebinop.")
        )
    )
    | Eunop(optr,expr) ->
    (
        match (get_expr_type scope expr) with
        | SYM_BOOL ->
        (
            match optr with
            | Op_not -> ()
            | _ -> failwith ("Error in proc \'"^
                            (get_scope_id scope)^
                            "\': type unmatch for Eunop.")
        )
        | _ ->
        (
            match optr with
            | Op_minus -> ()
            | _ -> failwith ("Error in proc \'"^
                            (get_scope_id scope)^
                            "\': type unmatch for Eunop.")
        )
    )
    | _ -> ()

(* Optimization functions *)
(* and simplify_prog prog =
    List.map simplify_proc prog

and simplify_proc ((proc_id,proc_params),proc_body) = 
    ((proc_id,proc_params),(simplify_proc_body proc_id proc_body))

and simplify_proc_body proc_id proc_body = 
    {decls=proc_body.decls;stmts=(simplify_stmts proc_id proc_body.stmts)}

and simplify_stmts proc_id stmts =
    List.map (simplify_stmt proc_id) stmts

and simplify_stmt proc_id = function
    | Assign(elem,expr) ->
        Assign((simplify_elem proc_id elem),(simplify_expr proc_id expr)) 
    | Read(elem) -> Read(simplify_elem proc_id elem)   
    | Write(write_expr) -> Write(simplify_write_expr proc_id write_expr)
    | Call(id,exprs) -> Call(id,(List.map (simplify_expr proc_id) exprs))
    | If_then(expr,stmts) -> If_then((simplify_expr proc_id expr),
                                (simplify_stmts proc_id stmts))
    | If_then_else(expr,then_stmts,else_stmts) ->
                            If_then_else((simplify_expr proc_id expr),
                                (simplify_stmts proc_id then_stmts),
                                (simplify_stmts proc_id else_stmts))
    | While(expr,stmts) -> While((simplify_expr proc_id expr),
                                (simplify_stmts proc_id stmts))

and simplify_elem proc_id = function
    | Elem(id,Some idxs) ->
        Elem(id,Some (List.map (simplify_expr proc_id) idxs))
    | elem -> elem

and simplify_write_expr proc_id = function
    | Expr(expr) -> Expr(simplify_expr proc_id expr)
    | String(string_const) -> String(string_const)

and simplify_expr proc_id = function
    | Eparen(expr) -> simplify_expr proc_id expr
    | Eelem(elem) -> Eelem(simplify_elem proc_id elem)
    | Ebinop(lexpr,optr,rexpr) ->
    (
        let simplified_lexpr = simplify_expr proc_id lexpr
        and simplified_rexpr = simplify_expr proc_id rexpr
        in
        match simplified_lexpr with
        | Ebool(lbool) ->
        (
            match simplified_rexpr with
            | Ebool(rbool) ->
            (
                match optr with
                | Op_or -> Ebool(lbool||rbool)
                | Op_and -> Ebool(lbool&&rbool)
                | Op_eq -> Ebool(lbool=rbool)
                | Op_ne -> Ebool(lbool<>rbool)
                | _ -> failwith ("Weird error in proc \'"^
                        proc_id^
                        "\': invalid bioptr in Ebinop. "^
                        "Should have been reported.")
            )
            | _ -> Ebinop(simplified_lexpr,optr,simplified_rexpr)
        )
        | Eint(lint) ->
        (
            match simplified_rexpr with
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
                | _ -> failwith ("Weird error in proc \'"^
                        proc_id^
                        "\': invalid optr in Ebinop. "^
                        "Should have been reported.")
            )
            | Efloat(rfloat) ->
            (
                match optr with(* 
                | Op_eq -> Ebool((float_of_int lint)=rfloat)
                | Op_ne -> Ebool((float_of_int lint)<>rfloat) *)
                | Op_lt -> Ebool((float_of_int lint)<rfloat)
                | Op_gt -> Ebool((float_of_int lint)>rfloat)
                | Op_le -> Ebool((float_of_int lint)<=rfloat)
                | Op_ge -> Ebool((float_of_int lint)>=rfloat)
                | Op_add -> Efloat((float_of_int lint)+.rfloat)
                | Op_sub -> Efloat((float_of_int lint)-.rfloat)
                | Op_mul -> Efloat((float_of_int lint)*.rfloat)
                | Op_div -> Efloat((float_of_int lint)/.rfloat)
                | _ -> failwith ("Weird error in proc \'"^
                        proc_id^
                        "\': invalid optr in Ebinop. "^
                        "Should have been reported.")
            )
            | _ -> Ebinop(simplified_lexpr,optr,simplified_rexpr)
        )
        | Efloat(lfloat) ->
        (
            match simplified_rexpr with
            | Eint(rint) ->
            (
                match optr with(* 
                | Op_eq -> Ebool(lfloat=(float_of_int rint))
                | Op_ne -> Ebool(lfloat<>(float_of_int rint)) *)
                | Op_lt -> Ebool(lfloat<(float_of_int rint))
                | Op_gt -> Ebool(lfloat>(float_of_int rint))
                | Op_le -> Ebool(lfloat<=(float_of_int rint))
                | Op_ge -> Ebool(lfloat>=(float_of_int rint))
                | Op_add -> Efloat(lfloat+.(float_of_int rint))
                | Op_sub -> Efloat(lfloat-.(float_of_int rint))
                | Op_mul -> Efloat(lfloat*.(float_of_int rint))
                | Op_div -> Efloat(lfloat/.(float_of_int rint))
                | _ -> failwith ("Weird error in proc \'"^
                        proc_id^
                        "\': invalid optr in Ebinop. "^
                        "Should have been reported.")
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
                | _ -> failwith ("Weird error in proc \'"^
                        proc_id^
                        "\': invalid optr in Ebinop. "^
                        "Should have been reported.")
            )
            | _ -> Ebinop(simplified_lexpr,optr,simplified_rexpr)
        )
        | _ -> Ebinop(simplified_lexpr,optr,simplified_rexpr)
    )
    | Eunop(optr,expr) ->
    (
        let simplified_expr = simplify_expr proc_id expr
        in
        match simplified_expr with
        | Ebool(bool_const) -> Ebool(not bool_const)
        | Eint(int_const) -> Eint(-int_const)
        | Efloat(float_const) -> Efloat(-.float_const)
        | _ -> Eunop(optr,simplified_expr)
    )
    | ori_expr -> ori_expr
*)
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
        | _ -> failwith ("Error in proc \'"^(get_scope_id scope)^
                        "\': invalid unoptr in Ebinop. ")
    )
    | Eunop(optr,expr) -> get_expr_type scope expr

and get_elem_type scope (Elem(id,_)) =
    let (_,sym_type,_,_) = Hashtbl.find (get_scope_st scope) id
    in
    sym_type

(* Print all symbol tables for debugging purposes *)
let rec print_all_sts = function
    | _ -> Hashtbl.iter
            (fun scope_id _ -> 
                (
                    print_st scope_id
                )
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