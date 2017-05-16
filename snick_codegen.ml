(*
** File:          snick_codegen.ml
** Description:   Module to generate brill code from a parsed snick program.
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
open Snick_analyze
open Snick_br_ast
open Snick_optimizer
open Format
(* 
    | OP_MOVE
    | OP_ADD_OFFSET
*)

let brprog = ref [] (* Brill program to be generated *)
let out_of_bounds_label = 0
let div_by_zero_label = 1
let next_label = ref 2

(* Strip any parentheses around a expression *)
let rec strip_paren expr = match expr with
    | Eparen paren_expr -> strip_paren paren_expr
    | _ -> expr

(* Start compiling program *)
let rec compile prog =
    analyse prog; (* First analyse the program into symbol table *)
    (* Generate brill program *)
    gen_br_program (simplify_prog prog);
    (* Print brill program *)
    print_prog !brprog

and is_idxs_all_static idxs =
    List.for_all
    (fun idx ->
        match idx with
        | Eint(_) -> true
        | _ -> false
    )
    idxs

and calc_static_offset idxs bases bounds =
    match idxs with
    | [] -> failwith ("Impossible error")
    | idx::[] ->
    (
        match idx with
        | Eint(int_idx) ->
        (
            match (List.hd bounds) with
            | (lo_bound,_) ->
            (
                int_idx - lo_bound
            )
        )
        | _ -> failwith ("Impossible error")
    )
    | idx::idxs_tail ->
    (
        match idx with
        | Eint(int_idx) ->
        (
            match (List.hd bounds) with
            | (lo_bound,_) ->
            (
                (int_idx - lo_bound) * (List.hd bases) +
                    calc_static_offset
                        (idxs_tail) (List.tl bases) (List.tl bounds)
            )
        )
        | _ -> failwith ("Impossible error")
    )

(* Brill program generation *)
and gen_br_program prog =
    gen_call "main";
    gen_halt "whatever";
    gen_br_out_of_bounds "whatever";
    gen_br_div_by_zero "whatever";
    List.iter gen_br_proc prog    

(* Create a label for the index out of bound error *)
and gen_br_out_of_bounds = function
    | _ ->
    (
        gen_label out_of_bounds_label;
        gen_string_const 0 "\"[FATAL]: array element out of bounds!\\n\"";
        gen_call_builtin "print_string";
        gen_halt "whatever"
    )

(* Create a label for the division by zero error *)
and gen_br_div_by_zero = function
    | _ ->
    (
        gen_label div_by_zero_label;
        gen_string_const 0 "\"[FATAL]: division by zero!\\n\"";
        gen_call_builtin "print_string";
        gen_halt "whatever"
    )

(*  *)
and gen_br_proc ((proc_id,params),proc_body) =
    let scope = Hashtbl.find ht_scopes proc_id
    in
    (
        gen_proc_label proc_id;
        gen_br_prologue scope params proc_body.decls;
        gen_br_stmts scope proc_body.stmts;
        gen_br_epilogue scope            
    )

and gen_br_prologue scope params decls =
    gen_comment "prologue";
    gen_unop "push" (get_scope_nslot scope);
    gen_br_params scope 0 params;
    gen_br_decls scope decls;
    
and gen_br_params scope cnt = function
    | [] -> ()
    | x::xs ->
        (
            gen_br_param scope cnt x;
            gen_br_params scope (cnt+1) xs
        )

and gen_br_param scope cnt (_, _, param_id) =
    let sym = Hashtbl.find (get_scope_st scope) param_id
    in
    match sym with
    | (_,_,nslot,_) -> gen_binop "store" nslot cnt
    
and gen_br_decls scope decls =
    let cnt = ref 0
    and ints_flag = ref false
    and int_reg = ref 0
    and reals_flag = ref false
    and real_reg = ref 0
    and reg = ref 0
    in
    (
        List.iter
            (fun (_, Variable(id,_)) ->
                (
                    let (_,sym_type,_,_) =
                        Hashtbl.find (get_scope_st scope) id
                    in
                    (
                        if (not !reals_flag) && (sym_type = SYM_REAL) then
                        (
                            reals_flag := true;
                            real_reg := !cnt;
                            incr cnt
                        )
                        else if (not !ints_flag) then
                        (
                            ints_flag := true;
                            int_reg := !cnt;
                            incr cnt
                        )
                    )
                )
            )
            decls;

        if !ints_flag then
            gen_int_const !int_reg 0;
        if !reals_flag then
            gen_real_const !real_reg 0.0;

        List.iter
            (fun (_, Variable(id,_)) ->
                (
                    let (_,sym_type,nslot,optn_bounds) =
                            Hashtbl.find (get_scope_st scope) id
                    in
                    (
                        if sym_type = SYM_REAL then
                            reg := !real_reg
                        else
                            reg := !int_reg;

                        match optn_bounds with
                        | None -> gen_binop "store" nslot !reg
                        | Some bounds ->
                            gen_br_init_array scope nslot !reg bounds
                    )
                )
            )
            decls;
    )

and gen_br_init_array scope nslot nreg bounds =
    let num = ref 1
    in
    (
        List.iter
        (fun (lo_bound,up_bound) ->
            ( num := ((up_bound - lo_bound) +1)*(!num) )
        )
        bounds;

        for offset = 0 to (!num-1) do
            gen_binop "store" (nslot+offset) nreg
        done
    )

and gen_br_stmts scope stmts =
    List.iter (gen_br_stmt scope) stmts

and gen_br_stmt scope stmt = match stmt with
    | Assign(elem,expr) -> gen_br_assign scope elem expr 
    | Read(elem) -> gen_br_read scope elem 
    | Write(write_expr) -> gen_br_write scope write_expr 
    | Call(proc_id,args) -> gen_br_call scope proc_id args 
    | If_then(expr,stmts) -> gen_br_ifthen scope expr stmts 
    | If_then_else(expr,then_stmts,else_stmts) ->
        gen_br_ifthenelse scope expr then_stmts else_stmts 
    | While(expr,stmts) -> gen_br_while scope expr stmts

and gen_br_assign scope (Elem(id,optn_idxs)) expr =
    gen_comment "assignment";
    let (symkind,symtype,nslot,optn_bounds) = 
        Hashtbl.find (get_scope_st scope) id
    and expr_type = get_expr_type scope expr
    in
    (
        gen_br_expr scope 0 expr;

        if ((symtype = SYM_REAL) && (expr_type = SYM_INT)) then
            gen_binop "int_to_real" 0 0;

        match optn_idxs with
        | Some idxs ->
        (
            match (is_idxs_all_static idxs) with
            | true ->
            (
                let (symkind,symtype,nslot,optn_bounds) =
                        Hashtbl.find (get_scope_st scope) id
                in
                (
                    let static_offset =
                    ( 
                        match optn_bounds with
                        | Some bounds -> calc_static_offset idxs
                                            (get_offset_bases bounds) bounds
                        | None -> failwith "Impossible error."
                    )
                    in
                    gen_binop "store" (nslot+static_offset) 0
                )
            )
            | false ->
            (
                gen_br_expr_array_addr scope 1 id idxs;
                gen_binop "store_indirect" 1 0
            )
        )
        | None ->
        (
            if symkind = SYM_PARAM_REF then
            (
                gen_binop "load" 1 nslot;
                gen_binop "store_indirect" 1 0
            )
            else
                gen_binop "store" nslot 0
        )
    )

and gen_br_read scope (Elem(id,optn_idxs)) =
    gen_comment "read";
    let (symkind,symtype,nslot,optn_bounds) 
        = Hashtbl.find (get_scope_st scope) id
    in
    (
        (
            match symtype with
            | SYM_BOOL -> gen_call_builtin "read_bool"
            | SYM_INT -> gen_call_builtin "read_int"
            | SYM_REAL -> gen_call_builtin "read_real"
        );
        match optn_idxs with
        | Some idxs ->
        (
            match (is_idxs_all_static idxs) with
            | true ->
            (
                let (symkind,symtype,nslot,optn_bounds) =
                        Hashtbl.find (get_scope_st scope) id
                in
                (
                    let static_offset =
                    ( 
                        match optn_bounds with
                        | Some bounds -> calc_static_offset idxs
                                            (get_offset_bases bounds) bounds
                        | None -> failwith "Impossible error."
                    )
                    in
                    gen_binop "store" (nslot+static_offset) 0
                )
            )
            | false ->
            (
                gen_br_expr_array_addr scope 1 id idxs;
                gen_binop "store_indirect" 1 0
            )
        )
        | None ->
        (
             if symkind = SYM_PARAM_REF then
            (
                gen_binop "load" 1 nslot;
                gen_binop "store_indirect" 1 0
            )
            else
                gen_binop "store" nslot 0
        )
    )

and gen_br_write scope write_expr = 
    gen_comment "write";
    match write_expr with
    | Expr(expr) ->
    (
        gen_br_expr scope 0 expr;
        match (get_expr_type scope expr) with
        | SYM_BOOL -> gen_call_builtin "print_bool"
        | SYM_INT -> gen_call_builtin "print_int"
        | SYM_REAL -> gen_call_builtin "print_real"
    )
    | String(string_const) ->
    (
        gen_string_const 0 string_const;
        gen_call_builtin "print_string"
    )


and gen_br_call scope proc_id args =
    gen_comment "proc call";
    let params = get_scope_params (Hashtbl.find ht_scopes proc_id)
    and nreg = ref 0
    in
    (
        List.iter2
            (fun arg param ->
                (
                    (
                        match param with
                        | (Ref,_,_) ->
                        (
                            match (strip_paren arg) with
                            | Eelem(Elem(id,optn_idxs)) ->
                            (
                                let (symkind,symtype,nslot,optn_bounds) =
                                    Hashtbl.find (get_scope_st scope) id
                                in
                                (
                                    if symkind = SYM_PARAM_REF then
                                        gen_binop "load" !nreg nslot
                                    else
                                    (
                                        match optn_idxs with
                                        | Some idxs ->
                                            gen_br_expr_array_addr
                                                scope !nreg id idxs
                                        | None ->
                                            gen_binop
                                                "load_address" !nreg nslot
                                    )
                                )
                            )
                            | _ -> 
                                failwith ("Weird errpr in call_"^proc_id^
                                        " in proc_"^(get_scope_id scope)^
                                        ": can't pass non-elem to a ref. "^
                                        "Should have been reported.")
                        )
                        | (Val,param_type,_) ->
                        (
                            gen_br_expr scope !nreg arg;
                            if (((get_expr_type scope arg) = SYM_INT)
                            && (param_type = Float)) then
                                gen_binop "int_to_real" !nreg !nreg
                        )
                    );
                    incr nreg
                )
            )
            args
            params;
        gen_call proc_id
    )

and gen_br_expr_array_val scope nreg id idxs =    
    match (is_idxs_all_static idxs) with
    | true ->
    (
        let (symkind,symtype,nslot,optn_bounds) =
                Hashtbl.find (get_scope_st scope) id
        in
        (
            let static_offset =
            ( 
                match optn_bounds with
                | Some bounds -> calc_static_offset idxs
                                    (get_offset_bases bounds) bounds
                | None -> failwith "Impossible error."
            )
            in
            gen_binop "load" nreg (nslot+static_offset)
        )
    )
    | false ->
    (
        gen_br_expr_array_addr scope nreg id idxs;
        gen_binop "load_indirect" nreg nreg
    )

and gen_br_expr_array_addr scope nreg id idxs =
    let (symkind,symtype,nslot,optn_bounds) =
        Hashtbl.find (get_scope_st scope) id
    in 
    (
        (
            match optn_bounds with
            | Some bounds -> 
                gen_dynamic_offset scope nreg idxs 
                    (get_offset_bases bounds) bounds
            | _ ->
                failwith ("Impossible error. "^
                            id^" should be an array in proc: "^
                            (get_scope_id scope))
        );
        gen_binop "load_address" (nreg+1) nslot;
        gen_triop "sub_offset" nreg (nreg+1) nreg
    )


(*
    offset idxs bases = (idx - lo_bound) * base + (offset idxs.tl bases.tl)
    except that: offset idx base = idx - lo_bound
*)
and gen_dynamic_offset scope nreg idxs bases bounds =
    match idxs with
    | [] -> failwith ("Impossible error in proc: "^
                        (get_scope_id scope))
    | idx::[] ->
    (
        match (List.hd bounds) with
        | (lo_bound,up_bound) ->
        (
            gen_br_expr scope nreg idx;

            gen_int_const (nreg+1) lo_bound;
            gen_triop "cmp_lt_int" (nreg+1) nreg (nreg+1);
            gen_binop "branch_on_true" (nreg+1) out_of_bounds_label;
            gen_int_const (nreg+1) up_bound;
            gen_triop "cmp_gt_int" (nreg+1) nreg (nreg+1);
            gen_binop "branch_on_true" (nreg+1) out_of_bounds_label;

            gen_int_const (nreg+1) lo_bound;
            gen_triop "sub_int" nreg nreg (nreg+1)
        )
    )
    | idx::idxs_tail ->
    (
        match (List.hd bounds) with
        | (lo_bound,up_bound) ->
        (
            gen_dynamic_offset scope nreg idxs_tail 
                (List.tl bases) (List.tl bounds);

            gen_br_expr scope (nreg+1) idx;

            gen_int_const (nreg+2) lo_bound;
            gen_triop "cmp_lt_int" (nreg+2) (nreg+1) (nreg+2);
            gen_binop "branch_on_true" (nreg+2) out_of_bounds_label;
            gen_int_const (nreg+2) up_bound;
            gen_triop "cmp_gt_int" (nreg+2) (nreg+1) (nreg+2);
            gen_binop "branch_on_true" (nreg+2) out_of_bounds_label;

            gen_int_const (nreg+2) lo_bound;
            gen_triop "sub_int" (nreg+1) (nreg+1) (nreg+2);

            gen_int_const (nreg+2) (List.hd bases);
            gen_triop "mul_int" (nreg+1) (nreg+1) (nreg+2);

            gen_triop "add_int" nreg (nreg+1) nreg 
        )
    )


and get_offset_bases bounds =
    let offset_bases = ref [1]
    in
    (
        List.iter
        (fun (lo_bound,up_bound) ->
            (
                offset_bases := List.append
                                [((up_bound - lo_bound + 1)*
                                    (List.hd !offset_bases))]
                                !offset_bases
            )
        )
        (List.rev bounds);
        offset_bases := List.tl !offset_bases;
        !offset_bases
    )

and gen_br_ifthen scope expr stmts =
    gen_comment "if";
    let after_label = !next_label
    in
    (
        incr next_label;
        gen_br_expr scope 0 expr;
        gen_binop "branch_on_false" 0 after_label;
        gen_br_stmts scope stmts;
        gen_label after_label
    )

and gen_br_ifthenelse scope expr then_stmts else_stmts =
    gen_comment "if";
    let else_label = !next_label
    in
    (
        incr next_label;
        let after_label = !next_label
        in
        (
            incr next_label;
            gen_br_expr scope 0 expr;
            gen_binop "branch_on_false" 0 else_label;
            gen_br_stmts scope then_stmts;
            gen_unop "branch_uncond" after_label;
            gen_label else_label;
            gen_br_stmts scope else_stmts;
            gen_label after_label
        )
    )

and gen_br_while scope expr stmts =
    gen_comment "while";
    let begin_label = !next_label
    in
    (
        incr next_label;
        let after_label = !next_label
        in
        (
            incr next_label;
            gen_label begin_label;
            gen_br_expr scope 0 expr;
            gen_binop "branch_on_false" 0 after_label;
            gen_br_stmts scope stmts;
            gen_unop "branch_uncond" begin_label;
            gen_label after_label
        )
    )

and gen_br_expr scope nreg = function
    | Ebool(bool_const) ->
    (
        match bool_const with
         | true -> gen_int_const nreg 1
         | false -> gen_int_const nreg 0
    )
    | Eint(int_const) -> gen_int_const nreg int_const
    | Efloat(float_const) -> gen_real_const nreg float_const
    | Eparen(expr) -> gen_br_expr scope nreg expr
    | Ebinop(lexpr,optr,rexpr) ->
        gen_br_expr_binop scope nreg lexpr optr rexpr
    | Eunop(optr,expr) -> gen_br_expr_unop scope nreg optr expr
    | Eelem(elem) ->
    (
        match elem with
        | Elem(id,None) -> gen_br_expr_id scope nreg id
        | Elem(id,Some idxs) -> gen_br_expr_array_val scope nreg id idxs
    )

and gen_br_expr_binop scope nreg lexpr optr rexpr =
    let lexpr_type = get_expr_type scope lexpr
    and rexpr_type = get_expr_type scope rexpr
    and lexpr_reg_usage = get_reg_usage scope lexpr
    and rexpr_reg_usage = get_reg_usage scope rexpr
    and lexpr_nreg = ref 0
    and rexpr_nreg = ref 0
    in
    (
        if lexpr_reg_usage >= rexpr_reg_usage then
        (
            lexpr_nreg := nreg;
            rexpr_nreg := (nreg+1);
            gen_br_expr scope !lexpr_nreg lexpr;
            gen_br_expr scope !rexpr_nreg rexpr
        )
        else
        (
            lexpr_nreg := (nreg+1);
            rexpr_nreg := nreg;
            gen_br_expr scope !rexpr_nreg rexpr;
            gen_br_expr scope !lexpr_nreg lexpr
        );

        (*check div_by_0*)
        if optr = Op_div then
        (
            if rexpr_type = SYM_REAL then
            (
                gen_real_const (nreg+2) 0.0;
                gen_triop "cmp_eq_real" (nreg+2) (nreg+2) !rexpr_nreg
            )
            else
            (
                gen_int_const (nreg+2) 0;
                gen_triop "cmp_eq_int" (nreg+2) (nreg+2) !rexpr_nreg
            );
            gen_binop "branch_on_true" (nreg+2) div_by_zero_label
        );

        if ((lexpr_type = SYM_INT) && (rexpr_type = SYM_REAL)) then
            gen_binop "int_to_real" !lexpr_nreg !lexpr_nreg
        else if ((lexpr_type = SYM_REAL) && (rexpr_type = SYM_INT)) then
            gen_binop "int_to_real" !rexpr_nreg !rexpr_nreg;

        if ((lexpr_type = SYM_BOOL) && (rexpr_type = SYM_BOOL)) then
            gen_br_expr_binop_bool
                scope nreg !lexpr_nreg !rexpr_nreg optr
        else if ((lexpr_type = SYM_REAL) || (rexpr_type = SYM_REAL)) then
            gen_br_expr_binop_by_type
                "real" nreg !lexpr_nreg !rexpr_nreg optr
        else
            gen_br_expr_binop_by_type
                "int" nreg !lexpr_nreg !rexpr_nreg optr
    )

and get_reg_usage scope = function
    | Ebool(_) -> 0
    | Eint(_) -> 0
    | Efloat(_) -> 0
    | Eparen(expr) -> get_reg_usage scope expr
    | Ebinop(lexpr,optr,rexpr) ->
    (
        let lexpr_reg_usage = get_reg_usage scope lexpr
        and rexpr_reg_usage = get_reg_usage scope rexpr
        in
        let min_count = min lexpr_reg_usage rexpr_reg_usage
        and max_count = max lexpr_reg_usage rexpr_reg_usage
        in
        let reg_usage_total = max max_count (min_count+1)
        in
        (
            if optr = Op_div then
                (max reg_usage_total 2)
            else
                reg_usage_total
        )
    )
    | Eunop(optr,expr) ->
    (
        let expr_reg_usage = get_reg_usage scope expr
        in
        (
            if optr = Op_minus then
                (max expr_reg_usage 1)
            else
                expr_reg_usage
        )
    ) 
    | Eelem(elem) ->
    (
        match elem with
        | Elem(id,None) -> 0
        | Elem(id,Some idxs) ->
        (
            if (is_idxs_all_static idxs) then
                0
            else
                let reg_usage_total = ref 0
                in
                (
                    if (List.length idxs) = 1 then
                    (
                        let idx = List.hd idxs
                        in
                        reg_usage_total :=
                            max ((get_reg_usage scope idx)+1) 2
                    )
                    else
                    (
                        List.iter
                        (fun idx ->
                            (
                                let reg_usage_1 =
                                    max ((get_reg_usage scope idx)+2) 3
                                in
                                reg_usage_total :=
                                    max reg_usage_1 !reg_usage_total
                            )
                        )
                        idxs
                    );
                    !reg_usage_total
                )
        )
    )

and gen_br_expr_binop_bool scope nreg lexpr_nreg rexpr_nreg = function
    | Op_or -> gen_triop "or" nreg lexpr_nreg rexpr_nreg
    | Op_and -> gen_triop "and" nreg lexpr_nreg rexpr_nreg
    | Op_eq -> gen_triop "cmp_eq_int" nreg lexpr_nreg rexpr_nreg
    | Op_ne -> gen_triop "cmp_ne_int" nreg lexpr_nreg rexpr_nreg
    | _ -> failwith "Invalid operator for bool type!"

and gen_br_expr_binop_by_type type_str nreg lexpr_nreg rexpr_nreg = function
    | Op_add -> gen_triop ("add_"^type_str) nreg lexpr_nreg rexpr_nreg
    | Op_sub -> gen_triop ("sub_"^type_str) nreg lexpr_nreg rexpr_nreg
    | Op_mul -> gen_triop ("mul_"^type_str) nreg lexpr_nreg rexpr_nreg
    | Op_div -> gen_triop ("div_"^type_str) nreg lexpr_nreg rexpr_nreg
    | Op_eq -> gen_triop ("cmp_eq_"^type_str) nreg lexpr_nreg rexpr_nreg
    | Op_ne -> gen_triop ("cmp_ne_"^type_str) nreg lexpr_nreg rexpr_nreg
    | Op_lt -> gen_triop ("cmp_lt_"^type_str) nreg lexpr_nreg rexpr_nreg
    | Op_le -> gen_triop ("cmp_le_"^type_str) nreg lexpr_nreg rexpr_nreg
    | Op_gt -> gen_triop ("cmp_gt_"^type_str) nreg lexpr_nreg rexpr_nreg
    | Op_ge -> gen_triop ("cmp_ge_"^type_str) nreg lexpr_nreg rexpr_nreg
    | _ -> failwith "Invalid operator for numeric types!"

and gen_br_expr_unop scope nreg optr expr =
    gen_br_expr scope nreg expr;
    let expr_type = get_expr_type scope expr
    in
    (
        if ((expr_type = SYM_BOOL) && (optr = Op_not)) then
            gen_binop "not" nreg nreg
        else if ((expr_type = SYM_INT) && (optr = Op_minus)) then
        (
            gen_int_const (nreg+1) 0;
            gen_triop "sub_int" nreg (nreg+1) nreg
        )
        else if ((expr_type = SYM_REAL) && (optr = Op_minus)) then
        (
            gen_real_const (nreg+1) 0.0;
            gen_triop "sub_real" nreg (nreg+1) nreg
        )
        else
            failwith "invalid optr for unop expr!"
    )

and gen_br_expr_id scope nreg id =
    let (symkind,_,nslot,_) = Hashtbl.find (get_scope_st scope) id
    in
    (
        match symkind with
        | SYM_PARAM_REF ->
        (
            gen_binop "load" nreg nslot;
            gen_binop "load_indirect" nreg nreg
        )
        | _ -> gen_binop "load" nreg nslot
    )

and gen_br_epilogue scope =
    gen_comment "epilogue";
    gen_unop "pop" (get_scope_nslot scope);
    gen_return "whatever"

and gen_comment comment =
    brprog := List.append !brprog [BrComment(comment)]

and gen_call proc_id =
    brprog := List.append !brprog [BrOp(OpCall(proc_id))]

and gen_halt = function
    | _ -> brprog := List.append !brprog [BrOp(OpHalt)]

and gen_proc_label proc_id =
    brprog := List.append !brprog [BrProc(proc_id)]

and gen_label nlabel = 
    brprog := List.append !brprog [BrLabel(nlabel)]

and gen_int_const nreg int_const =
    brprog := List.append !brprog [BrOp(OpIntConst(nreg,int_const))]

and gen_real_const nreg real_const =
    brprog := List.append !brprog [BrOp(OpRealConst(nreg,real_const))]

and gen_string_const nreg string_const =
    brprog := List.append !brprog [BrOp(OpStringConst(nreg,string_const))]

and gen_return = function
    | _ -> brprog := List.append !brprog [BrOp(OpReturn)]

and gen_debug_stack = function
    | _ -> brprog := List.append !brprog [BrOp(OpDebugStack)]

and gen_unop op x =
    let line = match op with
                | "push" -> BrOp(OpPush(x))
                | "pop" -> BrOp(OpPop(x))
                | "branch_uncond" -> BrOp(OpBranchUncond(x))
                | "debug_reg" -> BrOp(OpDebugReg(x))
                | "debug_slot" -> BrOp(OpDebugSlot(x))
                | _ -> failwith ("operation "^op^" not yet supported")
    in
    brprog := List.append !brprog [line]

and gen_binop op x1 x2 =
    let line = match op with
                | "load" -> BrOp(OpLoad(x1,x2))
                | "store" -> BrOp(OpStore(x1,x2))
                | "load_address" -> BrOp(OpLoadAddress(x1,x2))
                | "load_indirect" -> BrOp(OpLoadIndirect(x1,x2))
                | "store_indirect" -> BrOp(OpStoreIndirect(x1,x2))
                | "branch_on_true" -> BrOp(OpBranchOnTrue(x1,x2))
                | "branch_on_false" -> BrOp(OpBranchOnFalse(x1,x2))
                | "int_to_real" -> BrOp(OpIntToReal(x1,x2))
                | "not" -> BrOp(OpNot(x1,x2))
                | _ -> failwith ("operation "^op^" not yet supported")
    in
    brprog := List.append !brprog [line]

and gen_triop op x1 x2 x3 =
    let line = match op with
                | "or" -> BrOp(OpOr(x1,x2,x3))
                | "and" -> BrOp(OpAnd(x1,x2,x3))
                | "add_int" -> BrOp(OpAddInt(x1,x2,x3))
                | "sub_int" -> BrOp(OpSubInt(x1,x2,x3))
                | "mul_int" -> BrOp(OpMulInt(x1,x2,x3))
                | "div_int" -> BrOp(OpDivInt(x1,x2,x3))
                | "cmp_eq_int" -> BrOp(OpCmpEqInt(x1,x2,x3))
                | "cmp_ne_int" -> BrOp(OpCmpNeInt(x1,x2,x3))
                | "cmp_lt_int" -> BrOp(OpCmpLtInt(x1,x2,x3))
                | "cmp_le_int" -> BrOp(OpCmpLeInt(x1,x2,x3))
                | "cmp_gt_int" -> BrOp(OpCmpGtInt(x1,x2,x3))
                | "cmp_ge_int" -> BrOp(OpCmpGeInt(x1,x2,x3))
                | "add_real" -> BrOp(OpAddReal(x1,x2,x3))
                | "sub_real" -> BrOp(OpSubReal(x1,x2,x3))
                | "mul_real" -> BrOp(OpMulReal(x1,x2,x3))
                | "div_real" -> BrOp(OpDivReal(x1,x2,x3))
                | "cmp_eq_real" -> BrOp(OpCmpEqReal(x1,x2,x3))
                | "cmp_ne_real" -> BrOp(OpCmpNeReal(x1,x2,x3))
                | "cmp_lt_real" -> BrOp(OpCmpLtReal(x1,x2,x3))
                | "cmp_le_real" -> BrOp(OpCmpLeReal(x1,x2,x3))
                | "cmp_gt_real" -> BrOp(OpCmpGtReal(x1,x2,x3))
                | "cmp_ge_real" -> BrOp(OpCmpGeReal(x1,x2,x3))
                | "sub_offset" -> BrOp(OpSubOffset(x1,x2,x3))
                | _ -> failwith ("operation "^op^" not yet supported")
    in
    brprog := List.append !brprog [line]

and gen_call_builtin bltin_func =
    let line = match bltin_func with
                | "read_int" -> BrBltIn(BltInReadInt)
                | "read_real" -> BrBltIn(BltInReadReal)
                | "read_bool" -> BrBltIn(BltInReadBool)
                | "print_int" -> BrBltIn(BltInPrintInt)
                | "print_real" -> BrBltIn(BltInPrintReal)
                | "print_bool" -> BrBltIn(BltInPrintBool)
                | "print_string" -> BrBltIn(BltInPrintString)
                | _ -> failwith
                        ("bltin_func "^bltin_func^" not yet supported")
    in
    brprog := List.append !brprog [line]
