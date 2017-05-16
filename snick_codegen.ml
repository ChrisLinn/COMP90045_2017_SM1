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
open Snick_err
open Snick_analyze
open Snick_br_ast
open Snick_optimizer
open Format

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

(* Check if indices are static, i.e. all indices are int
** e.g. A[1,2] *)
and is_idxs_all_static idxs =
    List.for_all
    (fun idx ->
        match idx with
        | Eint(_) -> true
        | _ -> false
    )
    idxs

(* Calculate the offset for static indices *)
and calc_static_offset idxs bases bounds =
    let (lo_bound, _) = List.hd bounds in
    match idxs with
    | [] -> error_undefined ""
    | idx::[] ->
    (
        match idx with
        | Eint(int_idx) -> int_idx - lo_bound
        | _ -> error_undefined ""
    )
    | idx::idxs_tail ->
    (
        let offset = calc_static_offset 
                        (idxs_tail) (List.tl bases) (List.tl bounds) in
        match idx with
        | Eint(int_idx) -> (int_idx - lo_bound) * (List.hd bases) + offset
        | _ -> error_undefined ""
    )

(* Brill program generation *)
and gen_br_program prog =
    gen_call "main";
    gen_halt "";
    gen_br_out_of_bounds "";
    gen_br_div_by_zero "";
    List.iter gen_br_proc prog    

(* Create a label and instructions for the index out of bound error *)
and gen_br_out_of_bounds = function
    | _ ->
    (
        gen_label out_of_bounds_label;
        gen_string_const 0 "\"[FATAL]: array element out of bounds!\\n\"";
        gen_call_builtin "print_string";
        gen_halt ""
    )

(* Create a label and instructions for the division by zero error *)
and gen_br_div_by_zero = function
    | _ ->
    (
        gen_label div_by_zero_label;
        gen_string_const 0 "\"[FATAL]: division by zero!\\n\"";
        gen_call_builtin "print_string";
        gen_halt ""
    )

(* Generate a block of brill instructions for a snick procedure *)
and gen_br_proc ((proc_id,params),proc_body) =
    let scope = Hashtbl.find ht_scopes proc_id
    in
    (
        (* generate label *)
        gen_proc_label proc_id;
        (* generate prologue *)
        gen_br_prologue scope params proc_body.decls;
        (* generate instructions of statments in procedure *)
        gen_br_stmts scope proc_body.stmts;
        (* generate epilogue *)
        gen_br_epilogue scope            
    )

(* Generate prologue of procedure *)
and gen_br_prologue scope params decls =
    gen_comment "prologue"; (* mark with comment *)
    (* push this scope to stack *)
    gen_unop "push" (get_scope_nslot scope);
    (* store reference to procedure parameters *)
    gen_br_params scope 0 params;
    (* store declared variables in scope *)
    gen_br_decls scope decls;
    
(* Generate instruction to store brill procedure parameters *)
and gen_br_params scope cnt = function
    | [] -> ()
    | x::xs ->
        (
            gen_br_param scope cnt x;
            gen_br_params scope (cnt+1) xs
        )

(* Generate a intruction to save a parameter to register *)
and gen_br_param scope cnt (_, _, param_id) =
    let sym = Hashtbl.find (get_scope_st scope) param_id
    in match sym with
    | (_,_,nslot,_) -> gen_binop "store" nslot cnt
    
(* Generate instructions for declaration in scope *)
(* Declared bool value will be evaluate in runtime *)
and gen_br_decls scope decls =
    let cnt = ref 0             (* count of declarations in scope *)
    and ints_flag = ref false   (* if int or bool has been declared *)
    and int_reg = ref 0         (* register for int variable *)
    and reals_flag = ref false  (* if float has been declared *)
    and real_reg = ref 0        (* register for float variable *)
    and reg = ref 0             (* current assigned register *)
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
                        (   (* if current declaration is for float, and 
                            ** there hasn't been a float declaration before *)
                            reals_flag := true;
                            real_reg := !cnt;
                            incr cnt
                        )
                        else if (not !ints_flag) then
                        (   (* if current declaration is for int or bool,
                            ** and there hasn't been a int or float 
                            ** declaration before *)
                            ints_flag := true;
                            int_reg := !cnt;
                            incr cnt
                        )
                    )
                )
            )
            decls;

        (* initialize int to 0 and float false *)
        if !ints_flag then gen_int_const !int_reg 0;
        (* initialize float to 0.0 *)
        if !reals_flag then gen_real_const !real_reg 0.0;

        List.iter
            (fun (_, Variable(id,_)) ->
                (
                    let (_,sym_type,nslot,optn_bounds) =
                            Hashtbl.find (get_scope_st scope) id
                    in
                    (
                        (* refer to float register if symbol is float *)
                        if sym_type = SYM_REAL then reg := !real_reg
                        (* refer to int register if symbol is int or bool *)
                        else reg := !int_reg;

                        (* generate instruction to store declared variable *)
                        match optn_bounds with
                        | None -> gen_binop "store" nslot !reg
                        (* generate instructions for array declaration *)
                        | Some bounds -> 
                            gen_br_init_array scope nslot !reg bounds
                    )
                )
            )
            decls;
    )

(* Generate instruction for array initialization *)
and gen_br_init_array scope nslot nreg bounds =
    let num = ref 1 (* number of element in array *)
    in
    (
        (* Calculate total number of elements in array *)
        List.iter
            (fun (lo_bound,up_bound) ->
                ( num := ((up_bound - lo_bound) +1)*(!num) )
            )
            bounds;
        (* store elements of array *)
        for offset = 0 to (!num-1) do
            gen_binop "store" (nslot+offset) nreg
        done
    )

(* Generate instructions for procedure statements *)
and gen_br_stmts scope stmts =
    List.iter (gen_br_stmt scope) stmts

(* Generate instruction for a single statements,
** depending on statement type *)
and gen_br_stmt scope stmt = match stmt with
    | Assign(elem,expr) -> gen_br_assign scope elem expr 
    | Read(elem) -> gen_br_read scope elem 
    | Write(write_expr) -> gen_br_write scope write_expr 
    | Call(proc_id,args) -> gen_br_call scope proc_id args 
    | If_then(expr,stmts) -> gen_br_ifthen scope expr stmts 
    | If_then_else(expr,then_stmts,else_stmts) ->
        gen_br_ifthenelse scope expr then_stmts else_stmts 
    | While(expr,stmts) -> gen_br_while scope expr stmts

(* Generate instruction for assignment *)
and gen_br_assign scope (Elem(id,optn_idxs)) expr =
    gen_comment "assignment"; (* mark with comment *)
    let (symkind,symtype,nslot,optn_bounds) = 
        Hashtbl.find (get_scope_st scope) id
    and expr_type = get_expr_type scope expr
    in
    (
        gen_br_expr scope 0 expr;

        (* type casting from int to float *)
        if ((symtype = SYM_REAL) && (expr_type = SYM_INT)) then
            gen_binop "int_to_real" 0 0;

        match optn_idxs with
        | Some idxs -> (* if LHS of assign is an element of array *)
        (
            if (is_idxs_all_static idxs) then
                gen_op_static_idx scope 0 "store" id idxs
            else
            (
                gen_br_expr_array_addr scope 1 id idxs;
                gen_binop "store_indirect" 1 0
            )
        )
        | None ->
        (
            (* load register of LHS if LHS is a ref parameter
            ** of current scope, then rewites the register *)
            if symkind = SYM_PARAM_REF then
            (
                gen_binop "load" 1 nslot;
                gen_binop "store_indirect" 1 0
            )
            (* else store the new value *)
            else gen_binop "store" nslot 0
        )
    )

(* Generate instructions for snick read operation *)
and gen_br_read scope (Elem(id,optn_idxs)) =
    gen_comment "read"; (* mark with comment *)
    let (symkind,symtype,nslot,optn_bounds) 
        = Hashtbl.find (get_scope_st scope) id
    in
    (
        (   (* call builtin read depending on type of
            ** element being read *)
            match symtype with
            | SYM_BOOL -> gen_call_builtin "read_bool"
            | SYM_INT -> gen_call_builtin "read_int"
            | SYM_REAL -> gen_call_builtin "read_real"
        );
        match optn_idxs with
        | Some idxs -> (* if LHS of assign is an element of array *)
        (
            if (is_idxs_all_static idxs) then
                gen_op_static_idx scope 0 "store" id idxs
            else
            (
                gen_br_expr_array_addr scope 1 id idxs;
                gen_binop "store_indirect" 1 0
            )
        )
        | None ->
        (   (* load register of LHS if LHS is a ref parameter
            ** of current scope, then rewites the register *)
             if symkind = SYM_PARAM_REF then
            (
                gen_binop "load" 1 nslot;
                gen_binop "store_indirect" 1 0
            )
            (* else store the new value *)
            else gen_binop "store" nslot 0
        )
    )

(* Generate instructions for snick write operation *)
and gen_br_write scope write_expr = 
    gen_comment "write";
    match write_expr with
    | Expr(expr) ->
    (
        (* first determine what to be written *)
        gen_br_expr scope 0 expr; 
        (* call builtin write depending on type of
        ** element being read *)
        match (get_expr_type scope expr) with
        | SYM_BOOL -> gen_call_builtin "print_bool"
        | SYM_INT -> gen_call_builtin "print_int"
        | SYM_REAL -> gen_call_builtin "print_real"
    )
    | String(string_const) -> (* write string *)
    (
        gen_string_const 0 string_const;
        gen_call_builtin "print_string"
    )

(* Generate instructions for snick procedure call *)
and gen_br_call scope proc_id args =
    gen_comment "proc call"; (* mark with comment *)
    let params = get_scope_params (Hashtbl.find ht_scopes proc_id)
    and nreg = ref 0
    in
    (
        List.iter2
            (fun arg param ->
                (
                    (
                        match param with
                        | (Ref,_,_) -> (* for ref parameter *)
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
                            | _ -> error_undefined ""
                        )
                        | (Val,param_type,_) -> (* for val parameter *)
                        (
                            gen_br_expr scope !nreg arg;
                            (* type cast from int to real *)
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
        gen_call proc_id (* generate instruction for procedure call *)
    )

and gen_br_expr_array_val scope nreg id idxs =    
    if (is_idxs_all_static idxs) then
        gen_op_static_idx scope nreg "load" id idxs
    else
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
            | _ -> error_undefined ""
        );
        gen_binop "load_address" (nreg+1) nslot;
        gen_triop "sub_offset" nreg (nreg+1) nreg
    )

and gen_op_static_idx scope nreg op_str id idxs =
    let (symkind,symtype,nslot,optn_bounds) =
            Hashtbl.find (get_scope_st scope) id
    in
    (
        let static_offset =
        ( 
            match optn_bounds with
            | Some bounds -> calc_static_offset idxs
                                (get_offset_bases bounds) bounds
            | None -> error_undefined ""
        )
        in match op_str with
        | "store" -> gen_binop "store" (nslot+static_offset) 0
        | "load" -> gen_binop "load" nreg (nslot+static_offset)
        | _ -> ()
    )

and gen_dynamic_offset scope nreg idxs bases bounds =
    match idxs with
    | [] -> error_undefined ""
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

(* Get a list of starting slot of bounds of an array *)
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

(* Generate instruction for if-then statement *)
and gen_br_ifthen scope expr stmts =
    gen_comment "if"; (* mark with comment *)
    let after_label = !next_label
    in
    (
        incr next_label;
        gen_br_expr scope 0 expr; (* guard expression *)
        (* exist statment if false *)
        gen_binop "branch_on_false" 0 after_label;
        gen_br_stmts scope stmts;
        gen_label after_label
    )

(* Generate instruction for if-then-else statement *)
and gen_br_ifthenelse scope expr then_stmts else_stmts =
    gen_comment "if"; (* mark with comment *)
    let else_label = !next_label
    in
    (
        incr next_label;
        let after_label = !next_label
        in
        (
            incr next_label;
            gen_br_expr scope 0 expr; (* guard expression *)
            (* to else if false *)
            gen_binop "branch_on_false" 0 else_label;
            gen_br_stmts scope then_stmts;
            (* exit statement when finished then block *)
            gen_unop "branch_uncond" after_label;
            gen_label else_label;
            gen_br_stmts scope else_stmts;
            gen_label after_label
        )
    )

(* Generate instruction for while statement *)
and gen_br_while scope expr stmts =
    gen_comment "while"; (* mark with comment *)
    let begin_label = !next_label
    in
    (
        incr next_label;
        let after_label = !next_label
        in
        (
            incr next_label;
            gen_label begin_label; (* start while *)
            gen_br_expr scope 0 expr; (* guard expression *)
            (* exit while if guard is false *)
            gen_binop "branch_on_false" 0 after_label;
            gen_br_stmts scope stmts;
            (* back to start of loop *)
            gen_unop "branch_uncond" begin_label;
            gen_label after_label (* exit while *)
        )
    )

(* Generate instruction for expressions *)
and gen_br_expr scope nreg = function
    | Ebool(bool_const) ->
    (   (* evaluate bool constant to int *)
        if bool_const then gen_int_const nreg 1
        else gen_int_const nreg 0
    )
    | Eint(int_const) -> gen_int_const nreg int_const
    | Efloat(float_const) -> gen_real_const nreg float_const
    | Eparen(expr) -> gen_br_expr scope nreg expr
    | Ebinop(lexpr,optr,rexpr) ->
        gen_br_expr_binop scope nreg lexpr optr rexpr
    | Eunop(optr,expr) -> gen_br_expr_unop scope nreg optr expr
    | Eelem(elem) ->
    (   (* variable or array indexing *)
        match elem with
        | Elem(id,None) -> gen_br_expr_id scope nreg id
        | Elem(id,Some idxs) -> gen_br_expr_array_val scope nreg id idxs
    )

(* Generate instruction for snick binary operation *)
and gen_br_expr_binop scope nreg lexpr optr rexpr =
    let lexpr_type = get_expr_type scope lexpr      (* LHS type *)
    and rexpr_type = get_expr_type scope rexpr      (* RHS type *)
    and lexpr_reg_usage = get_reg_usage scope lexpr (* LHS register *)
    and rexpr_reg_usage = get_reg_usage scope rexpr (* RHS register *)
    and lexpr_nreg = ref 0  (* new register for LHS *)
    and rexpr_nreg = ref 0  (* new register for RHS *)
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

        (* check div_by_0 *)
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
            gen_br_expr_binop_numeric
                "real" nreg !lexpr_nreg !rexpr_nreg optr
        else
            gen_br_expr_binop_numeric
                "int" nreg !lexpr_nreg !rexpr_nreg optr
    )

(* Get use of register by an expression in scope *)
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
            if optr = Op_div then (max reg_usage_total 2)
            else reg_usage_total
        )
    )
    | Eunop(optr,expr) ->
    (
        let expr_reg_usage = get_reg_usage scope expr
        in
        (
            if optr = Op_minus then (max expr_reg_usage 1)
            else expr_reg_usage
        )
    ) 
    | Eelem(elem) -> (* other elements *)
    (
        match elem with
        | Elem(id,None) -> 0
        | Elem(id,Some idxs) -> (* for array *)
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

(* Generate instruction for snick binary operation of bool types *)
and gen_br_expr_binop_bool scope nreg lexpr_nreg rexpr_nreg = function
    | Op_or -> gen_triop "or" nreg lexpr_nreg rexpr_nreg
    | Op_and -> gen_triop "and" nreg lexpr_nreg rexpr_nreg
    | Op_eq -> gen_triop "cmp_eq_int" nreg lexpr_nreg rexpr_nreg
    | Op_ne -> gen_triop "cmp_ne_int" nreg lexpr_nreg rexpr_nreg
    | _ -> error_illegal_optr "" "bool"

(* Generate instruction for snick binary operation of numeric types *)
and gen_br_expr_binop_numeric type_str nreg lexpr_nreg rexpr_nreg = function
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
    | _ -> error_illegal_optr "" "numeric"

(* Generate instruction for snick unary operation *)
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
        else error_invalid_operation ""
    )

(* Generate loading instructions for expressions *)
and gen_br_expr_id scope nreg id =
    let (symkind,_,nslot,_) = Hashtbl.find (get_scope_st scope) id
    in
    (
        match symkind with
        | SYM_PARAM_REF -> (* if the expression is a ref parameter *)
        (
            (* load referenced register *)
            gen_binop "load" nreg nslot;
            gen_binop "load_indirect" nreg nreg
        )
        | _ -> gen_binop "load" nreg nslot
    )

(* Append epilogue section of a procedure (scope) to brill program *)
and gen_br_epilogue scope =
    gen_comment "epilogue";
    gen_unop "pop" (get_scope_nslot scope);
    gen_return ""

(* Append a comment to brill program *)
and gen_comment comment =
    brprog := List.append !brprog [BrComment(comment)]

(* Append call procedure instruction to brill program *)
and gen_call proc_id =
    brprog := List.append !brprog [BrOp(OpCall(proc_id))]

(* Append halt instruction to brill program *)
and gen_halt _ = 
    brprog := List.append !brprog [BrOp(OpHalt)]

(* Append procedure name as label to brill program *)
and gen_proc_label proc_id =
    brprog := List.append !brprog [BrProc(proc_id)]

(* Append a label to brill program *)
and gen_label nlabel = 
    brprog := List.append !brprog [BrLabel(nlabel)]

(* Append int const declaration to brill program *)
and gen_int_const nreg int_const =
    brprog := List.append !brprog [BrOp(OpIntConst(nreg,int_const))]

(* Append float const declaration to brill program *)
and gen_real_const nreg real_const =
    brprog := List.append !brprog [BrOp(OpRealConst(nreg,real_const))]

(* Append string const declaration to brill program *)
and gen_string_const nreg string_const =
    brprog := List.append !brprog [BrOp(OpStringConst(nreg,string_const))]

(* Append return command to brill program *)
and gen_return _ = 
    brprog := List.append !brprog [BrOp(OpReturn)]

(* Append debug stack to brill program *)
and gen_debug_stack _ =
    brprog := List.append !brprog [BrOp(OpDebugStack)]

(* Append unary operation to brill program *)
and gen_unop op x =
    let line = match op with
                | "push" -> BrOp(OpPush(x))
                | "pop" -> BrOp(OpPop(x))
                | "branch_uncond" -> BrOp(OpBranchUncond(x))
                | "debug_reg" -> BrOp(OpDebugReg(x))
                | "debug_slot" -> BrOp(OpDebugSlot(x))
                | _ -> failwith ("operation "^op^" not yet supported")
    in brprog := List.append !brprog [line]

(* Append binary operation to brill program *)
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
    in brprog := List.append !brprog [line]

(* Append tri operation to brill program *)
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
    in brprog := List.append !brprog [line]

(* Append builtin call to brill program *)
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
    in brprog := List.append !brprog [line]
