open Snick_ast
open Snick_symbol
open Snick_analyze
(* 
type opCode =
    | OP_PUSH_STACK_FRAME | OP_POP_STACK_FRAME
    | OP_HALT
    | OP_LOAD | OP_STORE | OP_LOAD_ADDRESS | OP_LOAD_INDIRECT | OP_STORE_INDIRECT
    | OP_INT_CONST | OP_REAL_CONST | OP_STRING_CONST
    | OP_INT_TO_REAL | OP_MOVE
    | OP_ADD_INT | OP_ADD_REAL | OP_ADD_OFFSET
    | OP_SUB_INT | OP_SUB_REAL | OP_SUB_OFFSET
    | OP_MUL_INT | OP_MUL_REAL | OP_DIV_INT | OP_DIV_REAL
    | OP_CMP_EQ_INT |  OP_CMP_NE_INT |  OP_CMP_GT_INT |  OP_CMP_GE_INT
    | OP_CMP_LT_INT |  OP_CMP_LE_INT |  OP_CMP_EQ_REAL | OP_CMP_NE_REAL
    | OP_CMP_GT_REAL | OP_CMP_GE_REAL | OP_CMP_LT_REAL | OP_CMP_LE_REAL
    | OP_AND | OP_OR | OP_NOT
    | OP_BRANCH_ON_TRUE | OP_BRANCH_ON_FALSE | OP_BRANCH_UNCOND
    | OP_CALL | OP_CALL_BUILTIN | OP_RETURN
    | OP_DEBUG_REG | OP_DEBUG_SLOT | OP_DEBUG_STACK *)

type opType =
    | OpCall of string
    | OpHalt
    (*unop*)
    | OpPush of int
    | OpBranchUncond of int
    (*binup*)
    | OpStore of (int * int)
    | OpLoad of (int * int)
    | OpLoadIndirect of (int * int)
    | OpBranchOnTrue of (int * int)
    | OpBranchOnFalse of (int * int)
    | OpIntToReal of (int * int)
    | OpLoadAddress of (int * int)
    | OpStoreIndirect of (int * int)
    | OpNot of (int * int)
    | OpSubInt of (int * int * int)
    | OpSubReal of (int * int * int)
    | OpReturn
    | OpIntConst of (int * int)
    | OpRealConst of (int * float)
    | OpStringConst of (int * string)

(* type brKind =
    | BR_BUILTIN
    | BR_PROC
    | BR_LABEL
    | BR_OP
    | BR_COMMENT *)

(* type brValueType = *)

type bltInType =
    | BltInPrintBool
    | BltInPrintInt
    | BltInPrintReal
    | BltInPrintString
    | BltInReadBool
    | BltInReadInt
    | BltInReadReal

type brLine =
    | BrProc of string
    | BrOp of opType
    | BrLabel of int
    | BrBltIn of bltInType

type brLines = brLine list option

type brProg = brLines


let brprog = ref []
let next_label = ref 0

let rec compile prog =
    analyse prog;
    gen_br_program prog;
    print_lines
(*i guess we dont need to check table == NULL, we could just check Snick_analyze.isValid*)
(* 
compile(FILE *fp, Program *prog) {
    void *table = analyse(prog);
    if (table == NULL) {
        //Then did not pass semantic analysis. Exit
        report_error_and_exit("Invalid program.");
    }
    OzProgram *ozprog = gen_oz_program(prog, table);
    print_lines(fp, ozprog->start);
    return (int)(!ozprog);
}
*)
    
and strip_paren expr = match expr with
    | Eparen paren_expr -> strip_paren paren_expr
    | _ -> expr

and print_lines = ()

and gen_br_program prog =
    gen_call "main";
    gen_halt;
(*     gen_br_out_of_bounds;
    gen_br_div_by_zero; *)
    List.iter gen_br_proc prog

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
    (* gen_comment *)
    gen_unop "push" (get_scope_nslot scope);
    gen_br_params (get_scope_st scope) 0 params;
    gen_br_decls (get_scope_st scope) decls;
    
and gen_br_params scope_st cnt = function
    | [] -> ()
    | x::xs ->
        (
            gen_br_param scope_st cnt x;
            gen_br_params scope_st (cnt+1) xs
        )

and gen_br_param scope_st cnt (_, _, param_id) =
    let sym = Hashtbl.find scope_st param_id
    in
    match sym with
    | (_,_,nslot,_) -> gen_binop "store" nslot cnt
    
and gen_br_decls scope_st decls =
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
                    let (_,sym_type,_,_) = Hashtbl.find scope_st id
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
                            Hashtbl.find scope_st id
                    in
                    (
                        if sym_type = SYM_REAL then
                            reg := !real_reg
                        else
                            reg := !int_reg;

                        if optn_bounds = None then
                            gen_binop "store" nslot !reg
                        else
                            gen_br_init_array nslot !reg optn_bounds
                    )
                )
            )
            decls;
    )

and gen_br_init_array nslot nreg optn_bounds = ()

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
    let (symkind,symtype,nslot,optn_bounds) = 
        Hashtbl.find (get_scope_st scope) id
    and expr_type = get_expr_type (get_scope_st scope) expr
    in
    (
        gen_br_expr scope 0 expr;
        if ((symtype = SYM_REAL) && (expr_type = SYM_INT)) then
            gen_binop "int_to_real" 0 0;
        if optn_idxs <> None then
        (

        )
        else if symkind = SYM_PARAM_REF then
        (
            gen_binop "load" 1 nslot;
            gen_binop "store_indirect" 1 0
        )
        else
            gen_binop "store" nslot 0
    )

and gen_br_read scope (Elem(id,optn_idxs)) =
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
        if optn_idxs <> None then
        (
        )
        else if symkind = SYM_PARAM_REF then
        (
            gen_binop "load" 1 nslot;
            gen_binop "store_indirect" 1 0
        )
        else
            gen_binop "store" nslot 0
    )

and gen_br_write scope = function
    | Expr(expr) ->
    (
        gen_br_expr scope 0 expr;
        match (get_expr_type (get_scope_st scope) expr) with
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
    let params = get_scope_params (Hashtbl.find ht_scopes proc_id)
    and nreg = ref 0
    and scope_st = get_scope_st scope
    in
    (
    (*try with Invalid_argument if the two lists are determined to have different lengths*)
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
                                    Hashtbl.find scope_st id
                                in
                                (
                                    if symkind = SYM_PARAM_REF then
                                        gen_binop "load" !nreg nslot
                                    else if optn_idxs <> None then
                                        gen_br_expr_array_addr
                                            scope !nreg (Elem(id,optn_idxs))
                                    else
                                        gen_binop "load_address" !nreg nslot
                                )
                            )
                            | _ -> 
                                raise (Failure "can't pass non-elem to a ref")
                        )
                        | (Val,param_type,_) ->
                        (
                            gen_br_expr scope !nreg arg;
                            if (((get_expr_type scope_st arg) = SYM_INT)
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

and gen_br_expr_array_addr scope nreg elem = ()

and gen_br_ifthen scope expr stmts =
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

and gen_br_expr_binop scope nreg lexpr optr rexpr = ()

and gen_br_expr_unop scope nreg optr expr =
    gen_br_expr scope nreg expr;
    let expr_type = get_expr_type (get_scope_st scope) expr
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
            raise (Failure "invalid optr for unop expr!")
    )

and gen_br_expr_id scope nreg id =
    let (symkind,symtype,nslot,_) = Hashtbl.find (get_scope_st scope) id
    in
    (
        match symkind with
        | SYM_PARAM_REF ->
        (
            gen_binop "load" nreg nslot;
            gen_binop "load_indrect" nreg nreg
        )
        | _ -> gen_binop "load" nreg nslot
    )

and gen_br_expr_array_val scope nreg id idxs = ()

and gen_br_epilogue scope =
    gen_unop "pop" (get_scope_nslot scope);
    gen_return

and gen_call proc_id =
    brprog := List.append !brprog [BrOp(OpCall(proc_id))]

and gen_halt =
    brprog := List.append !brprog [BrOp(OpHalt)]

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

and gen_return = 
    brprog := List.append !brprog [BrOp(OpReturn)]

(* and gen_unop op x = match op with
    | "push" -> brprog := List.append !brprog [BrOp(OpPush(x))]
    | _ -> ()
 *)
and gen_unop op x =
    let line = match op with
                | "push" -> BrOp(OpPush(x))
                | "branch_uncond" -> BrOp(OpBranchUncond(x))
                | _ -> raise (Failure ("wrong gen_unop "^op))
    in
    brprog := List.append !brprog [line]

(* and gen_binop op x1 x2 = match op with
    | "store" -> brprog := List.append !brprog [BrOp(OpStore(x1,x2))]
    | _ -> ()
 *)
and gen_binop op x1 x2 =
    let line = match op with
                | "store" -> BrOp(OpStore(x1,x2))
                | "load" -> BrOp(OpLoad(x1,x2))
                | "load_indrect" -> BrOp(OpLoadIndirect(x1,x2))
                | "branch_on_true" -> BrOp(OpBranchOnTrue(x1,x2))
                | "branch_on_false" -> BrOp(OpBranchOnFalse(x1,x2))
                | "int_to_real" -> BrOp(OpIntToReal(x1,x2))
                | "load_address" -> BrOp(OpLoadAddress(x1,x2))
                | "store_indirect" -> BrOp(OpStoreIndirect(x1,x2))
                | "not" -> BrOp(OpNot(x1,x2))
                | _ -> raise (Failure ("wrong gen_binop "^op))
    in
    brprog := List.append !brprog [line]

and gen_triop op x1 x2 x3 =
    let line = match op with
                | "sub_int" -> BrOp(OpSubInt(x1,x2,x3))
                | "sub_real" -> BrOp(OpSubReal(x1,x2,x3))
                | _ -> raise (Failure ("wrong gen_triop "^op))
    in
    brprog := List.append !brprog [line]

and gen_call_builtin bi_func =
    let line = match bi_func with
                | "print_bool" -> BrBltIn(BltInPrintBool)
                | "print_int" -> BrBltIn(BltInPrintInt)
                | "print_real" -> BrBltIn(BltInPrintReal)
                | "print_string" -> BrBltIn(BltInPrintString)
                | "read_bool" -> BrBltIn(BltInReadBool)
                | "read_int" -> BrBltIn(BltInReadInt)
                | "read_real" -> BrBltIn(BltInReadReal)
                | _ -> raise (Failure ("wrong bi_func "^bi_func))
    in
    brprog := List.append !brprog [line]