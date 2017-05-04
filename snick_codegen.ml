open Snick_ast
open Snick_analyze
open Snick_symbol
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
    | OpPush of int
    | OpStore of (int * int)

(* type brKind =
    | BR_BUILTIN
    | BR_PROC
    | BR_LABEL
    | BR_OP
    | BR_COMMENT *)

(* type brValueType = *)

type brLine =
    | BrProc of string
    | BrOp of opType

type brLines = brLine list option

type brProg = brLines

let brprog = ref [BrOp(OpCall("main"));BrOp(OpHalt)]

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

and print_lines = ()

and gen_br_program prog =
    (* gen_call "main";  *)
    (* gen_halt; *)
(*     gen_oz_out_of_bounds;
    gen_oz_div_by_zero; *)
    List.iter gen_br_proc prog

and gen_call proc_id =
    brprog := List.append !brprog [BrOp(OpCall(proc_id))]

and gen_br_proc ((proc_id,params),proc_body) =
    let
        scope = Hashtbl.find ht_scopes proc_id
    in
    begin
        gen_proc_label proc_id;
        gen_oz_prologue scope params proc_body.decls;
        gen_oz_stmts scope proc_body.stmts;
        gen_oz_epilogue scope            
    end

and gen_proc_label proc_id =
    brprog := List.append !brprog [BrProc(proc_id)]

and gen_oz_prologue scope params decls =
    (* gen_comment *)
    gen_unop "push" (get_scope_nslot scope);
    gen_br_params (get_scope_st scope) 0 params;
    gen_br_decls (get_scope_st scope) decls;
    
and gen_br_params scope_ht cnt = function
    | x::xs ->
        begin
            gen_br_param scope_ht cnt x;
            gen_br_params scope_ht (cnt+1) xs
        end
    | [] -> ()

and gen_br_param scope_ht cnt (_, _, param_id) =
    let
        sym = Hashtbl.find scope_ht param_id
    in
    match sym with
    | (_,_,nslot,_) -> gen_biop "store" nslot cnt
    

and gen_br_decls scope decls =
    List.iter gen_br_decl decls

and gen_br_decl decl = ()

and gen_oz_stmts scope stmts = ()

and gen_oz_epilogue scope = ()

(* and gen_unop op x = match op with
    | "push" -> brprog := List.append !brprog [BrOp(OpPush(x))]
    | _ -> ()
 *)
and gen_unop op x =
    let 
        line = match op with
                | "push" -> BrOp(OpPush(x))
                | _ -> raise (Failure ("wrong gen_unop "^op))
    in
    brprog := List.append !brprog [line]

(* and gen_biop op x1 x2 = match op with
    | "store" -> brprog := List.append !brprog [BrOp(OpStore(x1,x2))]
    | _ -> ()
 *)
and gen_biop op x1 x2 =
    let 
        line = match op with
                | "store" -> BrOp(OpStore(x1,x2))
                | _ -> raise (Failure ("wrong gen_biop "^op))
    in
    brprog := List.append !brprog [line]

and gen_triop op x1 x2 x3 =
    let 
        line = match op with
                | _ -> raise (Failure ("wrong gen_triop "^op))
    in
    brprog := List.append !brprog [line]