open Snick_analyze
open Snick_symbol

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
    | OP_DEBUG_REG | OP_DEBUG_SLOT | OP_DEBUG_STACK

type opType =
    | OpCall of string
    | OpHalt

type brKind =
    | BR_BUILTIN
    | BR_PROC
    | BR_LABEL
    | BR_OP
    | BR_COMMENT

(* type brValueType = *)

type brLine =
    | BrOp of opType


type brLines = brLine list option

type brProg = brLines

let brprog = ref [BrOp(OpCall("main"));BrOp(OpHalt)]


let rec compile prog =
    analyse prog;
    let brprog = gen_br_program prog in
        print_lines brprog
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

and print_lines lines = ()

and gen_br_program prog =
    (* gen_call "main";  *)
    (* gen_halt; *)
(*     gen_oz_out_of_bounds;
    gen_oz_div_by_zero; *)
    List.iter gen_br_proc prog

and gen_call proc_id =
    brprog := List.append !brprog [BrOp(OpCall(proc_id))]

and gen_br_proc proc = ()