(*
** File:          snick_br_ast.ml
** Description:   Specification of the abstract syntax tree for Snick,
**                also provides functions for printin out a brill program.
** Last Modified: Mon. 15th May 2017 
** 
** Group name: Mainframe
** 
** Member names   | usernames
** Xianzhuo REN   | xianzhuor 
** Haoyu LIN      | haoyul3
** Zequn MA       | zequnm
*)

open Format

(* Available operations in Brill *)
type opType =
    | OpCall of string
    | OpHalt
    | OpReturn
    | OpIntConst of (int * int)
    | OpRealConst of (int * float)
    | OpStringConst of (int * string)
    | OpDebugReg of int
    | OpDebugSlot of int
    | OpDebugStack
    (*unop*)
    | OpPush of int
    | OpPop of int
    | OpBranchUncond of int
    (*binop*)
    | OpLoad of (int * int)
    | OpStore of (int * int)
    | OpLoadAddress of (int * int)
    | OpLoadIndirect of (int * int)
    | OpStoreIndirect of (int * int)
    | OpBranchOnTrue of (int * int)
    | OpBranchOnFalse of (int * int)
    | OpIntToReal of (int * int)
    | OpNot of (int * int)
    (*triop*)
    | OpOr of (int * int * int)
    | OpAnd of (int * int * int)
    | OpAddInt of (int * int * int)
    | OpSubInt of (int * int * int)
    | OpMulInt of (int * int * int)
    | OpDivInt of (int * int * int)
    | OpCmpEqInt of (int * int * int)
    | OpCmpNeInt of (int * int * int)
    | OpCmpLtInt of (int * int * int)
    | OpCmpLeInt of (int * int * int)
    | OpCmpGtInt of (int * int * int)
    | OpCmpGeInt of (int * int * int)
    | OpAddReal of (int * int * int)
    | OpSubReal of (int * int * int)
    | OpMulReal of (int * int * int)
    | OpDivReal of (int * int * int)
    | OpCmpEqReal of (int * int * int)
    | OpCmpNeReal of (int * int * int)
    | OpCmpLtReal of (int * int * int)
    | OpCmpLeReal of (int * int * int)
    | OpCmpGtReal of (int * int * int)
    | OpCmpGeReal of (int * int * int)
    | OpSubOffset of (int * int * int)

(* Types of builtin calls *)
type bltInType =
    | BltInReadInt
    | BltInReadReal
    | BltInReadBool
    | BltInPrintInt
    | BltInPrintReal
    | BltInPrintBool
    | BltInPrintString

(* Representation of a line in a brill program *)
type brLine =
    | BrProc of string
    | BrOp of opType
    | BrLabel of int
    | BrBltIn of bltInType
    | BrComment of string

type brLines = brLine list option

type brProg = brLines

let indent = "    "
let width = -19

(* Print lines of brill program *)
let rec print_prog prog = List.iter print_line prog

and print_line = function
    | BrProc(proc_id) -> print_br_proc proc_id
    | BrOp(brOp) -> print_br_op brOp
    | BrLabel(nlabel) -> print_br_label nlabel
    | BrBltIn(brBltIn) -> print_br_bltin brBltIn
    | BrComment(brComment) -> print_br_comment brComment

and print_br_proc proc_id =
    fprintf std_formatter "proc_%s:\n" proc_id

and print_br_op = function
    | OpCall(proc_id) ->
        fprintf std_formatter "%s%*s proc_%s\n"
            indent width "call" proc_id
    | OpHalt ->
        fprintf std_formatter "%shalt\n"
            indent
    | OpPush(frame_size) ->
        fprintf std_formatter "%s%*s %d\n"
            indent width "push_stack_frame" frame_size
    | OpPop(frame_size) ->
        fprintf std_formatter "%s%*s %d\n"
            indent width "pop_stack_frame" frame_size
    | OpBranchUncond(nlabel) ->
        fprintf std_formatter "%s%*s label%d\n"
            indent width "branch_uncond" nlabel
    | OpLoad(nreg,nslot) ->
        fprintf std_formatter "%s%*s r%d, %d\n"
            indent width "load" nreg nslot
    | OpStore(nslot,nreg) ->
        fprintf std_formatter "%s%*s %d, r%d\n"
            indent width "store" nslot nreg
    | OpLoadAddress(nreg,nslot) ->
        fprintf std_formatter "%s%*s r%d, %d\n"
            indent width "load_address" nreg nslot
    | OpLoadIndirect(nreg1,nreg2) ->
        fprintf std_formatter "%s%*s r%d, r%d\n"
            indent width "load_indirect" nreg1 nreg2
    | OpStoreIndirect(nreg1,nreg2) ->
        fprintf std_formatter "%s%*s r%d, r%d\n"
            indent width "store_indirect" nreg1 nreg2
    | OpBranchOnTrue(nreg,nlabel) ->
        fprintf std_formatter "%s%*s r%d, label%d\n"
            indent width "branch_on_true" nreg nlabel
    | OpBranchOnFalse(nreg,nlabel) ->
        fprintf std_formatter "%s%*s r%d, label%d\n"
            indent width "branch_on_false" nreg nlabel
    | OpIntToReal(nreg_dest,nreg_scr) ->
        fprintf std_formatter "%s%*s r%d, r%d\n"
            indent width "int_to_real" nreg_dest nreg_scr
    | OpNot(nreg_dest,nreg_scr) ->
        fprintf std_formatter "%s%*s r%d, r%d\n"
            indent width "not" nreg_dest nreg_scr
    | OpOr(nreg_dest,nreg1,nreg2) ->
        fprintf std_formatter "%s%*s r%d, r%d, r%d\n"
            indent width "or" nreg_dest nreg1 nreg2
    | OpAnd(nreg_dest,nreg1,nreg2) ->
        fprintf std_formatter "%s%*s r%d, r%d, r%d\n"
            indent width "and" nreg_dest nreg1 nreg2
    | OpAddInt(nreg_dest,nreg_int1,nreg_int2) ->
        fprintf std_formatter "%s%*s r%d, r%d, r%d\n"
            indent width "add_int" nreg_dest nreg_int1 nreg_int2
    | OpSubInt(nreg_dest,nreg_int1,nreg_int2) ->
        fprintf std_formatter "%s%*s r%d, r%d, r%d\n"
            indent width "sub_int" nreg_dest nreg_int1 nreg_int2
    | OpMulInt(nreg_dest,nreg_int1,nreg_int2) ->
        fprintf std_formatter "%s%*s r%d, r%d, r%d\n"
            indent width "mul_int" nreg_dest nreg_int1 nreg_int2
    | OpDivInt(nreg_dest,nreg_int1,nreg_int2) ->
        fprintf std_formatter "%s%*s r%d, r%d, r%d\n"
            indent width "div_int" nreg_dest nreg_int1 nreg_int2
    | OpCmpEqInt(nreg_dest,nreg_int1,nreg_int2) ->
        fprintf std_formatter "%s%*s r%d, r%d, r%d\n"
            indent width "cmp_eq_int" nreg_dest nreg_int1 nreg_int2
    | OpCmpNeInt(nreg_dest,nreg_int1,nreg_int2) ->
        fprintf std_formatter "%s%*s r%d, r%d, r%d\n"
            indent width "cmp_ne_int" nreg_dest nreg_int1 nreg_int2
    | OpCmpLtInt(nreg_dest,nreg_int1,nreg_int2) ->
        fprintf std_formatter "%s%*s r%d, r%d, r%d\n"
            indent width "cmp_lt_int" nreg_dest nreg_int1 nreg_int2
    | OpCmpLeInt(nreg_dest,nreg_int1,nreg_int2) ->
        fprintf std_formatter "%s%*s r%d, r%d, r%d\n"
            indent width "cmp_le_int" nreg_dest nreg_int1 nreg_int2
    | OpCmpGtInt(nreg_dest,nreg_int1,nreg_int2) ->
        fprintf std_formatter "%s%*s r%d, r%d, r%d\n"
            indent width "cmp_gt_int" nreg_dest nreg_int1 nreg_int2
    | OpCmpGeInt(nreg_dest,nreg_int1,nreg_int2) ->
        fprintf std_formatter "%s%*s r%d, r%d, r%d\n"
            indent width "cmp_ge_int" nreg_dest nreg_int1 nreg_int2
    | OpAddReal(nreg_dest,nreg_real1,nreg_real2) ->
        fprintf std_formatter "%s%*s r%d, r%d, r%d\n"
            indent width "add_real" nreg_dest nreg_real1 nreg_real2
    | OpSubReal(nreg_dest,nreg_real1,nreg_real2) ->
        fprintf std_formatter "%s%*s r%d, r%d, r%d\n"
            indent width "sub_real" nreg_dest nreg_real1 nreg_real2
    | OpMulReal(nreg_dest,nreg_real1,nreg_real2) ->
        fprintf std_formatter "%s%*s r%d, r%d, r%d\n"
            indent width "mul_real" nreg_dest nreg_real1 nreg_real2
    | OpDivReal(nreg_dest,nreg_real1,nreg_real2) ->
        fprintf std_formatter "%s%*s r%d, r%d, r%d\n"
            indent width "div_real" nreg_dest nreg_real1 nreg_real2
    | OpCmpEqReal(nreg_dest,nreg_real1,nreg_real2) ->
        fprintf std_formatter "%s%*s r%d, r%d, r%d\n"
            indent width "cmp_eq_real" nreg_dest nreg_real1 nreg_real2
    | OpCmpNeReal(nreg_dest,nreg_real1,nreg_real2) ->
        fprintf std_formatter "%s%*s r%d, r%d, r%d\n"
            indent width "cmp_ne_real" nreg_dest nreg_real1 nreg_real2
    | OpCmpLtReal(nreg_dest,nreg_real1,nreg_real2) ->
        fprintf std_formatter "%s%*s r%d, r%d, r%d\n"
            indent width "cmp_lt_real" nreg_dest nreg_real1 nreg_real2
    | OpCmpLeReal(nreg_dest,nreg_real1,nreg_real2) ->
        fprintf std_formatter "%s%*s r%d, r%d, r%d\n"
            indent width "cmp_le_real" nreg_dest nreg_real1 nreg_real2
    | OpCmpGtReal(nreg_dest,nreg_real1,nreg_real2) ->
        fprintf std_formatter "%s%*s r%d, r%d, r%d\n"
            indent width "cmp_gt_real" nreg_dest nreg_real1 nreg_real2
    | OpCmpGeReal(nreg_dest,nreg_real1,nreg_real2) ->
        fprintf std_formatter "%s%*s r%d, r%d, r%d\n"
            indent width "cmp_ge_real" nreg_dest nreg_real1 nreg_real2
    | OpSubOffset(nreg_dest,nreg_addr,nreg_offset) ->
        fprintf std_formatter "%s%*s r%d, r%d, r%d\n"
            indent width "sub_offset" nreg_dest nreg_addr nreg_offset
    | OpReturn ->
        fprintf std_formatter "%sreturn\n"
            indent
    | OpIntConst(nreg,int_const) ->
        fprintf std_formatter "%s%*s r%d, %d\n"
            indent width "int_const" nreg int_const
    | OpRealConst(nreg,real_const) ->
        fprintf std_formatter "%s%*s r%d, %f\n"
            indent width "real_const" nreg real_const
    | OpStringConst(nreg,string_const) ->
        fprintf std_formatter "%s%*s r%d, %s\n"
            indent width "string_const" nreg string_const
    | OpDebugReg(nreg) ->
        fprintf std_formatter "%s%*s r%d\n"
            indent width "debug_reg" nreg
    | OpDebugSlot(nslot) ->
        fprintf std_formatter "%s%*s %d\n"
            indent width "debug_slot" nslot
    | OpDebugStack ->
        fprintf std_formatter "%s%*s\n"
            indent width "debug_stack"

and print_br_label nlabel =
    fprintf std_formatter "label%d:\n" nlabel

and print_br_bltin = function
    | BltInReadInt ->
        fprintf std_formatter "%s%*s read_int\n"
            indent width "call_builtin"
    | BltInReadReal ->
        fprintf std_formatter "%s%*s read_real\n"
            indent width "call_builtin"
    | BltInReadBool ->
        fprintf std_formatter "%s%*s read_bool\n"
            indent width "call_builtin"
    | BltInPrintInt ->
        fprintf std_formatter "%s%*s print_int\n"
            indent width "call_builtin"
    | BltInPrintReal ->
        fprintf std_formatter "%s%*s print_real\n"
            indent width "call_builtin"
    | BltInPrintBool ->
        fprintf std_formatter "%s%*s print_bool\n"
            indent width "call_builtin"
    | BltInPrintString ->
        fprintf std_formatter "%s%*s print_string\n"
            indent width "call_builtin"

and print_br_comment brComment =
    fprintf std_formatter "# %s\n" brComment
