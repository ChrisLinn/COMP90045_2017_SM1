(*
** File:          snick_ast.ml
** Description:   Specification of the abstract syntax tree for Snick
** Last Modified: Sun. 9th April 2017
** 
** Group name: Mainframe
** 
** Member names   | usernames
** Xianzhuo REN   | xianzhuor 
** Haoyu LIN      | haoyul3
** Zequn MA       | zequnm
*)

(* identifier *)
type ident = string

(* primitive types *)
type snicktype =
    | Bool
    | Int
    | Float 

(* array dimensions *)
type interval = (int * int)

(* variable representations *)
type variable =
    | Single_variable of ident
    | Array_variable of (ident * interval list)

(* single declation *)
type decl = (snicktype * variable)

(* binary operators *)
type optr =
    | Op_add | Op_sub | Op_mul | Op_div
    | Op_eq | Op_ne | Op_lt | Op_gt | Op_le | Op_ge
    | Op_and | Op_or
    | Op_not
    | Op_minus

(* unary operators *)
type unop =
    | Op_not
    | Op_minus

(* expression *)
type expr =
    (* variable element expression*)
    | Eelem of elem
    (* constant expression *)
    | Ebool of bool
    | Eint of int
    | Efloat of float
    (* expression inside a pair of parentheses *)
    | Eparen of expr
    (* operation expression *)
    | Ebinop of (expr * optr * expr)
    | Eunop of (optr * expr)
(* element to read, write or assign *)
and elem =
    | Single_elem of ident
    | Array_elem of (ident * expr list)

(* Expression that can be written (either an expression or string). *)
type write_expr =
    | Expr of expr
    | String of string

(* statement *)
type stmt =
    | Atom_stmt of atom_stmt
    | Comps_stmt of comps_stmt
and atom_stmt = (* atomic statement *)
    | Assign of (elem * expr)
    | Read of elem
    | Write of write_expr
    | Call of (ident * expr list)
and comps_stmt = (* composite statement *)
    | If_then of (expr * stmt list)
    | If_then_else of (expr * stmt list * stmt list)
    | While of (expr * stmt list)

(* procedure body *)
type proc_body = {
  decls : decl list ;
  stmts : stmt list
}

(* parameter indicator *)
type param_indc =
    | Val
    | Ref

(* procedure parameter *)
type param = (param_indc * snicktype * ident)

(* procedure header *)
type proc_header = (ident * param list)

(* procedure *)
type proc = (proc_header * proc_body)

(* list of procedures *)
type procs = proc list

(* program is a list of procedures *)
type program = procs

(* root node of the ast *)
type t = program