(* Specification of an AST for snick *)

type ident = string

(* primitive types *)
type snicktype =
    | Bool
    | Int
    | Float 

type interval = (int * int)

type variable =
    | Single_variable of ident
    | Array_variable of (ident * (interval list))

type decl = (snicktype * variable)

type elem =
    | Single_elem of ident
    | Array_elem of (ident * (int list))

type binop =
    | Op_add | Op_sub | Op_mul | Op_div
    | Op_eq | Op_ne | Op_lt | Op_gt | Op_le | Op_ge
    | Op_and | Op_or

type unop =
    | Op_not
    | Op_minus

type expr =
    (* variable element expression*)
    | Eelem of elem
    (* constant expression *)
    | Ebool of bool
    | Eint of int
    | Efloat of float
    (* opetarion expression *)
    | Ebinop of (expr * binop * expr)
    | Eunop of (unop * expr)

type stmt =
    (* Atomic statements *)
    | Assign of (elem * expr)
    | Read of elem
    | Write of expr
    | Call of (ident * (expr list))
    (* Composite statements *)
    | If_then of (expr * (stmt list))
    | If_then_else of (expr * (stmt list) * (stmt list))
    | While of (expr * (stmt list))

type proc_body = {
  decls : decl list ;
  stmts : stmt list
}

type param_indc =
    | Val
    | Ref

type param = (param_indc * snicktype * ident)

type proc_header = (ident * (param list))

type proc = (proc_header * proc_body)

type program = { procs: proc list }

(* root node of the ast *)
type t = program