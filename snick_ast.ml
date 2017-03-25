(* Specification of an AST for snick *)
type ident = string
type dimension = string
type index = Int

(* Keep aliases intact for pretty printing. *)
type snicktype =
	| Bool
	| Int
	| Float

type typedef =
    | (snicktype * ident)
    | (snicktype * ident * dimension)

type lvalue = 
    | LId of ident
    | LIdWithIndex of (ident * index)

type binop =
  | Op_add | Op_sub | Op_mul | Op_div
  | Op_eq | Op_ne | Op_lt | Op_gt | Op_le | Op_ge
  | Op_and | Op_or

type unop =
  | Op_not
  | Op_minus

(*-------------------------------------------------------------------------*)

type expr =
  | Ebool of bool
  | Eint of int
  | Elval of lvalue
  | Ebinop of (expr * binop * expr)
  | Eunop of (unop * expr)

(* Will need to AST elements with additional data.  *)
type rvalue =
  | Rexpr of expr

type decl = (ident * beantype)

type stmt = 
  | Assign of (lvalue * rvalue)
  | Read of lvalue
  | Write of expr

type program = {
  decls : typedef list ;
  stmts : stmt list
}
 
type t = program
