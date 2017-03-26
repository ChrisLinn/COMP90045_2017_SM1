(* Specification of an AST for snick *)
type ident = string
type dimension = string
type index = Int

(* Keep aliases intact for pretty printing. *)
type snick_const_type =
    | Bool
    | Int
    | Float 

type typedef =
    | Var of (snick_const_type * ident)
    | Array of (snick_const_type * ident * dimension)

type lvalue = 
    | LId of ident
    | LId_with_index of (ident * index)

type binop =
  | Op_add | Op_sub | Op_mul | Op_div
  | Op_eq | Op_ne | Op_lt | Op_gt | Op_le | Op_ge
  | Op_and | Op_or

type unop =
  | Op_not
  | Op_minus

type expr =
  | Eid of lvalue
  | Eid_with_expr_list of (lvalue * (expr list))
  | Econst of snick_const_type
  | Ebinop of (expr * binop * expr)
  | Eunop of (unop * expr)

(* up to here -------------------------------------------------------------*)

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
