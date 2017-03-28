(* Specification of an AST for snick *)
type ident = string
type interval = (Int * Int)

(* Keep aliases intact for pretty printing. *)
type snick_const_type =
    | Bool
    | Int
    | Float 

type typedef =
    | Var of (snick_const_type * ident)
    | Array of (snick_const_type * ident * interval list)

type lvalue = 
    | LId of ident
    | LArrayItem of (ident * expr list)

type binop =
  | Op_add | Op_sub | Op_mul | Op_div
  | Op_eq | Op_ne | Op_lt | Op_gt | Op_le | Op_ge
  | Op_and | Op_or

type unop =
  | Op_not
  | Op_minus

type expr =
  | Eid of lvalue
  | Econst of snick_const_type
  | Ebinop of (expr * binop * expr)
  | Eunop of (unop * expr)

(* Will need to AST elements with additional data.  *)
type rvalue =
  | Rexpr of expr

type decl = 
    | VarDecl of (snick_const_type * ident)
    | ArrayDecl of (snick_const_type * ident * interval list)

type atom_stmt = 
  | Assign of (lvalue * rvalue)
  | Read of lvalue
  | Write of expr
  | Call of (ident * expr list)
  | Call_without_para of ident

type comps_stmt =
  | IfThen of ( expr * stmt list )
  | IfThenElse of ( expr * stmt list * stmt list )
  | While of ( expr * stmt list )

type stmt = 
  | atom_stmt
  | comps_stmt

type program = {
  decls : typedef list ;
  stmts : stmt list
}
 
type t = program
