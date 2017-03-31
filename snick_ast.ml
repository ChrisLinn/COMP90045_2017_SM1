(* Specification of an AST for snick *)
type ident = string
type interval = (int*int)

(* Keep aliases intact for pretty printing. *)
type snicktype =
    | Bool
    | Int
    | Float 

type typedef =
    | SingleVarTypeDef of (snicktype * ident)
    | ArrayTypeDef of (snicktype * ident * interval list)

type variable =
    | SingleItem of ident
    | ArrayItem of (ident * int list)

type paratype =
    | Val
    | Ref

type param = (paratype * snicktype * variable)

type pheader = (ident * (param list)) (*can list be empty?*)

type binop =
  | Op_add | Op_sub | Op_mul | Op_div
  | Op_eq | Op_ne | Op_lt | Op_gt | Op_le | Op_ge
  | Op_and | Op_or

type unop =
  | Op_not
  | Op_minus

type expr =
  | Evar of variable
  | Econst of snicktype
  | Ebinop of (expr * binop * expr)
  | Eunop of (unop * expr)

type decl = 
    | VarDecl of (snicktype * ident)
    | ArrayDecl of (snicktype * ident * interval list)

type atom_stmt = 
  | Assign of (variable * expr)
  | Read of variable
  | Write of expr
  | Call of (ident * expr list)
  | Call_without_para of ident
and comps_stmt =
  | IfThen of ( expr * stmt list )
  | IfThenElse of ( expr * stmt list * stmt list )
  | While of ( expr * stmt list )
and stmt = 
  | Atom_stmt of atom_stmt
  | Comps_stmt of comps_stmt

type program = {
  decls : typedef list ;
  stmts : stmt list
}
 
type t = program
