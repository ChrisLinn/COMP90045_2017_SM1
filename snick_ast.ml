(* Specification of an AST for snick *)
type ident = string

(* Keep aliases intact for pretty printing. *)
type snicktype =
	| Bool
	| Int
	| Float

type typedef = (snicktype * ident)

type lvalue = 
	| LId of ident
	| 
