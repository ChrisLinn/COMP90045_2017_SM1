type ident = string

type snicktype =
	| Bool
	| Int
	| Float

type typedef = (snicktype * ident)

type lvalue = 
	| LId of ident
	| 
