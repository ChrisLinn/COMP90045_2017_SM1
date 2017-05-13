open Snick_ast

type symKind =
    | SYM_LOCAL
    | SYM_PARAM_VAL
    | SYM_PARAM_REF

type symType =
    | SYM_BOOL
    | SYM_REAL
    | SYM_INT

type nslot = int

type bound = interval

type symbol = (symKind * symType * nslot * bound list option)

type htScopeSt = (string, symbol) Hashtbl.t

type scope = Scope of (ident * htScopeSt * param list * nslot)

(* type symValType =
    | ParamVal of param 
    | DeclVal of decl *)


let rec get_scope_id (Scope(id,_,_,_)) = id

and get_scope_st (Scope(_,ht_st,_,_)) = ht_st

and get_scope_params (Scope(_,_,params,_)) = params

and get_scope_nslot (Scope(_,_,_,nslot)) = nslot

and sym_type_from_ast_type = function
    | Bool -> SYM_BOOL
    | Int -> SYM_INT
    | Float -> SYM_REAL

and sym_kind_from_ast_indc = function
    | Val -> SYM_PARAM_VAL
    | Ref -> SYM_PARAM_REF

and ast_type_from_sym_type = function
    | SYM_BOOL -> Bool
    | SYM_INT -> Int
    | SYM_REAL -> Float