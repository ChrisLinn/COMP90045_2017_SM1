open Snick_ast

type symKind =
    | SYM_LOCAL
    | SYM_PARAM_VAL
    | SYM_PARAM_REF

type symType =
    | SYM_BOOL
    | SYM_REAL
    | SYM_INT

(* Bound on an array symbol object *)
type bound = (int * int * int) (* (lower * upper * offset_size) *)

(* Bound on an array symbol object *)
type bounds = bound list

type symValType =
    | ParamVal of param 
    | DeclVal of decl
(* 
type symbol = (symbolKind * symType * symbValType * int * bool * bounds option) *)
type symbol = (symKind * symType * int * bounds option)
(* 
typedef struct symbol_data {
    SymbolKind  kind;
    SymType     type;
    void        *sym_value;    can be useful (update/optmz)
    int         line_no;        seems unuseful so removed
    int         slot;
    BOOL        used;            can be useful!!!!!!!!!!!
    Bounds  *bounds;
} symbol;
 *)

type scope = Scope of (ident * (string, symbol) Hashtbl.t * param list * int)
(* 
 typedef struct scope_data {
    char *id;                   
    void *table;                   seems scope's symbol table
    void *params;                   
    int line_no;                seems unuseful so removed
    int next_slot;              can be global?????
} scope;
 *)


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