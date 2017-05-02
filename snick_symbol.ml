open Snick_ast

type symbolKind =
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

type sbValType =
    | PVal of param 
    | DVal of decl

type symbol = (symbolKind * symType * sbValType * int * bool * bounds option)
(* 
typedef struct symbol_data {
    SymbolKind  kind;
    SymType     type;
    void        *sym_value;    ??????????????????????
    int         line_no;        seems unuseful so removed
    int         slot;
    BOOL        used;
    Bounds  *bounds;           ??????????????????????
} symbol;
 *)

type scope = Scope of (ident * (string, symbol) Hashtbl.t * param list * int)
(* 
 typedef struct scope_data {
    char *id;                   ?????????????????????
    void *table;                   ?????????????????????
    void *params;                   ?????????????????????
    int line_no;                seems unuseful so removed
    int next_slot;              can be global?????
} scope;
 *)

(* 
type sym_table = (Hashtbl.t * bool) *)
(* 
typedef struct symbol_table {
    void *table;                  ?????????????????/
    BOOL initialised;
} sym_table;
 *)

let ht_inis = 20
let ht_scopes = Hashtbl.create ht_inis

let rec gen_sym_table prog =
    List.iter generate_scope prog

and generate_scope ((proc_id,params),proc_body) =
    create_scope proc_id params;
    generate_params_symbols (Hashtbl.find ht_scopes proc_id) params;
    generate_decls_symbols (Hashtbl.find ht_scopes proc_id) proc_body.decls;

and create_scope scope_id params =
    Hashtbl.add
        ht_scopes
        scope_id
        (Scope(scope_id, (Hashtbl.create ht_inis), params, 0))

and generate_params_symbols scope params =
    List.iter (generate_param_symbol scope) params

and generate_param_symbol
        (Scope(scopeid,ht_st,params,nslot)) (indc,ptype,paramid) =
    let
        sym_kind = sym_kind_from_ast_indc indc
        and
        sym_type = sym_type_from_ast_type ptype
    in
    Hashtbl.add ht_st paramid 
                (sym_kind,sym_type,PVal(indc,ptype,paramid),nslot,false,None); 
    Hashtbl.replace ht_scopes scopeid (Scope(scopeid,ht_st,params,nslot+1));

and generate_decls_symbols scope decls = ()

and get_scope_st (Scope(_,ht_st,_,_)) = ht_st

and sym_type_from_ast_type = function
    | Bool -> SYM_BOOL
    | Int -> SYM_INT
    | Float -> SYM_REAL

and sym_kind_from_ast_indc = function
    | Val -> SYM_PARAM_VAL
    | Ref -> SYM_PARAM_REF