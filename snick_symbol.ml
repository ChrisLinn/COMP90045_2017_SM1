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

type symbol = (symbolKind * symType * expr * int * bool * bounds option)
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

type scope = (ident * (string, symbol) Hashtbl.t * param list * int)
(* 
 typedef struct scope_data {
    char *id;                   ?????????????????????
    void *table;                   ?????????????????????
    void *params;                   ?????????????????????
    int line_no;                seems unuseful so removed
    int next_slot;
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

let ht_init_size = 20
(* let sym_table = Hashtbl.create ht_init_size
 *)
let rec gen_sym_table prog =
    List.iter generate_scope prog

and generate_scope proc =
    create_scope;
    generate_params_symbols;
    generate_decls_symbols;

and create_scope = ()

and generate_params_symbols = ()

and generate_decls_symbols = ()