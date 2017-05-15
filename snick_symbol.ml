(*
** File:          snick_symbol.ml
** Description:   Defines the data structure and data types for
**                the Snick language.
** Last Modified: Mon. 15th May 2017 
** 
** Group name: Mainframe
** 
** Member names   | usernames
** Xianzhuo REN   | xianzhuor 
** Haoyu LIN      | haoyul3
** Zequn MA       | zequnm
*)

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

(* Representation of a single symbol *)
type symbol = (symKind * symType * nslot * bound list option)

(* Symbol table for a particular scope *)
type htScopeSt = (string, symbol) Hashtbl.t

(* Description of a scope *)
(* identifier of scope (proc), symbol table of this scope, parameters, size*)
type scope = Scope of (ident * htScopeSt * param list * nslot)

let get_scope_id (Scope(id,_,_,_)) = id

let get_scope_st (Scope(_,ht_st,_,_)) = ht_st

let get_scope_params (Scope(_,_,params,_)) = params

let get_scope_nslot (Scope(_,_,_,nslot)) = nslot

let sym_type_from_ast_type = function
    | Bool -> SYM_BOOL
    | Int -> SYM_INT
    | Float -> SYM_REAL

let sym_kind_from_ast_indc = function
    | Val -> SYM_PARAM_VAL
    | Ref -> SYM_PARAM_REF

let ast_type_from_sym_type = function
    | SYM_BOOL -> Bool
    | SYM_INT -> Int
    | SYM_REAL -> Float