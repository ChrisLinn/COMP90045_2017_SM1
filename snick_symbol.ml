open Snick_ast


type stackNum = int


(* WTF *)
(* 
type htValueType =
    | HtVfunc of (string , htValueType) Hashtbl.t
    | HtVref_Hash of (snicktype * (string , htValueType) Hashtbl.t)(* stored nest type of typedef*)
    | HtVhash of (snicktype * (string, htValueType) Hashtbl.t)(*self def type*)
    | HtVbool of (snicktype * stackNum) (*Int => stack num*)
    | HtVint of (snicktype * stackNum)
    | HtVref_Int of (snicktype * stackNum)
    | HtVref_Bool of (snicktype * stackNum)(*if snicktype is a ident, need to search through typedef hash table*)
    | HtVintext_Hash of (string , htValueType) Hashtbl.t
    | HtVref_Intext_Hash of (string , htValueType) Hashtbl.t
 *)


let ht_init_size = 10
let symbol_table = Hashtbl.create ht_init_size
let func_table = Hashtbl.create ht_init_size
let func_stack_num_hash = Hashtbl.create ht_init_size
let func_param_order_hash_table = Hashtbl.create ht_init_size (* WTF *)

let stack_cnt = ref (-1)
let cur_reg_cnt = ref (-1)
let cur_label_cnt = ref 0

(* 
let top_expr_type = ref None
let cur_expr_type = ref None
let cur_scope_ht = ref None
 *)


let rec build_tables prog =
    bldht_procs prog

and bldht_procs procs =
    List.iter bldht_proc procs

and bldht_proc ((proc_id, params), proc_body) =
    stack_cnt := -1;
    
    Hashtbl.add symbol_table proc_id func_table;  (* WTF *)

    build_symbol_table_hash_funcDecParamList
        func_table
        params;
    
    build_symbol_table_typedefStruct_list
        func_table
        proc_body.decls;
                
    Hashtbl.add func_stack_num_hash func_name (!stack_count+1);  (* WTF *)

    Hashtbl.add func_param_order_hash_table proc_id ((proc_id, params), proc_body)  (* WTF *)
