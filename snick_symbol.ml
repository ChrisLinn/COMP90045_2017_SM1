open Snick_ast

type stackNum = int (* WTF *)
type symbolType =
    | SybParam of (param_indc * snicktype * stackNum)
    | SybVar of (variable * stackNum)
type scope =
    Scope of (string , symbolType) Hashtbl.t
(* 
type htValueType =
    | Scope of (string , htValueType) Hashtbl.t
    | SybParam of (param_indc * snicktype * stackNum)
    | SybVar of (variable * stackNum) 
*)

let ht_init_size = 10
let ht_scope_st = Hashtbl.create ht_init_size
let func_stack_num_hash = Hashtbl.create ht_init_size
let func_param_order_hash_table = Hashtbl.create ht_init_size (* WTF *)


let stack_cnt = ref (-1)
let cur_reg_cnt = ref (-1)
let cur_label_cnt = ref 0

(* 
let top_expr_type = ref None
let cur_expr_type = ref None
 *)


let rec build_tables prog =
    bldht_procs prog

and bldht_procs procs =
    List.iter bldht_proc procs

and bldht_proc (((proc_id:string), params), proc_body) =
    stack_cnt := -1;
    
    (* 
    Hashtbl.add ht_scope_st proc_id (Hashtbl.create ht_init_size);  (* WTF *)
     *)
    Hashtbl.add ht_scope_st proc_id (Scope(Hashtbl.create ht_init_size));  (* WTF *)

    addParams
        (get_scope_st(Hashtbl.find ht_scope_st proc_id))
        (* 
        Hashtbl.find ht_scope_st proc_id
         *)
        params;
    
    addVars
        (get_scope_st(Hashtbl.find ht_scope_st proc_id))
        (* 
        Hashtbl.find ht_scope_st proc_id
         *)
        proc_body.decls;
                
    Hashtbl.add func_stack_num_hash proc_id (!stack_cnt+1);  (* WTF *)

    Hashtbl.add func_param_order_hash_table proc_id ((proc_id, params), proc_body)  (* WTF *)
(* 
and addParams scope_st params =
    List.iter
        (fun (pindc, ptype , pid) -> ( 
            incr stack_cnt;
            Hashtbl.add scope_st pid (SybParam(pindc,ptype,!stack_cnt))
        ))
        params
 *)
and addParams scope_st params =
    List.iter (addParam scope_st) params

and addParam scope_st (pindc, ptype , pid) = 
    incr stack_cnt;
    Hashtbl.add scope_st pid (SybParam(pindc,ptype,!stack_cnt))

and addVars scope_st vars =
    List.iter (addVar scope_st) vars

and addVar scope_st var =
    incr stack_cnt

and get_scope_st scope = match scope with
    | Scope(ht) -> ht