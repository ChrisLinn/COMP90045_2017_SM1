open Snick_ast
open Snick_symbol

let cur_scope_st = ref None

let rec analyze prog =
    build_tables prog;

    let main_def = find_proc prog "main" in
    analyze_proc main_def;
(* 
    List.iter (fun x ->( match x with 
    |((proc_id, params), proc_body) -> if not (proc_id = "main") then analyze_proc x)) prog
 *)

and analyze_proc ((proc_id, params), proc_body) =
    cur_scope_st := Some (get_scope_st(Hashtbl.find ht_scope_st proc_id));
    
(* 
    start_translate_by_function_declaration proc_id (proc_id,func_param_list);
    start_translate_by_function_variable_declaration proc_id typedefStruct_list;
    start_translate_by_function_stmt_list proc_id stmt_list;
    print_pop_stack_frame (Hashtbl.find func_stack_num_hash proc_id); (*pop stack at the end of function body*)
    print_return ()
     *)

and find_proc procs proc_id =    (*can be improved*)
    try
        match (List.hd procs) with
        | ((procid, params), proc_body) -> if procid = proc_id then List.hd procs  else find_proc (List.tl procs) proc_id
    with
        Failure e-> (raise (Failure "no main procedure\n")) 