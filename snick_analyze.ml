open Snick_ast
open Snick_symbol

let isValid = ref true

(*i guess we dont need to use to return result of gen_sym_table, we could just update it in Snick_symbol.ml*)
let rec analyse prog =
    gen_sym_table prog;
    List.iter analyse_proc prog;
    check_unused_symbols prog;    (*???????*)
    check_main                    (*???????*)

and analyse_proc ((proc_id,_),prog_body) =
    analyse_statements proc_id prog_body.stmts

and analyse_statements proc_id stmts =
    List.iter (analyse_statement proc_id) stmts

and analyse_statement proc_id stmt = match stmt with
    | Assign(_) -> analyse_assign stmt proc_id
    | Read(_) -> analyse_read stmt proc_id
    | Write(_) -> analyse_write stmt proc_id
    | Call(_) -> analyse_call stmt proc_id
    | If_then(_) -> analyse_if_then stmt proc_id
    | If_then_else(_) -> analyse_if_then_else stmt proc_id
    | While(_) -> analyse_while stmt proc_id

and analyse_assign stmt proc_id = ()

and analyse_read stmt proc_id = ()

and analyse_write stmt proc_id = ()

and analyse_call stmt proc_id = ()

and analyse_if_then stmt proc_id = ()

and analyse_if_then_else stmt proc_id = ()

and analyse_while stmt proc_id = ()

and check_unused_symbols prog = ()

and check_main = ()