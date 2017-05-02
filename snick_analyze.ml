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
    analyse_statements prog_body.stmts proc_id

and analyse_statements stmts proc_id = ()

and check_unused_symbols prog = ()

and check_main = ()