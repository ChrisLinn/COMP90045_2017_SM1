open Snick_ast
open Snick_symbol


let rec analyze prog =
    build_tables prog