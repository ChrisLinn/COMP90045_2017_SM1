(*pretty-printer converts from snick source code to well-formed style *)
open Snick_ast
open Format

let rec print_program fmt prog =
  print_procs fmt prog.procs

and print_procs fmt procs = ()