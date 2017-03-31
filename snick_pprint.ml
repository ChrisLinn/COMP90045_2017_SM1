(*pretty-printer converts from snick source code to well-formed style *)
open Snick_ast
open Format

let rec print_program fmt prog =
  print_typedefs fmt prog.typedefs;
  print_procs fmt prog.procs

and print_typedefs fmt = function
  | [] -> ()(* 
  | x :: xs -> print_typedef fmt x; print_typedefs fmt xs *)
 
and print_typedef fmt (ident, typespec) = 
  (* each typedef is in a single box, and is printed on one line *)
  (* fprintf fmt "@[typedef@ %a@ %s@]@." print_tspec typespec ident