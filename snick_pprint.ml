(* pretty-printer converts from snick source code to well-formed style *)
open Snick_ast
open Format

let print_program fmatr = function
    | [] -> () (* should raise error *)
    | x::[] -> print_proc fmatr x
    | x::xs -> print_proc fmatr x; print_newline (); print_procs fmatr xs

let print_proc fmatr (proc_header, proc_body) =
    print_proc_header fmatr proc_header; print_proc_body fmatr proc_body 

let print_proc_id fmatr ident = fprintf fmatr " "

let print_proc_params fmatr params =  fprintf fmatr " "

(*
let print_proc_header fmatr (ident, params) =



    
    print_proc_id fmatr ident; print_proc_params fmatr params




    print_decls fmt proc.decls;
    print_stmts fmt proc.stmts;

let print_decls fmt = function
    | [] -> ()
    | x::xs -> print_decl fmt x; print_decls fmt xs

let print_stmts fmt = function
    | [] -> ()
    | x::xs -> print_stmt fmt x; print_stmts fmt xs

let print_decl fmt decl = ()

let print_stmt fmt stmt = ()
*)   