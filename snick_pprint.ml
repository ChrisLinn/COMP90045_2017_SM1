(* pretty-printer converts from snick source code to well-formed style *)
open Snick_ast
open Format

let rec print_program fmtr prog = print_procs fmtr prog.procs

and print_procs fmtr = function
    | [] -> ()
    | x::[] -> print_proc fmtr x
    | x::xs -> print_proc fmtr x; print_newline (); print_procs fmtr xs
    (* | x::xs -> fprintf fmtr "%a@.%a" print_proc x print_procs xs *)

and print_proc fmtr (header, body) =
    fprintf fmtr "@[<v>proc %a@;<0 4>@[<v>%a@]@,end@]@." print_proc_header header print_proc_body body

and print_proc_header fmtr (ident, params) =
    fprintf fmtr "%s (%a)" ident print_params params

and print_params fmtr = function
    | [] -> ()
    | x :: [] -> print_param fmtr x
    | x :: xs -> print_param fmtr x; fprintf fmtr "%s" ", "; print_params fmtr xs

and print_param fmtr (indicator, param_type, ident) =
    fprintf fmtr "%a %a %s" print_param_indc indicator print_type param_type ident

and print_param_indc fmtr = function
    | Val -> fprintf fmtr "%s" "val"
    | Ref -> fprintf fmtr "%s" "ref"

and print_type fmtr = function
    | Bool -> fprintf fmtr "%s" "bool"
    | Int -> fprintf fmtr "%s" "int"
    | Float -> fprintf fmtr "%s" "float"

and print_proc_body fmtr prog_body =
    print_decls fmtr prog_body.decls;
    fprintf fmtr "@;";
    print_stmts fmtr prog_body.stmts;
    
and print_decls fmtr = function
    | [] -> ()
    | x :: [] -> print_decl fmtr x
    | x :: xs -> fprintf fmtr "%a@;%a" print_decl x print_decls xs
    (*| x :: xs -> print_decl fmtr x; fprintf fmtr "@;<0 4>"; print_decls fmtr xs*)

and print_stmts fmtr = function
    | [] -> ()
    | x :: [] -> print_stmt fmtr x
    | x :: xs -> fprintf fmtr "%a@;%a" print_stmt x print_stmts xs
    (*| x :: xs -> print_stmt fmtr x; fprintf fmtr "@;"; print_stmts fmtr xs*)

and print_decl fmtr (var_type, variable) =
    fprintf fmtr "%a %a;" print_type var_type print_var variable

and print_var fmtr = function
    | Single_variable ident -> fprintf fmtr "%s" ident
    | Array_variable (ident, itvls) -> fprintf fmtr "%s[%a]" ident print_itvls itvls

and print_itvls fmtr = function
    | [] -> ()
    | x::[] -> fprintf fmtr "%a" print_itvl x
    | x::xs -> fprintf fmtr "%a,%a" print_itvl x print_itvls xs

and print_itvl fmtr (st_pnt, end_pnt) =
    fprintf fmtr "%d..%d" st_pnt end_pnt

and print_stmt fmtr = function
    | Assign (elem, expr) -> fprintf fmtr "%a := %a;" print_elem elem print_expr expr
    | Read elem -> fprintf fmtr "read %a;" print_elem elem
    | Write expr -> fprintf fmtr "write %a;" print_expr expr
    | Call (ident, exprs) -> fprintf fmtr "%s(%a);" ident print_exprs exprs
    | If_then (expr, stmts) -> fprintf fmtr "if %a then@;<0 4>@[<v>%a@]@;fi" print_expr expr print_stmts stmts
    | If_then_else (expr, then_stmts, else_stmts) -> fprintf fmtr "if %a then@;<0 4>@[<v>%a@]@;else@;<0 4>@[<v>%a@]@;fi" print_expr expr print_stmts then_stmts print_stmts else_stmts
    | While (expr, stmts) -> fprintf fmtr "while %a do@;<0 4>@[<v>%a@]@;od" print_expr expr print_stmts stmts

and print_elem fmtr = function
    | Single_elem ident -> fprintf fmtr "%s" ident
    | Array_elem (ident, idxs) -> fprintf fmtr "%s[%a]" ident print_idxs idxs

and print_idxs fmtr = function
    | [] -> ()
    | x::[] -> fprintf fmtr "%d" x
    | x::xs -> fprintf fmtr "%d," x; print_idxs fmtr xs

and print_expr fmtr = function
    | Eelem elem -> print_elem fmtr elem
    | Ebool bool_const -> fprintf fmtr "%B" bool_const
    | Eint int_const -> fprintf fmtr "%d" int_const
    | Efloat float_const -> fprintf fmtr "%f" float_const
    | Ebinop (lexpr, binop, rexpr) -> fprintf fmtr "%a %a %a" print_expr lexpr print_binop binop print_expr rexpr
    | Eunop (unop, expr) -> fprintf fmtr "%a %a" print_unop unop print_expr expr

and print_exprs fmtr = function
    | [] -> ()
    | x::[] -> fprintf fmtr "%a" print_expr x
    | x::xs -> fprintf fmtr "%a,%a" print_expr x print_exprs xs

and print_binop fmtr = function
    | Op_add -> fprintf fmtr "%s" "+"
    | Op_sub -> fprintf fmtr "%s" "-"
    | Op_mul -> fprintf fmtr "%s" "*"
    | Op_div -> fprintf fmtr "%s" "/"
    | Op_eq -> fprintf fmtr "%s" "="
    | Op_ne -> fprintf fmtr "%s" "!="
    | Op_lt -> fprintf fmtr "%s" "<"
    | Op_gt -> fprintf fmtr "%s" ">"
    | Op_le -> fprintf fmtr "%s" "<="
    | Op_ge -> fprintf fmtr "%s" ">="
    | Op_and -> fprintf fmtr "%s" "and"
    | Op_or -> fprintf fmtr "%s" "or"

and print_unop fmtr = function
    | Op_not -> fprintf fmtr "%s" "not"
    | Op_minus -> fprintf fmtr "%s" "-"