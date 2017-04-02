(* pretty-printer converts from snick source code to well-formed style *)
open Snick_ast
open Format

let print_program fmtr = function
    | [] -> ()
    | x::[] -> print_proc fmtr x
    | x::xs -> print_proc fmtr x; fprintf fmtr "@."; print_procs fmtr xs

let print_proc fmtr (header, body) =
    fprintf fmtr "@[<v>proc %a@;<0 4>@[<v>%a@]@,end@]@." (print_proc_header fmtr header) (print_proc_body fmtr body)

let print_proc_header fmtr (ident, params) =
    fprintf fmtr "%s (%a)" ident (print_params fmtr params)

let print_params fmtr = function
    | [] -> ()
    | x::[] -> print_param fmtr x
    | x::xs -> fprintf fmtr "%a, " (print_param fmtr x); print_params fmtr xs

let print_param fmtr (indicator, param_type, ident) =
    fprintf fmtr "%a %a %s" (print_param_indc fmtr indicator) (print_type fmtr param_type) ident

let print_param_indc fmtr = function
    | Val -> fprintf fmtr "val"
    | Ref -> fprintf fmtr "ref"

let print_type fmtr = function
    | Bool -> fprintf fmtr "bool"
    | Int -> fprintf fmtr "int"
    | Float -> fprintf fmtr "float"

let print_proc_body fmtr (decls, stmts) =
    print_decls fmtr decls; fprintf fmtr "@."; print_stmts fmtr stmts

let print_decls fmtr = function
    | [] -> ()
    | x::xs -> print_decl fmtr x; print_decls fmtr xs

let print_stmts fmtr = function
    | [] -> ()
    | x::xs -> print_stmt fmtr x; print_stmts fmtr xs

let print_decl fmtr (var_type, variable) =
    fprintf fmtr "%a %a;@." (print_type fmtr var_type) (print_var fmtr variable)

let print_var fmtr = function
    | Single_variable ident -> fprintf fmtr "%s" ident
    | Array_variable (ident, itvls) -> "%s[%a]" ident (print_itvls fmtr itvls)

let print_itvls fmtr =
    | [] -> ()
    | x::[] -> fprintf "%a" (print_itvl fmtr x)
    | x::xs -> fprintf "%a,%a" (print_itvl fmtr x) (print_itvls fmtr xs)

let print_itvl fmtr (st_pnt, end_pnt) =
    fprintf "%d..%d" st_pnt end_pnt

let print_stmt fmtr =
    | Assign (elem, expr) -> fprintf fmtr "%a := %a;@." (print_elem fmtr elem) (print_expr fmtr expr)
    | Read elem -> fprintf fmtr "read %a;@." (print_elem fmtr elem)
    | Write expr -> fprintf fmtr "write %a;@." (print_expr fmtr expr)
    | Call (ident, exprs) -> fprintf fmtr "%s(%a);@." ident (print_exprs fmtr exprs)
    | If_then (expr, stmts) -> fprintf fmtr "if %a then@;<0 4>@[%a@]fi@." (print_expr fmtr expr) (print_stmts fmtr stmts)
    | If_then_else (expr, then_stmts, else_stmts) -> fprintf fmtr "if %a then@;<0 4>@[%a@]else@;<0 4>@[%a@]fi@." (print_expr fmtr expr) (print_stmts fmtr then_stmts) (print_stmts fmtr else_stmts)
    | While (expr, stmts) -> fprintf fmtr "while %a do@;<0 4>@[%a@]od@." (print_expr fmtr expr) (print_stmts fmtr stmts)

let print_elem fmtr = function
    | Single_elem ident -> fprintf fmtr "%s" ident
    | Array_elem (ident, idxs) -> fprintf fmtr "%s[%a]" ident (print_idxs fmtr idxs)

let print_idxs fmtr = function
    | x::[] -> fprintf fmtr "%d" x
    | x::xs -> fprintf fmtr "%d,%a" x (print_idxs fmtr xs)

let print_expr fmtr = function
    | Elem elem -> print_elem fmtr elem
    | Ebool bool_const -> fprintf fmtr "%B" bool_const
    | Eint int_const -> fprintf fmtr "%d" int_const
    | Efloat float_const -> fprintf fmtr "%f" float_const
    | Ebinop (lexpr, binop, rexpr) -> fprintf fmtr "%a %a %a" (print_expr fmtr lexpr) (print_binop fmtr binop) (print_expr fmtr rexpr)
    | Eunop (unop, expr) -> fprintf fmtr "%a %a" (print_unop fmtr unop) (print_expr fmtr expr)

let print_exprs fmtr = function
    | [] -> ()
    | x::[] -> fprintf fmtr "%a" (print_expr fmtr x)
    | x::xs -> fprintf fmtr "%a,%a" (print_expr fmtr x) (print_exprs fmtr xs)

let print_binop fmtr = function
    | Op_add -> fprintf fmtr "+"
    | Op_sub -> fprintf fmtr "-"
    | Op_mul -> fprintf fmtr "*"
    | Op_div -> fprintf fmtr "/"
    | Op_eq -> fprintf fmtr "="
    | Op_ne -> fprintf fmtr "!="
    | Op_lt -> fprintf fmtr "<"
    | Op_gt -> fprintf fmtr ">"
    | Op_le -> fprintf fmtr "<="
    | Op_ge -> fprintf fmtr ">="
    | Op_and -> fprintf fmtr "and"
    | Op_or -> fprintf fmtr "or"

let print_unop fmtr = function
    | Op_not -> fprintf fmtr "not"
    | Op_minus -> fprintf fmtr "-"