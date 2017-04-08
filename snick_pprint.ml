(*
** File:          snick_pprint.ml
** Description:   Pretty-printer converts from snick source code to well-formed style.
** Last Modified: 
** 
** Group name: Mainframe
** 
** Member names   | usernames
** Xianzhuo REN   | xianzhuor 
** Haoyu LIN      | haoyul3
** Zequn MA       | zequnm
*)

open Snick_ast
open Format

(* Print program as list of procedures. *)
let rec print_program fmtr prog = print_procs fmtr prog

(* Print list of procedures. *)
and print_procs fmtr = function
    | [] -> ()
    | x::[] -> fprintf fmtr "%a@," print_proc x
    | x::xs -> fprintf fmtr "%a@,%a" print_proc x print_procs xs

(* Print a single procedure. *)
and print_proc fmtr (header, body) =
    fprintf fmtr "@[<v>proc %a@;<0 4>@[<v>%a@]@,end@]@." print_proc_header header print_proc_body body

(* Print procedure header. *)
and print_proc_header fmtr (ident, params) =
    fprintf fmtr "%s (%a)" ident print_params params

(* Print the list of parameters in header. *)
and print_params fmtr = function
    | [] -> ()
    | x :: [] -> fprintf fmtr "%a" print_param x
    | x :: xs -> fprintf fmtr "%a, %a" print_param x print_params  xs

(* Print a single procedure parameter. *)
and print_param fmtr (indicator, param_type, ident) =
    fprintf fmtr "%a %a %s" print_param_indc indicator print_type param_type ident

(* Print the indicator of a procedure parameter. *)
and print_param_indc fmtr = function
    | Val -> fprintf fmtr "%s" "val"
    | Ref -> fprintf fmtr "%s" "ref"

(* Print the type of a procedure parameter. *)
and print_type fmtr = function
    | Bool -> fprintf fmtr "%s" "bool"
    | Int -> fprintf fmtr "%s" "int"
    | Float -> fprintf fmtr "%s" "float"

(* Print procedure body as a list of declarations followed by a list of statements. *)
and print_proc_body fmtr prog_body =
    fprintf fmtr "%a@,%a" print_decls prog_body.decls print_stmts prog_body.stmts

(* Print the list of declarations. *)
and print_decls fmtr = function
    | [] -> ()
    | x :: [] -> fprintf fmtr "%a@," print_decl x
    | x :: xs -> fprintf fmtr "%a@,%a" print_decl x print_decls xs

(* Print the list of statements. *)
and print_stmts fmtr = function
    | [] -> ()
    | x :: [] -> fprintf fmtr "%a" print_stmt x
    | x :: xs -> fprintf fmtr "%a@,%a" print_stmt x print_stmts xs

(* Print a single declaration. *)
and print_decl fmtr (var_type, variable) =
    fprintf fmtr "%a %a;" print_type var_type print_var variable

(* Print a variable. *)
and print_var fmtr = function
    | Single_variable ident -> fprintf fmtr "%s" ident
    | Array_variable (ident, itvls) -> fprintf fmtr "%s[%a]" ident print_itvls itvls

(* Print list of intervals. *)
and print_itvls fmtr = function
    | [] -> ()
    | x::[] -> fprintf fmtr "%a" print_itvl x
    | x::xs -> fprintf fmtr "%a,%a" print_itvl x print_itvls xs

(* Print a single interval *)
and print_itvl fmtr (st_pnt, end_pnt) =
    fprintf fmtr "%d..%d" st_pnt end_pnt

(* Print statement. *)
and print_stmt fmtr = function
    | Atom_stmt atom_stmt -> fprintf fmtr "%a" print_atom_stmt atom_stmt
    | Comps_stmt comps_stmt -> fprintf fmtr "%a" print_comps_stmt comps_stmt

(* Print atomic statement. *)
and print_atom_stmt fmtr = function
    | Assign (elem, expr) -> fprintf fmtr "%a := %a;" print_elem elem print_expr expr
    | Read elem -> fprintf fmtr "read %a;" print_elem elem
    | Write expr -> fprintf fmtr "write %a;" print_expr expr
    | Call (ident, exprs) -> fprintf fmtr "%s(%a);" ident print_exprs exprs

(* Print composite statement. *)
and print_comps_stmt fmtr = function
    | If_then (expr, stmts) -> fprintf fmtr "if %a then@;<0 4>@[<v>%a@]@,fi" 
        print_expr expr print_stmts stmts
    | If_then_else (expr, then_stmts, else_stmts) -> 
        fprintf fmtr "if %a then@;<0 4>@[<v>%a@]@,else@;<0 4>@[<v>%a@]@,fi" 
        print_expr expr print_stmts then_stmts print_stmts else_stmts
    | While (expr, stmts) -> fprintf fmtr "while %a do@;<0 4>@[<v>%a@]@,od" 
        print_expr expr print_stmts stmts

(* Print element to be assigned to or be read. *)
and print_elem fmtr = function
    | Single_elem ident -> fprintf fmtr "%s" ident
    | Array_elem (ident, idxs) -> fprintf fmtr "%s[%a]" ident print_idxs idxs

(* Print indexing. *)
and print_idxs fmtr = function
    | [] -> ()
    | x::[] -> fprintf fmtr "%a" print_idx x
    | x::xs -> fprintf fmtr "%a,%a" print_idx x print_idxs xs

(* Print a index number. *)
and print_idx fmtr = fprintf fmtr "%d"

(* Print an expression. *)
and print_expr fmtr = function
    | Eelem elem -> fprintf fmtr "%a" print_elem elem
    | Ebool bool_const -> fprintf fmtr "%B" bool_const
    | Eint int_const -> fprintf fmtr "%d" int_const
    | Efloat float_const -> fprintf fmtr "%f" float_const
    | Estring string_const -> fprintf fmtr "%s" string_const
    (* Parenthsis to be printed (or removed) in other functions. 
       Any parenthsis on the right of an operator will be preserved.
    *)
    | Eparen expr -> fprintf fmtr "%a" print_expr expr
    | Ebinop bin_expr -> fprintf fmtr "%a" print_binop bin_expr
    | Eunop un_expr -> fprintf fmtr "%a" print_unop un_expr

(* Print expressions. *)
and print_exprs fmtr = function
    | [] -> ()
    | x::[] -> fprintf fmtr "%a" print_expr x
    | x::xs -> fprintf fmtr "%a, %a" print_expr x print_exprs xs

(* Print binary operations. 
** Each operation will be printed differently according to their precedence.
*)
and print_binop fmtr (lexpr, binop, rexpr) = match binop with
    | Op_add -> fprintf fmtr "%a" print_low_expr (lexpr, "+", rexpr)
    | Op_sub -> fprintf fmtr "%a" print_low_expr (lexpr, "-", rexpr)
    | Op_mul -> fprintf fmtr "%a" print_hgh_expr (lexpr, "*", rexpr)
    | Op_div -> fprintf fmtr "%a" print_hgh_expr (lexpr, "/", rexpr)
    | Op_eq -> fprintf fmtr "%a" print_comp_expr (lexpr, "=", rexpr)
    | Op_ne -> fprintf fmtr "%a" print_comp_expr (lexpr, "!=", rexpr)
    | Op_lt -> fprintf fmtr "%a" print_comp_expr (lexpr, "<", rexpr)
    | Op_gt -> fprintf fmtr "%a" print_comp_expr (lexpr, ">", rexpr)
    | Op_le -> fprintf fmtr "%a" print_comp_expr (lexpr, "<=", rexpr)
    | Op_ge -> fprintf fmtr "%a" print_comp_expr (lexpr, ">=", rexpr)
    | Op_and -> fprintf fmtr "%a" print_and_expr (lexpr, rexpr)
    | Op_or -> fprintf fmtr "%a" print_or_expr (lexpr, rexpr)

(* Print unary operators (not and unary minus). 
** Since an expression is always on the right side of and unary operator,
** any parenthisis will be preserved.
*)
and print_unop fmtr = function
    | (Op_not, Eparen expr) -> fprintf fmtr "not (%a)" print_expr expr
    | (Op_minus, Eparen expr) -> fprintf fmtr "-(%a)" print_expr expr
    | (Op_not, expr) -> fprintf fmtr "not %a" print_expr expr
    | (Op_minus, expr) -> fprintf fmtr "-%a" print_expr expr

(* Print high precedence binary operators (multiply and divide). 
** Any parenthsis on left side will be removed if the left side
** operation has the same of higher precedence (/, *, unary minus).
** Any parenthsis on the right is preserved.
*)
and print_hgh_expr fmtr = function
    | (Eparen lexpr, optr, Eparen rexpr) -> begin
        match lexpr with
        | Ebinop (_, Op_mul, _) -> fprintf fmtr "%a %s (%a)" print_expr lexpr optr print_expr rexpr
        | Ebinop (_, Op_div, _) -> fprintf fmtr "%a %s (%a)" print_expr lexpr optr print_expr rexpr
        | Eunop (Op_minus, _) -> fprintf fmtr "%a %s (%a)" print_expr lexpr optr print_expr rexpr
        | _ -> fprintf fmtr "(%a) %s (%a)" print_expr lexpr optr print_expr rexpr
    end
    | (Eparen lexpr, optr, rexpr) -> begin
        match lexpr with
        | Ebinop (_, Op_mul, _) -> fprintf fmtr "%a %s %a" print_expr lexpr optr print_expr rexpr
        | Ebinop (_, Op_div, _) -> fprintf fmtr "%a %s %a" print_expr lexpr optr print_expr rexpr
        | Eunop (Op_minus, _) -> fprintf fmtr "%a %s %a" print_expr lexpr optr print_expr rexpr
        | _ -> fprintf fmtr "(%a) %s %a" print_expr lexpr optr print_expr rexpr
    end
    | (lexpr, optr, Eparen rexpr) -> fprintf fmtr "%a %s (%a)" print_expr lexpr optr print_expr rexpr
    | (lexpr, optr, rexpr) -> fprintf fmtr "%a %s %a" print_expr lexpr optr print_expr rexpr

(* Print low precedence binary operators (plus and minus). 
** Any parenthsis on left side will be removed if the left side
** operation has the same of higher precedence (+, -, *, /, unary minus).
** Any parenthsis on the right is preserved.
*)
and print_low_expr fmtr = function
    | (Eparen lexpr, optr, Eparen rexpr) -> begin
        match lexpr with
        | Ebinop (_, Op_mul, _) -> fprintf fmtr "%a %s (%a)" print_expr lexpr optr print_expr rexpr
        | Ebinop (_, Op_div, _) -> fprintf fmtr "%a %s (%a)" print_expr lexpr optr print_expr rexpr
        | Ebinop (_, Op_add, _) -> fprintf fmtr "%a %s (%a)" print_expr lexpr optr print_expr rexpr
        | Ebinop (_, Op_sub, _) -> fprintf fmtr "%a %s (%a)" print_expr lexpr optr print_expr rexpr
        | Eunop (Op_minus, _) -> fprintf fmtr "%a %s (%a)" print_expr lexpr optr print_expr rexpr
        | _ -> fprintf fmtr "(%a) %s (%a)" print_expr lexpr optr print_expr rexpr
    end
    | (Eparen lexpr, optr, rexpr) -> begin
        match lexpr with
        | Ebinop (_, Op_mul, _) -> fprintf fmtr "%a %s %a" print_expr lexpr optr print_expr rexpr
        | Ebinop (_, Op_div, _) -> fprintf fmtr "%a %s %a" print_expr lexpr optr print_expr rexpr
        | Ebinop (_, Op_add, _) -> fprintf fmtr "%a %s %a" print_expr lexpr optr print_expr rexpr
        | Ebinop (_, Op_sub, _) -> fprintf fmtr "%a %s %a" print_expr lexpr optr print_expr rexpr
        | Eunop (Op_minus, _) -> fprintf fmtr "%a %s %a" print_expr lexpr optr print_expr rexpr
        | _ -> fprintf fmtr "(%a) %s %a" print_expr lexpr optr print_expr rexpr
    end
    | (lexpr, optr, Eparen rexpr) -> fprintf fmtr "%a %s (%a)" print_expr lexpr optr print_expr rexpr
    | (lexpr, optr, rexpr) -> fprintf fmtr "%a %s %a" print_expr lexpr optr print_expr rexpr

(* Print all the comparison binary operators (=, !=, <, >, <=, >=). 
** Any parenthsis on left side will be preserved if the left side
** operation has the lower precedence (not, and, or).
** Any parenthsis on the right is preserved.
*)
and print_comp_expr fmtr = function
    | (Eparen lexpr, optr, Eparen rexpr) -> begin
        match lexpr with
        | Ebinop (_, Op_and, _) -> fprintf fmtr "(%a) %s (%a)" print_expr lexpr optr print_expr rexpr
        | Ebinop (_, Op_or, _) -> fprintf fmtr "(%a) %s (%a)" print_expr lexpr optr print_expr rexpr
        | Eunop (Op_not, _) -> fprintf fmtr "(%a) %s (%a)" print_expr lexpr optr print_expr rexpr
        | _ -> fprintf fmtr "%a %s (%a)" print_expr lexpr optr print_expr rexpr
    end
    | (Eparen lexpr, optr, rexpr) -> begin
        match lexpr with
        | Ebinop (_, Op_and, _) -> fprintf fmtr "(%a) %s %a" print_expr lexpr optr print_expr rexpr
        | Ebinop (_, Op_or, _) -> fprintf fmtr "(%a) %s %a" print_expr lexpr optr print_expr rexpr
        | Eunop (Op_not, _) -> fprintf fmtr "(%a) %s %a" print_expr lexpr optr print_expr rexpr
        | _ -> fprintf fmtr "%a %s %a" print_expr lexpr optr print_expr rexpr
    end
    | (lexpr, optr, Eparen rexpr) -> fprintf fmtr "%a %s (%a)" print_expr lexpr optr print_expr rexpr
    | (lexpr, optr, rexpr) -> fprintf fmtr "%a %s %a" print_expr lexpr optr print_expr rexpr

(* Print expression involving and binary operator. 
** Since the 'and' operator's precedence is only higher than 'or',
** unless the expression on left is 'or', any parenthsis on left is 
** unnecessary and will be removed.
** Any parenthsis on the right is preserved.
*)
and print_and_expr fmtr = function
    | (Eparen lexpr, Eparen rexpr) -> begin
        match lexpr with
        | Ebinop (_, Op_or, _) -> fprintf fmtr "(%a) and (%a)" print_expr lexpr print_expr rexpr
        | _ -> fprintf fmtr "%a and (%a)" print_expr lexpr print_expr rexpr
    end
    | (Eparen lexpr, rexpr) -> begin
        match lexpr with
        | Ebinop (_, Op_or, _) -> fprintf fmtr "(%a) and %a" print_expr lexpr print_expr rexpr
        | _ -> fprintf fmtr "%a and %a" print_expr lexpr print_expr rexpr
    end
    | (lexpr, Eparen rexpr) -> fprintf fmtr "%a and (%a)" print_expr lexpr print_expr rexpr
    | (lexpr, rexpr) -> fprintf fmtr "%a and %a" print_expr lexpr print_expr rexpr

(* Print expression involving or binary operator. 
** Since the 'or' operator has the lowest precedence,
** any parenthsis is unnecessary and will be removed.
*)
and print_or_expr fmtr (lexpr, rexpr) = 
    fprintf fmtr "%a or %a" print_expr lexpr print_expr rexpr
