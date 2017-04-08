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
    | Atom_stmt atom_stmt -> fprintf fmtr "%a" print_atom_stmt atom_stmt
    | Comps_stmt comps_stmt -> fprintf fmtr "%a" print_comps_stmt comps_stmt

and print_atom_stmt fmtr = function
    | Assign (elem, expr) -> fprintf fmtr "%a := %a;" print_elem elem print_expr expr
    | Read elem -> fprintf fmtr "read %a;" print_elem elem
    | Write expr -> fprintf fmtr "write %a;" print_expr expr
    | Call (ident, exprs) -> fprintf fmtr "%s(%a);" ident print_exprs exprs

and print_comps_stmt fmtr = function
    | If_then (expr, stmts) -> fprintf fmtr "if %a then@;<0 4>@[<v>%a@]@,fi" print_expr expr print_stmts stmts
    | If_then_else (expr, then_stmts, else_stmts) -> fprintf fmtr "if %a then@;<0 4>@[<v>%a@]@,else@;<0 4>@[<v>%a@]@,fi" print_expr expr print_stmts then_stmts print_stmts else_stmts
    | While (expr, stmts) -> fprintf fmtr "while %a do@;<0 4>@[<v>%a@]@,od" print_expr expr print_stmts stmts

and print_elem fmtr = function
    | Single_elem ident -> fprintf fmtr "%s" ident
    | Array_elem (ident, idxs) -> fprintf fmtr "%s[%a]" ident print_idxs idxs

and print_idxs fmtr = function
    | [] -> ()
    | x::[] -> fprintf fmtr "%a" print_idx x
    | x::xs -> fprintf fmtr "%a,%a" print_idx x print_idxs xs

and print_idx fmtr = fprintf fmtr "%d"

and print_expr fmtr = function
    | Eelem elem -> fprintf fmtr "%a" print_elem elem
    | Ebool bool_const -> fprintf fmtr "%B" bool_const
    | Eint int_const -> fprintf fmtr "%d" int_const
    | Efloat float_const -> fprintf fmtr "%f" float_const
    | Estring string_const -> fprintf fmtr "%s" string_const
    | Eparen expr -> fprintf fmtr "%a" print_expr (strip_paren expr)
    | Ebinop bin_expr -> fprintf fmtr "%a" print_binop bin_expr
    | Eunop un_expr -> fprintf fmtr "%a" print_unop un_expr

and print_exprs fmtr = function
    | [] -> ()
    | x::[] -> fprintf fmtr "%a" print_expr x
    | x::xs -> fprintf fmtr "%a, %a" print_expr x print_exprs xs

and print_binop fmtr (lexpr, binop, rexpr) = match binop with
    | Op_add -> fprintf fmtr "%a" print_low_expr (lexpr, "+", rexpr)
    | Op_sub -> fprintf fmtr "%a" print_low_expr (lexpr, "-", rexpr)
    | Op_mul -> fprintf fmtr "%a" print_hgh_expr (lexpr, "*", rexpr)
    | Op_div -> fprintf fmtr "%a" print_hgh_expr (lexpr, "/", rexpr)
    | Op_eq -> fprintf fmtr "%a" print_eq_expr (lexpr, rexpr)
    | Op_ne -> fprintf fmtr "%a" print_ne_expr (lexpr, rexpr)
    | Op_lt -> fprintf fmtr "%a" print_comp_expr (lexpr, "<", rexpr)
    | Op_gt -> fprintf fmtr "%a" print_comp_expr (lexpr, ">", rexpr)
    | Op_le -> fprintf fmtr "%a" print_comp_expr (lexpr, "<=", rexpr)
    | Op_ge -> fprintf fmtr "%a" print_comp_expr (lexpr, ">=", rexpr)
    | Op_and -> fprintf fmtr "%a" print_and_expr (lexpr, rexpr)
    | Op_or -> fprintf fmtr "%a" print_or_expr (lexpr, rexpr)

and print_unop fmtr (unop, expr) = match unop with
    | Op_not -> fprintf fmtr "%a" print_not_expr expr
    | Op_minus -> fprintf fmtr "%a" print_minus_expr expr

and print_low_expr fmtr = function
    | (lexpr, optr, Eparen rexpr_inside) ->
        begin
            let lexpr_strip = strip_paren lexpr 
                and rexpr_inside_strip = strip_paren rexpr_inside
            in match rexpr_inside_strip with
            | Ebinop (_, Op_mul, _) -> fprintf fmtr "%a %s %a" print_expr lexpr_strip optr print_expr rexpr_inside_strip
            | Ebinop (_, Op_div, _) -> fprintf fmtr "%a %s %a" print_expr lexpr_strip optr print_expr rexpr_inside_strip
            | Eunop (Op_minus, _) -> fprintf fmtr "%a %s %a" print_expr lexpr_strip optr print_expr rexpr_inside_strip
            | _ -> fprintf fmtr "%a %s (%a)" print_expr lexpr_strip optr print_expr rexpr_inside_strip
        end
    | (lexpr, optr, rexpr) ->
        let lexpr_strip = strip_paren lexpr
        in fprintf fmtr "%a %s %a" print_expr lexpr_strip optr print_expr rexpr

and print_hgh_expr fmtr = function
    | (Eparen lexpr_inside, optr, Eparen rexpr_inside) ->
        begin
            let lexpr_inside_strip = strip_paren lexpr_inside
                and rexpr_inside_strip = strip_paren rexpr_inside
            in match lexpr_inside_strip with
            | Ebinop (_, Op_mul, _) ->
                begin
                    match rexpr_inside_strip with
                    | Eunop (Op_minus, _) -> fprintf fmtr "%a %s %a" print_expr lexpr_inside_strip optr print_expr rexpr_inside_strip
                    | _ -> fprintf fmtr "%a %s (%a)" print_expr lexpr_inside_strip optr print_expr rexpr_inside_strip
                end
            | Ebinop (_, Op_div, _) ->
                begin
                    match rexpr_inside_strip with
                    | Eunop (Op_minus, _) -> fprintf fmtr "%a %s %a" print_expr lexpr_inside_strip optr print_expr rexpr_inside_strip
                    | _ -> fprintf fmtr "%a %s (%a)" print_expr lexpr_inside_strip optr print_expr rexpr_inside_strip
                end
            | Eunop (Op_minus, _) ->
                begin
                    match rexpr_inside_strip with
                    | Eunop (Op_minus, _) -> fprintf fmtr "%a %s %a" print_expr lexpr_inside_strip optr print_expr rexpr_inside_strip
                    | _ -> fprintf fmtr "%a %s (%a)" print_expr lexpr_inside_strip optr print_expr rexpr_inside_strip
                end
            | _ ->
                begin
                    match rexpr_inside_strip with
                    | Eunop (Op_minus, _) -> fprintf fmtr "(%a) %s %a" print_expr lexpr_inside_strip optr print_expr rexpr_inside_strip
                    | _ -> fprintf fmtr "(%a) %s (%a)" print_expr lexpr_inside_strip optr print_expr rexpr_inside_strip
                end
        end
    | (Eparen lexpr_inside, optr, rexpr) ->
        begin
            let lexpr_inside_strip = strip_paren lexpr_inside
            in match lexpr_inside_strip with
            | Ebinop (_, Op_mul, _) -> fprintf fmtr "%a %s %a" print_expr lexpr_inside_strip optr print_expr rexpr
            | Ebinop (_, Op_div, _) -> fprintf fmtr "%a %s %a" print_expr lexpr_inside_strip optr print_expr rexpr
            | Eunop (Op_minus, _) -> fprintf fmtr "%a %s %a" print_expr lexpr_inside_strip optr print_expr rexpr
            | _ -> fprintf fmtr "(%a) %s %a" print_expr lexpr_inside_strip optr print_expr rexpr
        end
    | (lexpr, optr, Eparen rexpr_inside) ->
        begin
            let rexpr_inside_strip = strip_paren rexpr_inside
            in match rexpr_inside_strip with
            | Eunop (Op_minus, _) -> fprintf fmtr "%a %s %a" print_expr lexpr optr print_expr rexpr_inside_strip
            | _ -> fprintf fmtr "%a %s (%a)" print_expr lexpr optr print_expr rexpr_inside_strip
        end
    | (lexpr, optr, rexpr) -> fprintf fmtr "%a %s %a" print_expr lexpr optr print_expr rexpr

and print_eq_expr fmtr = function
    | (Eparen lexpr_inside, Eparen rexpr_inside) ->
        begin
            let
                lexpr_inside_strip = strip_paren lexpr_inside
            and
                rexpr_inside_strip = strip_paren rexpr_inside
            in
                match lexpr_inside_strip with
                | Ebinop (_, Op_and, _) ->
                    begin
                        match rexpr_inside_strip with
                        | Ebinop (_, Op_add, _) -> fprintf fmtr "(%a) = %a" print_expr lexpr_inside_strip print_expr rexpr_inside_strip
                        | Ebinop (_, Op_sub, _) -> fprintf fmtr "(%a) = %a" print_expr lexpr_inside_strip print_expr rexpr_inside_strip
                        | Ebinop (_, Op_mul, _) -> fprintf fmtr "(%a) = %a" print_expr lexpr_inside_strip print_expr rexpr_inside_strip
                        | Ebinop (_, Op_div, _) -> fprintf fmtr "(%a) = %a" print_expr lexpr_inside_strip print_expr rexpr_inside_strip
                        | Eunop (Op_minus, _) -> fprintf fmtr "(%a) = %a" print_expr lexpr_inside_strip print_expr rexpr_inside_strip            
                        | _ -> fprintf fmtr "(%a) = (%a)" print_expr lexpr_inside_strip print_expr rexpr_inside_strip
                    end
                | Ebinop (_, Op_or, _) ->
                    begin
                        match rexpr_inside_strip with
                        | Ebinop (_, Op_add, _) -> fprintf fmtr "(%a) = %a" print_expr lexpr_inside_strip print_expr rexpr_inside_strip
                        | Ebinop (_, Op_sub, _) -> fprintf fmtr "(%a) = %a" print_expr lexpr_inside_strip print_expr rexpr_inside_strip
                        | Ebinop (_, Op_mul, _) -> fprintf fmtr "(%a) = %a" print_expr lexpr_inside_strip print_expr rexpr_inside_strip
                        | Ebinop (_, Op_div, _) -> fprintf fmtr "(%a) = %a" print_expr lexpr_inside_strip print_expr rexpr_inside_strip
                        | Eunop (Op_minus, _) -> fprintf fmtr "(%a) = %a" print_expr lexpr_inside_strip print_expr rexpr_inside_strip            
                        | _ -> fprintf fmtr "(%a) = (%a)" print_expr lexpr_inside_strip print_expr rexpr_inside_strip
                    end
                | Eunop (Op_not, _) ->
                    begin
                        match rexpr_inside_strip with
                        | Ebinop (_, Op_add, _) -> fprintf fmtr "(%a) = %a" print_expr lexpr_inside_strip print_expr rexpr_inside_strip
                        | Ebinop (_, Op_sub, _) -> fprintf fmtr "(%a) = %a" print_expr lexpr_inside_strip print_expr rexpr_inside_strip
                        | Ebinop (_, Op_mul, _) -> fprintf fmtr "(%a) = %a" print_expr lexpr_inside_strip print_expr rexpr_inside_strip
                        | Ebinop (_, Op_div, _) -> fprintf fmtr "(%a) = %a" print_expr lexpr_inside_strip print_expr rexpr_inside_strip
                        | Eunop (Op_minus, _) -> fprintf fmtr "(%a) = %a" print_expr lexpr_inside_strip print_expr rexpr_inside_strip            
                        | _ -> fprintf fmtr "(%a) = (%a)" print_expr lexpr_inside_strip print_expr rexpr_inside_strip
                    end
                | _ ->
                    begin
                        match rexpr_inside_strip with
                        | Ebinop (_, Op_add, _) -> fprintf fmtr "%a = %a" print_expr lexpr_inside_strip print_expr rexpr_inside_strip
                        | Ebinop (_, Op_sub, _) -> fprintf fmtr "%a = %a" print_expr lexpr_inside_strip print_expr rexpr_inside_strip
                        | Ebinop (_, Op_mul, _) -> fprintf fmtr "%a = %a" print_expr lexpr_inside_strip print_expr rexpr_inside_strip
                        | Ebinop (_, Op_div, _) -> fprintf fmtr "%a = %a" print_expr lexpr_inside_strip print_expr rexpr_inside_strip
                        | Eunop (Op_minus, _) -> fprintf fmtr "%a = %a" print_expr lexpr_inside_strip print_expr rexpr_inside_strip            
                        | _ -> fprintf fmtr "%a = (%a)" print_expr lexpr_inside_strip print_expr rexpr_inside_strip
                    end
        end
    | (Eparen lexpr_inside, rexpr) ->
        begin
            let
                lexpr_inside_strip = strip_paren lexpr_inside
            in
                match lexpr_inside_strip with
                | Ebinop (_, Op_or, _) -> fprintf fmtr "(%a) = %a" print_expr lexpr_inside_strip print_expr rexpr
                | Ebinop (_, Op_and, _) -> fprintf fmtr "(%a) = %a" print_expr lexpr_inside_strip print_expr rexpr
                | Eunop (Op_not, _) -> fprintf fmtr "(%a) = %a" print_expr lexpr_inside_strip print_expr rexpr
                | _ -> fprintf fmtr "%a = %a" print_expr lexpr_inside_strip print_expr rexpr
        end
    | (lexpr, Eparen rexpr_inside) ->
        begin
            let
                rexpr_inside_strip = strip_paren rexpr_inside
            in
                match rexpr_inside_strip with
                | Ebinop (_, Op_add, _) -> fprintf fmtr "%a = %a" print_expr lexpr print_expr rexpr_inside_strip
                | Ebinop (_, Op_sub, _) -> fprintf fmtr "%a = %a" print_expr lexpr print_expr rexpr_inside_strip
                | Ebinop (_, Op_mul, _) -> fprintf fmtr "%a = %a" print_expr lexpr print_expr rexpr_inside_strip
                | Ebinop (_, Op_div, _) -> fprintf fmtr "%a = %a" print_expr lexpr print_expr rexpr_inside_strip
                | Eunop (Op_minus, _) -> fprintf fmtr "%a = %a" print_expr lexpr print_expr rexpr_inside_strip            
                | _ -> fprintf fmtr "%a = (%a)" print_expr lexpr print_expr rexpr_inside_strip
        end
    | (lexpr, rexpr) -> fprintf fmtr "%a = %a" print_expr lexpr print_expr rexpr

and print_ne_expr fmtr = function
    | (Eparen lexpr_inside, Eparen rexpr_inside) ->
        begin
            let
                lexpr_inside_strip = strip_paren lexpr_inside
            and
                rexpr_inside_strip = strip_paren rexpr_inside
            in
                match lexpr_inside_strip with
                | Ebinop (_, Op_and, _) ->
                    begin
                        match rexpr_inside_strip with
                        | Ebinop (_, Op_add, _) -> fprintf fmtr "(%a) != %a" print_expr lexpr_inside_strip print_expr rexpr_inside_strip
                        | Ebinop (_, Op_sub, _) -> fprintf fmtr "(%a) != %a" print_expr lexpr_inside_strip print_expr rexpr_inside_strip
                        | Ebinop (_, Op_mul, _) -> fprintf fmtr "(%a) != %a" print_expr lexpr_inside_strip print_expr rexpr_inside_strip
                        | Ebinop (_, Op_div, _) -> fprintf fmtr "(%a) != %a" print_expr lexpr_inside_strip print_expr rexpr_inside_strip
                        | Eunop (Op_minus, _) -> fprintf fmtr "(%a) != %a" print_expr lexpr_inside_strip print_expr rexpr_inside_strip            
                        | _ -> fprintf fmtr "(%a) != (%a)" print_expr lexpr_inside_strip print_expr rexpr_inside_strip
                    end
                | Ebinop (_, Op_or, _) ->
                    begin
                        match rexpr_inside_strip with
                        | Ebinop (_, Op_add, _) -> fprintf fmtr "(%a) != %a" print_expr lexpr_inside_strip print_expr rexpr_inside_strip
                        | Ebinop (_, Op_sub, _) -> fprintf fmtr "(%a) != %a" print_expr lexpr_inside_strip print_expr rexpr_inside_strip
                        | Ebinop (_, Op_mul, _) -> fprintf fmtr "(%a) != %a" print_expr lexpr_inside_strip print_expr rexpr_inside_strip
                        | Ebinop (_, Op_div, _) -> fprintf fmtr "(%a) != %a" print_expr lexpr_inside_strip print_expr rexpr_inside_strip
                        | Eunop (Op_minus, _) -> fprintf fmtr "(%a) != %a" print_expr lexpr_inside_strip print_expr rexpr_inside_strip            
                        | _ -> fprintf fmtr "(%a) != (%a)" print_expr lexpr_inside_strip print_expr rexpr_inside_strip
                    end
                | Eunop (Op_not, _) ->
                    begin
                        match rexpr_inside_strip with
                        | Ebinop (_, Op_add, _) -> fprintf fmtr "(%a) != %a" print_expr lexpr_inside_strip print_expr rexpr_inside_strip
                        | Ebinop (_, Op_sub, _) -> fprintf fmtr "(%a) != %a" print_expr lexpr_inside_strip print_expr rexpr_inside_strip
                        | Ebinop (_, Op_mul, _) -> fprintf fmtr "(%a) != %a" print_expr lexpr_inside_strip print_expr rexpr_inside_strip
                        | Ebinop (_, Op_div, _) -> fprintf fmtr "(%a) != %a" print_expr lexpr_inside_strip print_expr rexpr_inside_strip
                        | Eunop (Op_minus, _) -> fprintf fmtr "(%a) != %a" print_expr lexpr_inside_strip print_expr rexpr_inside_strip            
                        | _ -> fprintf fmtr "(%a) != (%a)" print_expr lexpr_inside_strip print_expr rexpr_inside_strip
                    end
                | _ ->
                    begin
                        match rexpr_inside_strip with
                        | Ebinop (_, Op_add, _) -> fprintf fmtr "%a != %a" print_expr lexpr_inside_strip print_expr rexpr_inside_strip
                        | Ebinop (_, Op_sub, _) -> fprintf fmtr "%a != %a" print_expr lexpr_inside_strip print_expr rexpr_inside_strip
                        | Ebinop (_, Op_mul, _) -> fprintf fmtr "%a != %a" print_expr lexpr_inside_strip print_expr rexpr_inside_strip
                        | Ebinop (_, Op_div, _) -> fprintf fmtr "%a != %a" print_expr lexpr_inside_strip print_expr rexpr_inside_strip
                        | Eunop (Op_minus, _) -> fprintf fmtr "%a != %a" print_expr lexpr_inside_strip print_expr rexpr_inside_strip            
                        | _ -> fprintf fmtr "%a != (%a)" print_expr lexpr_inside_strip print_expr rexpr_inside_strip
                    end
        end
    | (Eparen lexpr_inside, rexpr) ->
        begin
            let lexpr_inside_strip = strip_paren lexpr_inside
            in match lexpr_inside_strip with
            | Ebinop (_, Op_or, _) -> fprintf fmtr "(%a) != %a" print_expr lexpr_inside_strip print_expr rexpr
            | Ebinop (_, Op_and, _) -> fprintf fmtr "(%a) != %a" print_expr lexpr_inside_strip print_expr rexpr
            | Eunop (Op_not, _) -> fprintf fmtr "(%a) != %a" print_expr lexpr_inside_strip print_expr rexpr
            | _ -> fprintf fmtr "%a != %a" print_expr lexpr_inside_strip print_expr rexpr
        end
    | (lexpr, Eparen rexpr_inside) ->
        begin
            let rexpr_inside_strip = strip_paren rexpr_inside
            in match rexpr_inside_strip with
            | Ebinop (_, Op_add, _) -> fprintf fmtr "%a != %a" print_expr lexpr print_expr rexpr_inside_strip
            | Ebinop (_, Op_sub, _) -> fprintf fmtr "%a != %a" print_expr lexpr print_expr rexpr_inside_strip
            | Ebinop (_, Op_mul, _) -> fprintf fmtr "%a != %a" print_expr lexpr print_expr rexpr_inside_strip
            | Ebinop (_, Op_div, _) -> fprintf fmtr "%a != %a" print_expr lexpr print_expr rexpr_inside_strip
            | Eunop (Op_minus, _) -> fprintf fmtr "%a != %a" print_expr lexpr print_expr rexpr_inside_strip            
            | _ -> fprintf fmtr "%a != (%a)" print_expr lexpr print_expr rexpr_inside_strip
        end
    | (lexpr, rexpr) -> fprintf fmtr "%a != %a" print_expr lexpr print_expr rexpr

and print_comp_expr fmtr (lexpr, comp, rexpr) =
    let lexpr_strip = strip_paren lexpr and rexpr_strip = strip_paren rexpr
    in fprintf fmtr "%a %s %a" print_expr lexpr_strip comp print_expr rexpr_strip

and print_and_expr fmtr = function
    | (Eparen lexpr_inside, Eparen rexpr_inside) ->
        begin
            let lexpr_inside_strip = strip_paren lexpr_inside
                and rexpr_inside_strip = strip_paren rexpr_inside
            in match lexpr_inside_strip with
            | Ebinop (_, Op_or, _) ->
                begin
                    match rexpr_inside_strip with
                    | Ebinop (_, Op_and, _) -> fprintf fmtr "(%a) and (%a)" print_expr lexpr_inside_strip print_expr rexpr_inside_strip
                    | Ebinop (_, Op_or, _) -> fprintf fmtr "(%a) and (%a)" print_expr lexpr_inside_strip print_expr rexpr_inside_strip
                    | _ -> fprintf fmtr "(%a) and %a" print_expr lexpr_inside_strip print_expr rexpr_inside_strip
                end
            | _ ->
                begin
                    match rexpr_inside_strip with
                    | Ebinop (_, Op_and, _) -> fprintf fmtr "%a and (%a)" print_expr lexpr_inside_strip print_expr rexpr_inside_strip
                    | Ebinop (_, Op_or, _) -> fprintf fmtr "%a and (%a)" print_expr lexpr_inside_strip print_expr rexpr_inside_strip
                    | _ -> fprintf fmtr "%a and %a" print_expr lexpr_inside_strip print_expr rexpr_inside_strip
                end
        end
    | (Eparen lexpr_inside, rexpr) ->
        begin
            let lexpr_inside_strip = strip_paren lexpr_inside
            in match lexpr_inside_strip with
            | Ebinop (_, Op_or, _) -> fprintf fmtr "(%a) and %a" print_expr lexpr_inside_strip print_expr rexpr
            | _ -> fprintf fmtr "%a and %a" print_expr lexpr_inside_strip print_expr rexpr
        end
    | (lexpr, Eparen rexpr_inside) ->
        begin
            let rexpr_inside_strip = strip_paren rexpr_inside
            in match rexpr_inside_strip with
            | Ebinop (_, Op_and, _) -> fprintf fmtr "%a and (%a)" print_expr lexpr print_expr rexpr_inside_strip
            | Ebinop (_, Op_or, _) -> fprintf fmtr "%a and (%a)" print_expr lexpr print_expr rexpr_inside_strip
            | _ -> fprintf fmtr "%a and %a" print_expr lexpr print_expr rexpr_inside_strip
        end
    | (lexpr, rexpr) -> fprintf fmtr "%a and %a" print_expr lexpr print_expr rexpr

and print_or_expr fmtr = function
    | (lexpr, Eparen rexpr_inside) ->
        begin
            let lexpr_strip = strip_paren lexpr
            and rexpr_inside_strip = strip_paren rexpr_inside
            in match rexpr_inside_strip with
            | Ebinop (_, Op_or, _) -> fprintf fmtr "%a or (%a)" print_expr lexpr_strip print_expr rexpr_inside_strip
            | _ -> fprintf fmtr "%a or %a" print_expr lexpr_strip print_expr rexpr_inside_strip
        end
    | (lexpr, rexpr) -> 
        let lexpr_strip = strip_paren lexpr
        in fprintf fmtr "%a or %a" print_expr lexpr_strip print_expr rexpr

and print_not_expr fmtr = function
    | Eparen expr_inside ->
        begin
            let expr_inside_strip = strip_paren expr_inside
            in match expr_inside_strip with
            | Ebinop (_, Op_and, _) -> fprintf fmtr "%s (%a)" "not" print_expr expr_inside_strip
            | Ebinop (_, Op_or, _) -> fprintf fmtr "%s (%a)" "not" print_expr expr_inside_strip
            | _ -> fprintf fmtr "%s %a" "not" print_expr expr_inside_strip
        end
    | expr -> fprintf fmtr "%s %a" "not" print_expr expr

and print_minus_expr fmtr = function
    | Eparen expr_inside ->
        begin
            let expr_inside_strip = strip_paren expr_inside
            in match expr_inside_strip with
            | Eunop (Op_minus, _) -> fprintf fmtr "%s %a" "-" print_expr expr_inside_strip
            | _ -> fprintf fmtr "%s (%a)" "-" print_expr expr_inside_strip
        end
    | expr -> fprintf fmtr "%s %a" "-" print_expr expr

and strip_paren expr = match expr with
    | Eparen paren_expr -> strip_paren paren_expr
    | _ -> expr
