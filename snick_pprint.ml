(*
** File:          snick_pprint.ml
** Description:   Pretty-printer converts from snick source 
**                code to well-formed style.
** Last Modified: Sun. 9th April 2017
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
    fprintf fmtr "@[<v>proc %a@;<0 4>@[<v>%a@]@,end@]@."
                print_proc_header header print_proc_body body

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
    | Write expr -> 
        begin
            match expr with
            | Expr wexpr -> fprintf fmtr "write %a;" print_expr wexpr
            | String str -> fprintf fmtr "write %s;" str
        end
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

(* Print element to be assigned to or be read / written. *)
and print_elem fmtr = function
    | Single_elem ident -> fprintf fmtr "%s" ident
    | Array_elem (ident, idxs) -> fprintf fmtr "%s[%a]" ident print_exprs idxs

(* Print an expression. *)
and print_expr fmtr = function
    | Eelem elem -> fprintf fmtr "%a" print_elem elem
    | Ebool bool_const -> fprintf fmtr "%B" bool_const
    | Eint int_const -> fprintf fmtr "%d" int_const
    | Efloat float_const -> fprintf fmtr "%f" float_const
    (* Parentheses to be printed (or removed) in other functions,
       so only print the expression within the parenthesis.
    *)
    | Eparen expr -> fprintf fmtr "%a" print_expr (strip_paren expr)
    | Ebinop bin_expr -> fprintf fmtr "%a" print_binop bin_expr
    | Eunop un_expr -> fprintf fmtr "%a" print_unop un_expr

(* Print expressions. *)
and print_exprs fmtr = function
    | [] -> ()
    | x::[] -> fprintf fmtr "%a" print_expr x
    | x::xs -> fprintf fmtr "%a, %a" print_expr x print_exprs xs

(* Print binary operations. 
** Each operation will be printed differently.
*)
and print_binop fmtr = function
    | (Eparen lexpr_inside, optr, Eparen rexpr_inside) ->
        begin
            let
                lexpr_inside_strip = strip_paren lexpr_inside
            and
                rexpr_inside_strip = strip_paren rexpr_inside
            in
                let
                    lcmpr_result = cmpr_prec lexpr_inside_strip optr
                and 
                    rcmpr_result = cmpr_prec rexpr_inside_strip optr
                in
                    if lcmpr_result>=0 && rcmpr_result>0 then
                        fprintf fmtr "%a %a %a"
                            print_expr lexpr_inside_strip print_binoptr optr print_expr rexpr_inside_strip
                    else if lcmpr_result>=0 && rcmpr_result<=0 then 
                        fprintf fmtr "%a %a (%a)"
                            print_expr lexpr_inside_strip print_binoptr optr print_expr rexpr_inside_strip
                    else if lcmpr_result<0 && rcmpr_result>0 then
                        fprintf fmtr "(%a) %a %a"
                            print_expr lexpr_inside_strip print_binoptr optr print_expr rexpr_inside_strip
                    else
                        fprintf fmtr "(%a) %a (%a)"
                            print_expr lexpr_inside_strip print_binoptr optr print_expr rexpr_inside_strip
        end
    | (Eparen lexpr_inside, optr, rexpr) ->
        begin
            let
                lexpr_inside_strip = strip_paren lexpr_inside
            in
                let 
                    lcmpr_result = cmpr_prec lexpr_inside_strip optr
                in
                    if lcmpr_result>=0 then
                        fprintf fmtr "%a %a %a"
                            print_expr lexpr_inside_strip print_binoptr optr print_expr rexpr
                    else
                        fprintf fmtr "(%a) %a %a"
                            print_expr lexpr_inside_strip print_binoptr optr print_expr rexpr
        end
    | (lexpr, optr, Eparen rexpr_inside) ->
        begin
            let
                rexpr_inside_strip = strip_paren rexpr_inside
            in
                let 
                    rcmpr_result = cmpr_prec rexpr_inside_strip optr
                in
                    if rcmpr_result>0 then
                        fprintf fmtr "%a %a %a"
                            print_expr lexpr print_binoptr optr print_expr rexpr_inside_strip
                    else
                        fprintf fmtr "%a %a (%a)"
                            print_expr lexpr print_binoptr optr print_expr rexpr_inside_strip
        end
    | (lexpr, optr, rexpr) ->
        begin
            fprintf fmtr "%a %a %a"
                print_expr lexpr print_binoptr optr print_expr rexpr 
        end

and print_unop fmtr = function
    | (optr, Eparen expr_inside) ->
        begin
            let
                expr_inside_strip = strip_paren expr_inside
            in
                let 
                    cmpr_result = cmpr_prec expr_inside_strip optr
                in
                    if cmpr_result<0 then
                        fprintf fmtr "%a (%a)"
                            print_unoptr optr print_expr expr_inside_strip 
                    else
                        fprintf fmtr "%a %a"
                            print_unoptr optr print_expr expr_inside_strip 
        end
    | (optr, expr) ->
        begin
            fprintf fmtr "%a %a %a"
                print_optr optr print_expr expr 
        end

and cmpr_prec exp optr = match exp with
    | (_, exp_binoptr, _) -> (get_prec exp_binoptr) - (get_prec optr)
    | (exp_unoptr, _) -> (get_prec exp_unoptr) - (get_prec optr)

and get_prec optr = match optr with
    | Op_minus -> 7
    | Op_mul | Op_div -> 6
    | Op_add | Op_sub -> 5
    | Op_eq | Op_ne | Op_lt | Op_gt | Op_le | Op_ge -> 4
    | Op_not -> 3
    | Op_and -> 2
    | Op_or -> 1

and print_binoptr optr = match optr with
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

and print_unoptr optr = match optr with
    | Op_not -> fprintf fmtr "%s" "not"
    | Op_minus -> fprintf fmtr "%s" "-"

(* Print unary operators (not and unary minus). 
** Each operation will be printed differently.
*)
(* and print_unop fmtr (unop, expr) = match unop with
    | Op_not -> fprintf fmtr "%a" print_not_expr expr
    | Op_minus -> fprintf fmtr "%a" print_minus_expr expr
 *)
(* Following functions prints out expressions involving various operators.
** Expressions are evaluated (according to precedence rules) 
** case by case to decide whether or not to print out parentheses.
** Constants are considered higher precedence than any operators.
*)

(* Add operation, removes any parentheses around expressions of 
** higher precedence on the RHS, and remove all parentheses on the left,
** since add has lowest precedence among arithmetic operators.
*)
and print_add_expr fmtr = function
    | (lexpr, Eparen rexpr_inside) ->
        begin
            let
                lexpr_strip = strip_paren lexpr 
            and
                rexpr_inside_strip = strip_paren rexpr_inside
            in
                match rexpr_inside_strip with
                | Eelem _  | Eint _ | Efloat _
                | Ebinop (_, Op_mul, _) | Ebinop (_, Op_div, _) | Eunop (Op_minus, _)
                    -> fprintf fmtr "%a + %a"
                            print_expr lexpr_strip print_expr rexpr_inside_strip
                | _ -> fprintf fmtr "%a + (%a)"
                            print_expr lexpr_strip print_expr rexpr_inside_strip
        end
    | (lexpr, rexpr) ->
        let
            lexpr_strip = strip_paren lexpr
        in
            fprintf fmtr "%a + %a" print_expr lexpr_strip print_expr rexpr

(* Subtract operation, removes any parentheses around expressions of 
** higher precedence on the RHS, and remove all parentheses on the left,
** since subtract has lowest precedence among arithmetic operators.
*)
and print_sub_expr fmtr = function
    | (lexpr, Eparen rexpr_inside) ->
        begin
            let
                lexpr_strip = strip_paren lexpr 
            and
                rexpr_inside_strip = strip_paren rexpr_inside
            in
                match rexpr_inside_strip with
                | Eelem _ | Eint _ | Efloat _
                | Ebinop (_, Op_mul, _) | Ebinop (_, Op_div, _) | Eunop (Op_minus, _)
                    -> fprintf fmtr "%a - %a"
                                print_expr lexpr_strip print_expr rexpr_inside_strip
                | _ -> fprintf fmtr "%a - (%a)"
                                print_expr lexpr_strip print_expr rexpr_inside_strip
        end
    | (lexpr, rexpr) ->
        let
            lexpr_strip = strip_paren lexpr
        in
            fprintf fmtr "%a - %a" print_expr lexpr_strip print_expr rexpr

(* Multiply operation, removes any parentheses around expressions of 
** higher precedence on the RHS, and higher or equal precedence on LHS.
*)
and print_mul_expr fmtr = function
    | (Eparen lexpr_inside, Eparen rexpr_inside) ->
        begin
            let
                lexpr_inside_strip = strip_paren lexpr_inside
            and
                rexpr_inside_strip = strip_paren rexpr_inside
            in
                match lexpr_inside_strip with
                | Eelem _ | Eint _ | Efloat _
                | Ebinop (_, Op_mul, _) | Ebinop (_, Op_div, _) | Eunop (Op_minus, _)
                    ->
                    begin
                        match rexpr_inside_strip with
                        | Eelem _ | Eint _ | Efloat _
                        | Eunop (Op_minus, _) -> fprintf fmtr "%a * %a"
                                        print_expr lexpr_inside_strip print_expr rexpr_inside_strip            
                        | _ -> fprintf fmtr "%a * (%a)"
                                        print_expr lexpr_inside_strip print_expr rexpr_inside_strip
                    end
                | _ ->
                    begin
                        match rexpr_inside_strip with
                        | Eelem _ | Eint _ | Efloat _
                        | Eunop (Op_minus, _) -> fprintf fmtr "(%a) * %a"
                                    print_expr lexpr_inside_strip print_expr rexpr_inside_strip            
                        | _ -> fprintf fmtr "(%a) * (%a)"
                                    print_expr lexpr_inside_strip print_expr rexpr_inside_strip
                    end
        end
    | (Eparen lexpr_inside, rexpr) ->
        begin
            let
                lexpr_inside_strip = strip_paren lexpr_inside
            in
                match lexpr_inside_strip with
                | Eelem _ | Eint _ | Efloat _
                | Ebinop (_, Op_mul, _) | Ebinop (_, Op_div, _) | Eunop (Op_minus, _)
                    -> fprintf fmtr "%a * %a"
                            print_expr lexpr_inside_strip print_expr rexpr
                | _ -> fprintf fmtr "(%a) * %a"
                            print_expr lexpr_inside_strip print_expr rexpr
        end
    | (lexpr, Eparen rexpr_inside) ->
        begin
            let
                rexpr_inside_strip = strip_paren rexpr_inside
            in
                match rexpr_inside_strip with
                | Eelem _ | Eint _ | Efloat _
                | Eunop (Op_minus, _)
                    -> fprintf fmtr "%a * %a"
                            print_expr lexpr print_expr rexpr_inside_strip            
                | _ -> fprintf fmtr "%a * (%a)"
                            print_expr lexpr print_expr rexpr_inside_strip
        end
    | (lexpr, rexpr) -> fprintf fmtr "%a * %a" print_expr lexpr print_expr rexpr

(* Divide operation, removes any parentheses around expressions of 
** higher precedence on the RHS, and higher or equal precedence on LHS.
*)
and print_div_expr fmtr = function
    | (Eparen lexpr_inside, Eparen rexpr_inside) ->
        begin
            let
                lexpr_inside_strip = strip_paren lexpr_inside
            and
                rexpr_inside_strip = strip_paren rexpr_inside
            in
                match lexpr_inside_strip with
                | Eelem _ | Eint _ | Efloat _
                | Ebinop (_, Op_mul, _) | Ebinop (_, Op_div, _) | Eunop (Op_minus, _)
                    ->
                    begin
                        match rexpr_inside_strip with
                        | Eelem _ | Eint _ | Efloat _
                        | Eunop (Op_minus, _)
                            -> fprintf fmtr "%a / %a"
                                    print_expr lexpr_inside_strip print_expr rexpr_inside_strip            
                        | _ -> fprintf fmtr "%a / (%a)"
                                    print_expr lexpr_inside_strip print_expr rexpr_inside_strip
                    end
                | _ ->
                    begin
                        match rexpr_inside_strip with
                        | Eelem _ | Eint _ | Efloat _
                        | Eunop (Op_minus, _)
                            -> fprintf fmtr "(%a) / %a"
                                    print_expr lexpr_inside_strip print_expr rexpr_inside_strip            
                        | _ -> fprintf fmtr "(%a) / (%a)"
                                    print_expr lexpr_inside_strip print_expr rexpr_inside_strip
                    end
        end
    | (Eparen lexpr_inside, rexpr) ->
        begin
            let
                lexpr_inside_strip = strip_paren lexpr_inside
            in
                match lexpr_inside_strip with
                | Eelem _ | Eint _ | Efloat _
                | Ebinop (_, Op_mul, _) | Ebinop (_, Op_div, _) | Eunop (Op_minus, _)
                    -> fprintf fmtr "%a / %a"
                            print_expr lexpr_inside_strip print_expr rexpr
                | _ -> fprintf fmtr "(%a) / %a"
                            print_expr lexpr_inside_strip print_expr rexpr
        end
    | (lexpr, Eparen rexpr_inside) ->
        begin
            let
                rexpr_inside_strip = strip_paren rexpr_inside
            in
                match rexpr_inside_strip with
                | Eelem _ | Eint _ | Efloat _
                | Eunop (Op_minus, _)
                    -> fprintf fmtr "%a / %a"
                                print_expr lexpr print_expr rexpr_inside_strip            
                | _ -> fprintf fmtr "%a / (%a)"
                                print_expr lexpr print_expr rexpr_inside_strip
        end
    | (lexpr, rexpr) -> fprintf fmtr "%a / %a" print_expr lexpr print_expr rexpr

(* Equal operation, removes any parentheses around expressions of 
** higher precedence on the RHS, and higher or equal precedence on LHS.
*)
and print_eq_expr fmtr = function
    | (Eparen lexpr_inside, Eparen rexpr_inside) ->
        begin
            let
                lexpr_inside_strip = strip_paren lexpr_inside
            and
                rexpr_inside_strip = strip_paren rexpr_inside
            in
                match lexpr_inside_strip with
                | Ebinop (_, Op_and, _) | Ebinop (_, Op_or, _) | Eunop (Op_not, _)
                    ->
                    begin
                        match rexpr_inside_strip with
                        | Eelem _ | Eint _ | Efloat _
                        | Ebinop (_, Op_add, _) | Ebinop (_, Op_sub, _)
                        | Ebinop (_, Op_mul, _) | Ebinop (_, Op_div, _)
                        | Eunop (Op_minus, _)
                            -> fprintf fmtr "(%a) = %a"
                                        print_expr lexpr_inside_strip print_expr rexpr_inside_strip            
                        | _ -> fprintf fmtr "(%a) = (%a)"
                                        print_expr lexpr_inside_strip print_expr rexpr_inside_strip
                    end
                | _ ->
                    begin
                        match rexpr_inside_strip with
                        | Eelem _ | Ebool _ | Eint _ | Efloat _
                        | Ebinop (_, Op_add, _) | Ebinop (_, Op_sub, _)
                        | Ebinop (_, Op_mul, _) | Ebinop (_, Op_div, _)
                        | Eunop (Op_minus, _)
                            -> fprintf fmtr "%a = %a"
                                        print_expr lexpr_inside_strip print_expr rexpr_inside_strip            
                        | _ -> fprintf fmtr "%a = (%a)"
                                        print_expr lexpr_inside_strip print_expr rexpr_inside_strip
                    end
        end
    | (Eparen lexpr_inside, rexpr) ->
        begin
            let
                lexpr_inside_strip = strip_paren lexpr_inside
            in
                match lexpr_inside_strip with
                | Ebinop (_, Op_or, _) | Ebinop (_, Op_and, _) | Eunop (Op_not, _)
                    -> fprintf fmtr "(%a) = %a"
                                print_expr lexpr_inside_strip print_expr rexpr
                | _ -> fprintf fmtr "%a = %a"
                                print_expr lexpr_inside_strip print_expr rexpr
        end
    | (lexpr, Eparen rexpr_inside) ->
        begin
            let
                rexpr_inside_strip = strip_paren rexpr_inside
            in
                match rexpr_inside_strip with
                | Eelem _ | Ebool _ | Eint _ | Efloat _
                | Ebinop (_, Op_add, _) | Ebinop (_, Op_sub, _)
                | Ebinop (_, Op_mul, _) | Ebinop (_, Op_div, _)
                | Eunop (Op_minus, _)
                    -> fprintf fmtr "%a = %a"
                                print_expr lexpr print_expr rexpr_inside_strip            
                | _ -> fprintf fmtr "%a = (%a)"
                                print_expr lexpr print_expr rexpr_inside_strip
        end
    | (lexpr, rexpr) -> fprintf fmtr "%a = %a" print_expr lexpr print_expr rexpr

(* Not Equal operation, removes any parentheses around expressions of 
** higher precedence on the RHS, and higher or equal precedence on LHS.
*)
and print_ne_expr fmtr = function
    | (Eparen lexpr_inside, Eparen rexpr_inside) ->
        begin
            let
                lexpr_inside_strip = strip_paren lexpr_inside
            and
                rexpr_inside_strip = strip_paren rexpr_inside
            in
                match lexpr_inside_strip with
                | Ebinop (_, Op_and, _) | Ebinop (_, Op_or, _) | Eunop (Op_not, _)
                    ->
                    begin
                        match rexpr_inside_strip with
                        | Eelem _ | Ebool _ | Eint _ | Efloat _
                        | Ebinop (_, Op_add, _) | Ebinop (_, Op_sub, _)
                        | Ebinop (_, Op_mul, _) | Ebinop (_, Op_div, _)
                        | Eunop (Op_minus, _)
                            -> fprintf fmtr "(%a) != %a"
                                    print_expr lexpr_inside_strip print_expr rexpr_inside_strip            
                        | _ -> fprintf fmtr "(%a) != (%a)"
                                    print_expr lexpr_inside_strip print_expr rexpr_inside_strip
                    end
                | _ ->
                    begin
                        match rexpr_inside_strip with
                        | Eelem _ | Ebool _ | Eint _ | Efloat _
                        | Ebinop (_, Op_add, _) | Ebinop (_, Op_sub, _)
                        | Ebinop (_, Op_mul, _) | Ebinop (_, Op_div, _)
                        | Eunop (Op_minus, _)
                            -> fprintf fmtr "%a != %a"
                                    print_expr lexpr_inside_strip print_expr rexpr_inside_strip            
                        | _ -> fprintf fmtr "%a != (%a)"
                                    print_expr lexpr_inside_strip print_expr rexpr_inside_strip
                    end
        end
    | (Eparen lexpr_inside, rexpr) ->
        begin
            let
                lexpr_inside_strip = strip_paren lexpr_inside
            in
                match lexpr_inside_strip with
                | Ebinop (_, Op_or, _)
                | Ebinop (_, Op_and, _)
                | Eunop (Op_not, _) -> fprintf fmtr "(%a) != %a" 
                    print_expr lexpr_inside_strip print_expr rexpr
                | _ -> fprintf fmtr "%a != %a" 
                    print_expr lexpr_inside_strip print_expr rexpr
        end
    | (lexpr, Eparen rexpr_inside) ->
        begin
            let
                rexpr_inside_strip = strip_paren rexpr_inside
            in
                match rexpr_inside_strip with
                | Eelem _ | Ebool _ | Eint _ | Efloat _
                | Ebinop (_, Op_add, _) | Ebinop (_, Op_sub, _)
                | Ebinop (_, Op_mul, _) | Ebinop (_, Op_div, _)
                | Eunop (Op_minus, _)
                    -> fprintf fmtr "%a != %a" print_expr lexpr print_expr rexpr_inside_strip            
                | _ -> fprintf fmtr "%a != (%a)" print_expr lexpr print_expr rexpr_inside_strip
        end
    | (lexpr, rexpr) -> fprintf fmtr "%a != %a" print_expr lexpr print_expr rexpr

(* Comparision operations (>, <, >=, <=), assuming these only compares numeric values,
** remove all parentheses since these operators are of lower precedence than any
** arithmetic operations.
*)
and print_comp_expr fmtr (lexpr, optr, rexpr) = 
        let
            lexpr_strip = strip_paren lexpr
        and
            rexpr_strip = strip_paren rexpr
        in
            fprintf fmtr "%a %s %a" print_expr lexpr_strip optr print_expr rexpr_strip

(* And operation, removes any parentheses around expressions of 
** higher precedence on the RHS, and higher or equal precedence on LHS.
*)
and print_and_expr fmtr = function
    | (Eparen lexpr_inside, Eparen rexpr_inside) ->
        begin
            let
               lexpr_inside_strip = strip_paren lexpr_inside
            and
                rexpr_inside_strip = strip_paren rexpr_inside
            in
                match lexpr_inside_strip with
                | Ebinop (_, Op_or, _) ->
                    begin
                        match rexpr_inside_strip with
                        | Ebinop (_, Op_and, _) | Ebinop (_, Op_or, _)
                            -> fprintf fmtr "(%a) and (%a)"
                                        print_expr lexpr_inside_strip print_expr rexpr_inside_strip
                        | _ -> fprintf fmtr "(%a) and %a"
                                        print_expr lexpr_inside_strip print_expr rexpr_inside_strip
                    end
                | _ ->
                    begin
                        match rexpr_inside_strip with
                        | Ebinop (_, Op_and, _) | Ebinop (_, Op_or, _)
                            -> fprintf fmtr "%a and (%a)"
                                        print_expr lexpr_inside_strip print_expr rexpr_inside_strip
                        | _ -> fprintf fmtr "%a and %a"
                                        print_expr lexpr_inside_strip print_expr rexpr_inside_strip
                    end
        end
    | (Eparen lexpr_inside, rexpr) ->
        begin
            let
               lexpr_inside_strip = strip_paren lexpr_inside
            in
                match lexpr_inside_strip with
                | Ebinop (_, Op_or, _) -> fprintf fmtr "(%a) and %a"
                                print_expr lexpr_inside_strip print_expr rexpr
                | _ -> fprintf fmtr "%a and %a"
                                print_expr lexpr_inside_strip print_expr rexpr
        end
    | (lexpr, Eparen rexpr_inside) ->
        begin
            let
                rexpr_inside_strip = strip_paren rexpr_inside
            in
                match rexpr_inside_strip with
                | Ebinop (_, Op_and, _) | Ebinop (_, Op_or, _)
                    -> fprintf fmtr "%a and (%a)" print_expr lexpr print_expr rexpr_inside_strip
                | _ -> fprintf fmtr "%a and %a" print_expr lexpr print_expr rexpr_inside_strip
        end
    | (lexpr, rexpr) -> fprintf fmtr "%a and %a" print_expr lexpr print_expr rexpr

(* Or operation, removes any parentheses around expressions of 
** higher precedence on the RHS, and higher or equal precedence on LHS.
*)
and print_or_expr fmtr = function
    | (lexpr, Eparen rexpr_inside) ->
        begin
            let
                lexpr_strip = strip_paren lexpr
            and
                rexpr_inside_strip = strip_paren rexpr_inside
            in
                match rexpr_inside_strip with
                | Ebinop (_, Op_or, _) -> fprintf fmtr "%a or (%a)"
                                print_expr lexpr_strip print_expr rexpr_inside_strip
                | _ -> fprintf fmtr "%a or %a"
                                print_expr lexpr_strip print_expr rexpr_inside_strip
        end
    | (lexpr, rexpr) -> 
        let
            lexpr_strip = strip_paren lexpr
        in
            fprintf fmtr "%a or %a" print_expr lexpr_strip print_expr rexpr

(* Not operation, removes any parentheses around expressions of 
** higher precedence.
*)
and print_not_expr fmtr = function
    | Eparen expr_inside ->
        begin
            let
                expr_inside_strip = strip_paren expr_inside
            in
                match expr_inside_strip with
                | Ebinop (_, Op_and, _) | Ebinop (_, Op_or, _)
                    -> fprintf fmtr "%s (%a)" "not"
                                print_expr expr_inside_strip
                | _ -> fprintf fmtr "%s %a" "not"
                                print_expr expr_inside_strip
        end
    | expr -> fprintf fmtr "%s %a" "not" print_expr expr

(* Unary-minus operation, removes any parentheses around expressions of 
** higher precedence.
*)
and print_minus_expr fmtr = function
    | Eparen expr_inside ->
        begin
            let
                expr_inside_strip = strip_paren expr_inside
            in
                match expr_inside_strip with
                | Eelem _  | Eint _ | Efloat _
                | Eunop (Op_minus, _)
                    -> fprintf fmtr "%s %a" "-"
                            print_expr expr_inside_strip
                | _ -> fprintf fmtr "%s (%a)" "-"
                            print_expr expr_inside_strip
        end
    | expr -> fprintf fmtr "%s %a" "-" print_expr expr

(* Strips parentheses in case there are multiple pairs of parentheses
** around an expression.
*)
and strip_paren expr = match expr with
    | Eparen paren_expr -> strip_paren paren_expr
    | _ -> expr
