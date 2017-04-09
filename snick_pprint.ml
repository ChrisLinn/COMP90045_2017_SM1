(* pretty-printer converts from snick source code to well-formed style *)
open Snick_ast
open Format

let rec print_program fmtr prog = print_procs fmtr prog

and print_procs fmtr = function
    | [] -> ()
    | x::[] -> fprintf fmtr "%a@," print_proc x
    | x::xs -> fprintf fmtr "%a@,%a" print_proc x print_procs xs

and print_proc fmtr (header, body) =
    fprintf fmtr "@[<v>proc %a@;<0 4>@[<v>%a@]@,end@]@." print_proc_header header print_proc_body body

and print_proc_header fmtr (ident, params) =
    fprintf fmtr "%s (%a)" ident print_params params

and print_params fmtr = function
    | [] -> ()
    | x :: [] -> fprintf fmtr "%a" print_param x
    | x :: xs -> fprintf fmtr "%a, %a" print_param x print_params  xs

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
    fprintf fmtr "%a@,%a" print_decls prog_body.decls print_stmts prog_body.stmts
    
and print_decls fmtr = function
    | [] -> ()
    | x :: [] -> fprintf fmtr "%a@," print_decl x
    | x :: xs -> fprintf fmtr "%a@,%a" print_decl x print_decls xs

and print_stmts fmtr = function
    | [] -> ()
    | x :: [] -> fprintf fmtr "%a" print_stmt x
    | x :: xs -> fprintf fmtr "%a@,%a" print_stmt x print_stmts xs

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
    | Array_elem (ident, idxs) -> fprintf fmtr "%s[%a]" ident print_exprs idxs

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
    | Op_add -> fprintf fmtr "%a" print_add_expr (lexpr, rexpr)
    | Op_sub -> fprintf fmtr "%a" print_sub_expr (lexpr, rexpr)
    | Op_mul -> fprintf fmtr "%a" print_mul_expr (lexpr, rexpr)
    | Op_div -> fprintf fmtr "%a" print_div_expr (lexpr, rexpr)
    | Op_eq -> fprintf fmtr "%a" print_eq_expr (lexpr, rexpr)
    | Op_ne -> fprintf fmtr "%a" print_ne_expr (lexpr, rexpr)
    | Op_lt -> fprintf fmtr "%a" print_lt_expr (lexpr, rexpr)
    | Op_gt -> fprintf fmtr "%a" print_gt_expr (lexpr, rexpr)
    | Op_le -> fprintf fmtr "%a" print_le_expr (lexpr, rexpr)
    | Op_ge -> fprintf fmtr "%a" print_ge_expr (lexpr, rexpr)
    | Op_and -> fprintf fmtr "%a" print_and_expr (lexpr, rexpr)
    | Op_or -> fprintf fmtr "%a" print_or_expr (lexpr, rexpr)

and print_unop fmtr (unop, expr) = match unop with
    | Op_not -> fprintf fmtr "%a" print_not_expr expr
    | Op_minus -> fprintf fmtr "%a" print_minus_expr expr

and print_add_expr fmtr = function
    | (lexpr, Eparen rexpr_inside) ->
        begin
            let
                lexpr_strip = strip_paren lexpr 
            and
                rexpr_inside_strip = strip_paren rexpr_inside
            in
                match rexpr_inside_strip with
                | Eelem _
                | Ebool _
                | Eint _
                | Efloat _
                | Ebinop (_, Op_mul, _)
                | Ebinop (_, Op_div, _)
                | Eunop (Op_minus, _) -> fprintf fmtr "%a + %a" print_expr lexpr_strip print_expr rexpr_inside_strip
                | _ -> fprintf fmtr "%a + (%a)" print_expr lexpr_strip print_expr rexpr_inside_strip
        end
    | (lexpr, rexpr) ->
        let
            lexpr_strip = strip_paren lexpr
        in
            fprintf fmtr "%a + %a" print_expr lexpr_strip print_expr rexpr

and print_sub_expr fmtr = function
    | (lexpr, Eparen rexpr_inside) ->
        begin
            let
                lexpr_strip = strip_paren lexpr 
            and
                rexpr_inside_strip = strip_paren rexpr_inside
            in
                match rexpr_inside_strip with
                | Eelem _
                | Ebool _
                | Eint _
                | Efloat _
                | Ebinop (_, Op_mul, _)
                | Ebinop (_, Op_div, _)
                | Eunop (Op_minus, _) -> fprintf fmtr "%a - %a" print_expr lexpr_strip print_expr rexpr_inside_strip
                | _ -> fprintf fmtr "%a - (%a)" print_expr lexpr_strip print_expr rexpr_inside_strip
        end
    | (lexpr, rexpr) ->
        let
            lexpr_strip = strip_paren lexpr
        in
            fprintf fmtr "%a - %a" print_expr lexpr_strip print_expr rexpr

and print_mul_expr fmtr = function
    | (Eparen lexpr_inside, Eparen rexpr_inside) ->
        begin
            let
                lexpr_inside_strip = strip_paren lexpr_inside
            and
                rexpr_inside_strip = strip_paren rexpr_inside
            in
                match lexpr_inside_strip with
                | Eelem _
                | Ebool _
                | Eint _
                | Efloat _
                | Ebinop (_, Op_mul, _)
                | Ebinop (_, Op_div, _)
                | Eunop (Op_minus, _) ->
                    begin
                        match rexpr_inside_strip with
                        | Eelem _
                        | Ebool _
                        | Eint _
                        | Efloat _
                        | Eunop (Op_minus, _) -> fprintf fmtr "%a * %a" print_expr lexpr_inside_strip print_expr rexpr_inside_strip            
                        | _ -> fprintf fmtr "%a * (%a)" print_expr lexpr_inside_strip print_expr rexpr_inside_strip
                    end
                | _ ->
                    begin
                        match rexpr_inside_strip with
                        | Eelem _
                        | Ebool _
                        | Eint _
                        | Efloat _
                        | Eunop (Op_minus, _) -> fprintf fmtr "(%a) * %a" print_expr lexpr_inside_strip print_expr rexpr_inside_strip            
                        | _ -> fprintf fmtr "(%a) * (%a)" print_expr lexpr_inside_strip print_expr rexpr_inside_strip
                    end
        end
    | (Eparen lexpr_inside, rexpr) ->
        begin
            let
                lexpr_inside_strip = strip_paren lexpr_inside
            in
                match lexpr_inside_strip with
                | Eelem _
                | Ebool _
                | Eint _
                | Efloat _
                | Ebinop (_, Op_mul, _)
                | Ebinop (_, Op_div, _)
                | Eunop (Op_minus, _) -> fprintf fmtr "%a * %a" print_expr lexpr_inside_strip print_expr rexpr
                | _ -> fprintf fmtr "(%a) * %a" print_expr lexpr_inside_strip print_expr rexpr
        end
    | (lexpr, Eparen rexpr_inside) ->
        begin
            let
                rexpr_inside_strip = strip_paren rexpr_inside
            in
                match rexpr_inside_strip with
                | Eelem _
                | Ebool _
                | Eint _
                | Efloat _
                | Eunop (Op_minus, _) -> fprintf fmtr "%a * %a" print_expr lexpr print_expr rexpr_inside_strip            
                | _ -> fprintf fmtr "%a * (%a)" print_expr lexpr print_expr rexpr_inside_strip
        end
    | (lexpr, rexpr) -> fprintf fmtr "%a * %a" print_expr lexpr print_expr rexpr

and print_div_expr fmtr = function
    | (Eparen lexpr_inside, Eparen rexpr_inside) ->
        begin
            let
                lexpr_inside_strip = strip_paren lexpr_inside
            and
                rexpr_inside_strip = strip_paren rexpr_inside
            in
                match lexpr_inside_strip with
                | Eelem _
                | Ebool _
                | Eint _
                | Efloat _
                | Ebinop (_, Op_mul, _)
                | Ebinop (_, Op_div, _)
                | Eunop (Op_minus, _) ->
                    begin
                        match rexpr_inside_strip with
                        | Eelem _
                        | Ebool _
                        | Eint _
                        | Efloat _
                        | Eunop (Op_minus, _) -> fprintf fmtr "%a / %a" print_expr lexpr_inside_strip print_expr rexpr_inside_strip            
                        | _ -> fprintf fmtr "%a / (%a)" print_expr lexpr_inside_strip print_expr rexpr_inside_strip
                    end
                | _ ->
                    begin
                        match rexpr_inside_strip with
                        | Eelem _
                        | Ebool _
                        | Eint _
                        | Efloat _
                        | Eunop (Op_minus, _) -> fprintf fmtr "(%a) / %a" print_expr lexpr_inside_strip print_expr rexpr_inside_strip            
                        | _ -> fprintf fmtr "(%a) / (%a)" print_expr lexpr_inside_strip print_expr rexpr_inside_strip
                    end
        end
    | (Eparen lexpr_inside, rexpr) ->
        begin
            let
                lexpr_inside_strip = strip_paren lexpr_inside
            in
                match lexpr_inside_strip with
                | Eelem _
                | Ebool _
                | Eint _
                | Efloat _
                | Ebinop (_, Op_mul, _)
                | Ebinop (_, Op_div, _)
                | Eunop (Op_minus, _) -> fprintf fmtr "%a / %a" print_expr lexpr_inside_strip print_expr rexpr
                | _ -> fprintf fmtr "(%a) / %a" print_expr lexpr_inside_strip print_expr rexpr
        end
    | (lexpr, Eparen rexpr_inside) ->
        begin
            let
                rexpr_inside_strip = strip_paren rexpr_inside
            in
                match rexpr_inside_strip with
                | Eelem _
                | Ebool _
                | Eint _
                | Efloat _
                | Eunop (Op_minus, _) -> fprintf fmtr "%a / %a" print_expr lexpr print_expr rexpr_inside_strip            
                | _ -> fprintf fmtr "%a / (%a)" print_expr lexpr print_expr rexpr_inside_strip
        end
    | (lexpr, rexpr) -> fprintf fmtr "%a / %a" print_expr lexpr print_expr rexpr

and print_eq_expr fmtr = function
    | (Eparen lexpr_inside, Eparen rexpr_inside) ->
        begin
            let
                lexpr_inside_strip = strip_paren lexpr_inside
            and
                rexpr_inside_strip = strip_paren rexpr_inside
            in
                match lexpr_inside_strip with
                | Ebinop (_, Op_and, _)
                | Ebinop (_, Op_or, _)
                | Eunop (Op_not, _) ->
                    begin
                        match rexpr_inside_strip with
                        | Eelem _
                        | Ebool _
                        | Eint _
                        | Efloat _
                        | Ebinop (_, Op_add, _)
                        | Ebinop (_, Op_sub, _)
                        | Ebinop (_, Op_mul, _)
                        | Ebinop (_, Op_div, _)
                        | Eunop (Op_minus, _) -> fprintf fmtr "(%a) = %a" print_expr lexpr_inside_strip print_expr rexpr_inside_strip            
                        | _ -> fprintf fmtr "(%a) = (%a)" print_expr lexpr_inside_strip print_expr rexpr_inside_strip
                    end
                | _ ->
                    begin
                        match rexpr_inside_strip with
                        | Eelem _
                        | Ebool _
                        | Eint _
                        | Efloat _
                        | Ebinop (_, Op_add, _)
                        | Ebinop (_, Op_sub, _)
                        | Ebinop (_, Op_mul, _)
                        | Ebinop (_, Op_div, _)
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
                | Ebinop (_, Op_or, _)
                | Ebinop (_, Op_and, _)
                | Eunop (Op_not, _) -> fprintf fmtr "(%a) = %a" print_expr lexpr_inside_strip print_expr rexpr
                | _ -> fprintf fmtr "%a = %a" print_expr lexpr_inside_strip print_expr rexpr
        end
    | (lexpr, Eparen rexpr_inside) ->
        begin
            let
                rexpr_inside_strip = strip_paren rexpr_inside
            in
                match rexpr_inside_strip with
                | Eelem _
                | Ebool _
                | Eint _
                | Efloat _
                | Ebinop (_, Op_add, _)
                | Ebinop (_, Op_sub, _)
                | Ebinop (_, Op_mul, _)
                | Ebinop (_, Op_div, _)
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
                | Ebinop (_, Op_and, _)
                | Ebinop (_, Op_or, _)
                | Eunop (Op_not, _) ->
                    begin
                        match rexpr_inside_strip with
                        | Eelem _
                        | Ebool _
                        | Eint _
                        | Efloat _
                        | Ebinop (_, Op_add, _)
                        | Ebinop (_, Op_sub, _)
                        | Ebinop (_, Op_mul, _)
                        | Ebinop (_, Op_div, _)
                        | Eunop (Op_minus, _) -> fprintf fmtr "(%a) != %a" print_expr lexpr_inside_strip print_expr rexpr_inside_strip            
                        | _ -> fprintf fmtr "(%a) != (%a)" print_expr lexpr_inside_strip print_expr rexpr_inside_strip
                    end
                | _ ->
                    begin
                        match rexpr_inside_strip with
                        | Eelem _
                        | Ebool _
                        | Eint _
                        | Efloat _
                        | Ebinop (_, Op_add, _)
                        | Ebinop (_, Op_sub, _)
                        | Ebinop (_, Op_mul, _)
                        | Ebinop (_, Op_div, _)
                        | Eunop (Op_minus, _) -> fprintf fmtr "%a != %a" print_expr lexpr_inside_strip print_expr rexpr_inside_strip            
                        | _ -> fprintf fmtr "%a != (%a)" print_expr lexpr_inside_strip print_expr rexpr_inside_strip
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
                | Eelem _
                | Ebool _
                | Eint _
                | Efloat _
                | Ebinop (_, Op_add, _)
                | Ebinop (_, Op_sub, _)
                | Ebinop (_, Op_mul, _)
                | Ebinop (_, Op_div, _)
                | Eunop (Op_minus, _) -> fprintf fmtr "%a != %a" print_expr lexpr print_expr rexpr_inside_strip            
                | _ -> fprintf fmtr "%a != (%a)" print_expr lexpr print_expr rexpr_inside_strip
        end
    | (lexpr, rexpr) -> fprintf fmtr "%a != %a" print_expr lexpr print_expr rexpr

and print_lt_expr fmtr (lexpr, rexpr) = 
        let
            lexpr_strip = strip_paren lexpr
        and
            rexpr_strip = strip_paren rexpr
        in
            fprintf fmtr "%a < %a" print_expr lexpr_strip print_expr rexpr_strip


and print_gt_expr fmtr (lexpr, rexpr) = 
        let
            lexpr_strip = strip_paren lexpr
        and
            rexpr_strip = strip_paren rexpr
        in
            fprintf fmtr "%a > %a" print_expr lexpr_strip print_expr rexpr_strip

and print_le_expr fmtr (lexpr, rexpr) = 
        let
            lexpr_strip = strip_paren lexpr
        and
            rexpr_strip = strip_paren rexpr
        in
            fprintf fmtr "%a <= %a" print_expr lexpr_strip print_expr rexpr_strip

and print_ge_expr fmtr (lexpr, rexpr) = 
        let
            lexpr_strip = strip_paren lexpr
        and
            rexpr_strip = strip_paren rexpr
        in
            fprintf fmtr "%a >= %a" print_expr lexpr_strip print_expr rexpr_strip

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
                        | Ebinop (_, Op_and, _)
                        | Ebinop (_, Op_or, _) -> fprintf fmtr "(%a) and (%a)" print_expr lexpr_inside_strip print_expr rexpr_inside_strip
                        | _ -> fprintf fmtr "(%a) and %a" print_expr lexpr_inside_strip print_expr rexpr_inside_strip
                    end
                | _ ->
                    begin
                        match rexpr_inside_strip with
                        | Ebinop (_, Op_and, _)
                        | Ebinop (_, Op_or, _) -> fprintf fmtr "%a and (%a)" print_expr lexpr_inside_strip print_expr rexpr_inside_strip
                        | _ -> fprintf fmtr "%a and %a" print_expr lexpr_inside_strip print_expr rexpr_inside_strip
                    end
        end
    | (Eparen lexpr_inside, rexpr) ->
        begin
            let
               lexpr_inside_strip = strip_paren lexpr_inside
            in
                match lexpr_inside_strip with
                | Ebinop (_, Op_or, _) -> fprintf fmtr "(%a) and %a" print_expr lexpr_inside_strip print_expr rexpr
                | _ -> fprintf fmtr "%a and %a" print_expr lexpr_inside_strip print_expr rexpr
        end
    | (lexpr, Eparen rexpr_inside) ->
        begin
            let
                rexpr_inside_strip = strip_paren rexpr_inside
            in
                match rexpr_inside_strip with
                | Ebinop (_, Op_and, _)
                | Ebinop (_, Op_or, _) -> fprintf fmtr "%a and (%a)" print_expr lexpr print_expr rexpr_inside_strip
                | _ -> fprintf fmtr "%a and %a" print_expr lexpr print_expr rexpr_inside_strip
        end
    | (lexpr, rexpr) -> fprintf fmtr "%a and %a" print_expr lexpr print_expr rexpr

and print_or_expr fmtr = function
    | (lexpr, Eparen rexpr_inside) ->
        begin
            let
                lexpr_strip = strip_paren lexpr
            and
                rexpr_inside_strip = strip_paren rexpr_inside
            in
                match rexpr_inside_strip with
                | Ebinop (_, Op_or, _) -> fprintf fmtr "%a or (%a)" print_expr lexpr_strip print_expr rexpr_inside_strip
                | _ -> fprintf fmtr "%a or %a" print_expr lexpr_strip print_expr rexpr_inside_strip
        end
    | (lexpr, rexpr) -> 
        let
            lexpr_strip = strip_paren lexpr
        in
            fprintf fmtr "%a or %a" print_expr lexpr_strip print_expr rexpr

and print_not_expr fmtr = function
    | Eparen expr_inside ->
        begin
            let
                expr_inside_strip = strip_paren expr_inside
            in
                match expr_inside_strip with
                | Ebinop (_, Op_and, _)
                | Ebinop (_, Op_or, _) -> fprintf fmtr "%s (%a)" "not" print_expr expr_inside_strip
                | _ -> fprintf fmtr "%s %a" "not" print_expr expr_inside_strip
        end
    | expr -> fprintf fmtr "%s %a" "not" print_expr expr

and print_minus_expr fmtr = function
    | Eparen expr_inside ->
        begin
            let
                expr_inside_strip = strip_paren expr_inside
            in
                match expr_inside_strip with
                | Eunop (Op_minus, _) -> fprintf fmtr "%s %a" "-" print_expr expr_inside_strip
                | _ -> fprintf fmtr "%s (%a)" "-" print_expr expr_inside_strip
        end
    | expr -> fprintf fmtr "%s %a" "-" print_expr expr

and strip_paren expr = match expr with
    | Eparen paren_expr -> strip_paren paren_expr
    | _ -> expr
