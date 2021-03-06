(*
** File:          snick_optimizer.ml
** Description:   Module to do simple local optimizations to a program.
**				  At this stage, it is only evaluating values of expressions
**				  before turning into brill code.
** Last Modified: Tue. 16th May 2017 
** 
** Group name: Mainframe
** 
** Member names   | usernames
** Xianzhuo REN   | xianzhuor 
** Haoyu LIN      | haoyul3
** Zequn MA       | zequnm
*)

open Snick_ast
open Snick_symbol
open Snick_err
open Format

(* Start optimization (simplification) procedure by procedure *)
let rec simplify_prog prog =
    List.map simplify_proc prog

(* Simplify a procedure,
** returns a procdure type with simplified procedure body *)
and simplify_proc ((proc_id,proc_params),proc_body) = 
    ((proc_id,proc_params),(simplify_proc_body proc_id proc_body))

(* Return the same procedure body with simplified statments *)
and simplify_proc_body proc_id proc_body = 
    {decls=proc_body.decls;stmts=(simplify_stmts proc_id proc_body.stmts)}

(* Simplify statement by statment *)
and simplify_stmts proc_id stmts =
    List.map (simplify_stmt proc_id) stmts

(* Select simplification method depending on type of statement *)
and simplify_stmt proc_id = function
    | Assign(elem,expr) ->
        Assign((simplify_elem proc_id elem),(simplify_expr proc_id expr)) 
    | Read(elem) -> Read(simplify_elem proc_id elem)   
    | Write(write_expr) -> Write(simplify_write_expr proc_id write_expr)
    | Call(id,exprs) -> Call(id,(List.map (simplify_expr proc_id) exprs))
    | If_then(expr,stmts) -> If_then((simplify_expr proc_id expr),
                                (simplify_stmts proc_id stmts))
    | If_then_else(expr,then_stmts,else_stmts) ->
                            If_then_else((simplify_expr proc_id expr),
                                (simplify_stmts proc_id then_stmts),
                                (simplify_stmts proc_id else_stmts))
    | While(expr,stmts) -> While((simplify_expr proc_id expr),
                                (simplify_stmts proc_id stmts))

(* For array indexing, return the value
** otherwise, retrun as it is *)
and simplify_elem proc_id = function
    | Elem(id,Some idxs) ->
        Elem(id,Some (List.map (simplify_expr proc_id) idxs))
    | elem -> elem

(* Run simplication on expression being written if not string *)
and simplify_write_expr proc_id = function
    | Expr(expr) -> Expr(simplify_expr proc_id expr)
    | String(string_const) -> String(string_const)

(* Evaluate a expression by its type if possible *)
and simplify_expr proc_id = function
    | Eparen(expr) -> simplify_expr proc_id expr
    | Eelem(elem) -> Eelem(simplify_elem proc_id elem)
    | Ebinop(lexpr,optr,rexpr) -> eval_binop proc_id lexpr optr rexpr
    | Eunop(optr,expr) -> eval_unop proc_id optr expr
    | ori_expr -> ori_expr

(* Evaluate binary operation if possible *)
and eval_binop proc_id lexpr optr rexpr =
    let simplified_lexpr = simplify_expr proc_id lexpr
    and simplified_rexpr = simplify_expr proc_id rexpr
    in
    match simplified_lexpr with
    | Ebool(lbool) ->
    (
        match simplified_rexpr with
        | Ebool(rbool) ->
        (
            match optr with
            | Op_or -> Ebool(lbool||rbool)
            | Op_and -> Ebool(lbool&&rbool)
            | Op_eq -> Ebool(lbool=rbool)
            | Op_ne -> Ebool(lbool<>rbool)
            | _ -> error_invalid_operation proc_id
        )
        | _ -> Ebinop(simplified_lexpr,optr,simplified_rexpr)
    )
    | Eint(lint) ->
    (
        match simplified_rexpr with
        | Eint(rint) ->
        (
            match optr with
            | Op_eq -> Ebool(lint=rint)
            | Op_ne -> Ebool(lint<>rint)
            | Op_lt -> Ebool(lint<rint)
            | Op_gt -> Ebool(lint>rint)
            | Op_le -> Ebool(lint<=rint)
            | Op_ge -> Ebool(lint>=rint)
            | Op_add -> Eint(lint+rint)
            | Op_sub -> Eint(lint-rint)
            | Op_mul -> Eint(lint*rint)
            | Op_div -> Eint(lint/rint)
            | _ -> error_invalid_operation proc_id
        )
        | Efloat(rfloat) ->
        (
            match optr with
            | Op_lt -> Ebool((float_of_int lint)<rfloat)
            | Op_gt -> Ebool((float_of_int lint)>rfloat)
            | Op_le -> Ebool((float_of_int lint)<=rfloat)
            | Op_ge -> Ebool((float_of_int lint)>=rfloat)
            | Op_add -> Efloat((float_of_int lint)+.rfloat)
            | Op_sub -> Efloat((float_of_int lint)-.rfloat)
            | Op_mul -> Efloat((float_of_int lint)*.rfloat)
            | Op_div -> Efloat((float_of_int lint)/.rfloat)
            | _ -> error_invalid_operation proc_id
        )
        | _ -> Ebinop(simplified_lexpr,optr,simplified_rexpr)
    )
    | Efloat(lfloat) ->
    (
        match simplified_rexpr with
        | Eint(rint) ->
        (
            match optr with
            | Op_lt -> Ebool(lfloat<(float_of_int rint))
            | Op_gt -> Ebool(lfloat>(float_of_int rint))
            | Op_le -> Ebool(lfloat<=(float_of_int rint))
            | Op_ge -> Ebool(lfloat>=(float_of_int rint))
            | Op_add -> Efloat(lfloat+.(float_of_int rint))
            | Op_sub -> Efloat(lfloat-.(float_of_int rint))
            | Op_mul -> Efloat(lfloat*.(float_of_int rint))
            | Op_div -> Efloat(lfloat/.(float_of_int rint))
            | _ -> error_invalid_operation proc_id
        )
        | Efloat(rfloat) ->
        (
            match optr with
            | Op_eq -> Ebool(lfloat=rfloat)
            | Op_ne -> Ebool(lfloat<>rfloat)
            | Op_lt -> Ebool(lfloat<rfloat)
            | Op_gt -> Ebool(lfloat>rfloat)
            | Op_le -> Ebool(lfloat<=rfloat)
            | Op_ge -> Ebool(lfloat>=rfloat)
            | Op_add -> Efloat(lfloat+.rfloat)
            | Op_sub -> Efloat(lfloat-.rfloat)
            | Op_mul -> Efloat(lfloat*.rfloat)
            | Op_div -> Efloat(lfloat/.rfloat)
            | _ -> error_invalid_operation proc_id
        )
        | _ -> Ebinop(simplified_lexpr,optr,simplified_rexpr)
    )
    | _ -> Ebinop(simplified_lexpr,optr,simplified_rexpr)

(* Evaluate unary operation if possible *)
and eval_unop proc_id optr expr =
    let simplified_expr = simplify_expr proc_id expr
    in
    match simplified_expr with
    | Ebool(bool_const) -> Ebool(not bool_const)
    | Eint(int_const) -> Eint(-int_const)
    | Efloat(float_const) -> Efloat(-.float_const)
    | _ -> Eunop(optr,simplified_expr)

