# Todo

+ [X] symbol table
    + [X] stack_cnt for a scope or a table?
        + [X] dont understand slots_needed_for_table
    + [X] sbValType
        + [X] great, seems can be used to opt (update type and value)?
            + [X] removed temporally
    + [ ] line num
        + [ ] dont know how to get it 
+ [ ] analyze
+ [ ] codegen
    + [X] gen_br_decls
        + [X] what about bool
            + [X] because brill is writen in c!
+ [ ] may need to alter AST
+ [ ] int_to_float
    + [ ] recursive call ref
    + [X] check int_to_float in gen_br_expr_unop seems unuseful
+ [ ] array
    + [ ] option type
    + [ ] decls
        + [ ] analyze
        + [ ] codegen
            + [ ] gen_br_init_array
            + [ ] gen_br_assign
            + [ ] gen_br_read
            + [ ] gen_br_expr_array_addr
            + [ ] gen_br_expr_array_val
            + [ ] get_reg_usage
+ [ ] error handling
    + [ ] analyse_assign in analyze logic error 
    + [ ] analyze (recursively) assign/if/while/read
    + [ ] semantic error report in analyze
    + [X] float assigned to int
    + [X] pass float arg to int param
    + [X] gen_br_call params args num unmatch
        + [X] justification needed for semantic analysis
            + [X] report or raise a failure
        + [ ] print_dupe_proc_errors
    + [X] type check in get_expr_type
    + [X] find main
        + [X] print main first?
+ [X] optimization
+ [ ] complete line type
+ [ ] ifdebug dump symbol table
+ [X] lineno seems unuserful?
    + [X] seems used for error reporting, can be useful for semantic error report
+ [X] begin-end -> () style
+ [X] better to gen_main & gen_halt or init?
+ [X] gen_comment
+ [X] div_by_zero
    + [X] gen_br_div_by_zero
    + [X] label
    + [X] get_reg_usage