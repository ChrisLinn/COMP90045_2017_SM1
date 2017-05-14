# Todo

+ [X] error handling
    - [X] undelcared
        - [X] elem/array
        - [X] func
    - [X] duplicated decl/def
        - [X] elem/array
        - [X] func
    + [X] analyse_assign in analyze logic error 
    + [X] analyze (recursively) assign/if/while/read
    + [X] semantic error report in analyze
    + [X] float assigned to int
    + [X] pass float arg to int param
    + [X] type check in get_expr_type
    + [X] find main
        + [X] print main first?
    + [X] gen_br_call params args num unmatch
+ [X] array
    + [X] option type
    - [ ] analyze
        - [X] dynamic out_of_bound
        - [X] decls negative interval num / lo_bound>upbound
        - [ ] static out_of_bound
    - [X] codegen
        - [X] gen_br_init_array
        - [X] gen_br_assign
        - [X] gen_br_read
        - [X] gen_br_expr_array_addr
        - [X] gen_br_expr_array_val
        - [X] get_reg_usage
+ [ ] int_to_float
    + [ ] recursive call ref
    + [X] check int_to_float in gen_br_expr_unop seems unuseful
+ [ ] complete line type
+ [X] optimization
+ [X] ifdebug dump symbol table
+ [X] lineno seems unuserful?
    + [X] seems used for error reporting, can be useful for semantic error report
+ [X] begin-end -> () style
+ [X] better to gen_main & gen_halt or init?
+ [X] gen_comment
+ [X] div_by_zero
    + [X] gen_br_div_by_zero
    + [X] label
    + [X] get_reg_usage
+ [X] symbol table
    + [X] stack_cnt for a scope or a table?
        + [X] dont understand slots_needed_for_table
    + [X] sbValType
        + [X] great, seems can be used to opt (update type and value)?
            + [X] removed temporally
    + [X] line num
        + [X] dont know how to get it 
+ [X] analyze
+ [X] codegen
    + [X] gen_br_decls
        + [X] what about bool
            + [X] because brill is writen in c!
+ [X] may need to alter AST