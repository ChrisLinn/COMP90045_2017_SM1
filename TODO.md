# Todo

+ [ ] symbol table
    + [ ] stack_cnt for a scope or a table?
        + [ ] dont understand slots_needed_for_table
    + [ ] sbValType
        + [ ] great, seems can be used to opt (update type and value)?
            + [ ] removed temporally
+ [ ] analyze
+ [ ] codegen
    + [ ] don't understand ini_regs's counting
+ [ ] may need to alter AST
+ [ ] array
    + [ ] option type
    + [ ] decls
        + [ ] analyze
        + [ ] codegen
+ [ ] error handling
    + [ ] analyze (recursively) assign/if/while/read
    + [ ] find main
        + [ ] print main first?
    + [ ] semantic error report in codegen
        + [ ] justification needed for semantic analysis
            + [ ] report or raise a failure
        + [ ] print_dupe_proc_errors
+ [ ] optimization
+ [ ] ifdebug dump symbol table
+ [ ] lineno seems unuserful?
    + [ ] seems used for error reporting, can be useful for semantic error report
+ [ ] better to gen_main & gen_halt or init?
+ [ ] begin-end -> () style