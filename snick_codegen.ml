open Snick_analyze


let rec compile prog =
    let table = analyse prog in
        let brprog = gen_br_program prog table in
            print_lines brprog
(*i guess we dont need to check table == NULL, we could just check Snick_analyze.isValid*)
(* 
compile(FILE *fp, Program *prog) {
    void *table = analyse(prog);
    if (table == NULL) {
        //Then did not pass semantic analysis. Exit
        report_error_and_exit("Invalid program.");
    }
    OzProgram *ozprog = gen_oz_program(prog, table);
    print_lines(fp, ozprog->start);
    return (int)(!ozprog);
}
*)


and print_lines lines = ()

and gen_br_program prog table = ()