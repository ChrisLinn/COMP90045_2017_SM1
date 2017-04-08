module P = Snick_parse

(* Argument parsing code *)
let infile_name = ref None

type compiler_mode = PrettyPrint | Compile
let mode = ref Compile

(* print current position of lexbuf *)
let err_pos lexbuf =
    let pos = Lexing.lexeme_start_p lexbuf in
        Format.sprintf ": line %d, col %d."
            (pos.Lexing.pos_lnum)
            (pos.Lexing.pos_cnum - pos.Lexing.pos_bol) 

(* --------------------------------------------- *)
(*  Specification for command-line options       *)
(* --------------------------------------------- *)
let (speclist:(Arg.key * Arg.spec * Arg.doc) list) =
    ["-p",
       Arg.Unit(fun () -> mode := PrettyPrint),
       " Run the compiler in pretty-printer mode"
    ]

let main () =
    (* Parse the command-line arguments *)
    Arg.parse speclist
        (begin fun fname -> infile_name := Some fname end)
        "snick [-p] [bean source]" ;
    (* Open the input file *)
    let infile = match !infile_name with
    | None -> stdin
    | Some fname -> open_in fname in
    (* Initialize lexing buffer *)
    let lexbuf = Lexing.from_channel infile in
    (* Call the parser *)
    try
        let prog = Snick_parse.program Snick_lex.token lexbuf in
        match !mode with
        | PrettyPrint -> Snick_pprint.print_program Format.std_formatter prog 
        | Compile -> print_string "Compiling function is not yet enabled!!!\n"
    with
        | Failure x -> print_string ("Lexing Error" ^ (err_pos lexbuf) ^ "\n")
        | Parsing.Parse_error -> print_string ("Parsing Error" ^ (err_pos lexbuf) ^ "\n")

let _ = main ()
