(*
** File:          snick.ml
** Description:   Main file for Snick compiler,
**                modified based on given sample.
** Last Modified: Sun. 9th April 2017 
** 
** Group name: Mainframe
** 
** Member names   | usernames
** Xianzhuo REN   | xianzhuor 
** Haoyu LIN      | haoyul3
** Zequn MA       | zequnm
*)

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
            (pos.Lexing.pos_cnum - pos.Lexing.pos_bol + 1) 

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
        "snick [-p] [snick source]" ;
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
        | PrettyPrint ->
            Snick_pprint.print_program Format.std_formatter prog
        | Compile ->
            Snick_codegen.compile prog
    with
        (* Handle failure from lexer, print error position. *)
        | Snick_lex.LexErr ->
            failwith ("Lexing Error" ^ (err_pos lexbuf))
        (* Handle error from parser, print error position. *)
        | Parsing.Parse_error ->
            failwith ("Parsing Error" ^ (err_pos lexbuf))
        | Failure x ->
            failwith ("Semantic Error: "^x)

let _ = main ()
