(* *)

type compiler_mode = PrettyPrint | Compile

let source_file = ref None
let mode = ref Compile


let (speclist:(Arg.key * Arg.spec * Arg.doc) list) =
	["-p",
		Arg.Unit(fun () -> mode := PrettyPrint),
		" Run the compiler in pretty-printer mode"
	]

let main () =
	Arg.parse speclist
		(begin fun fname -> source_file := Some fname end)
		"snick [-p] [snick source]"
	let infile = match !source_file with
	| None -> stdin
	| Some fname -> open_in fname in
	let lexbuf = Lexing.from_channel infile in
	let prog = Snick_parse.program Snick_lex.token lexbuf in
	match !mode with
	| PrettyPrint -> 
	  Snick_pprint.print_program Format.std_formatter prog
	| Compile -> Printf.printf "Compile mode is not yet supported!!!"

let _ = main ()
