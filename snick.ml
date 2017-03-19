(* *)

type compiler_mode = PrettyPrint | Compile

let mode = ref Compile
let source_file = ref "None"
let put = Printf.printf
let get = Lexing.lexeme
(* 
	let source_file = ref None
	(*
		Error: The type of this expression, '_a option ref,
		contains type variables that cannot be generalized
	*)
*)


let (speclist:(Arg.key * Arg.spec * Arg.doc) list) =
	[
		"-p",
		Arg.String (fun fname -> 
						mode := PrettyPrint;
						source_file := fname(* ; Printf.printf "%s\n" fname *)),
		" Run the compiler in pretty-printer mode"
	]

let main () =
	Arg.parse
		speclist
		(fun anon_arg -> ())
		"snick [-p] [snick source]";
		(* 
			anonymous arguments:
			http://stackoverflow.com/questions/29966941/make-ocaml-arg-parse-function-accept-anonymous-arguments-starting-with
		*)
	(* put "%s\n" (!source_file); *)
				let infile = match !source_file with
								| "None" -> stdin
								| fname -> open_in fname in
			let lexbuf = Lexing.from_channel infile in
		let prog = Parser.main Lexer.token lexbuf in
	match !mode with
		| PrettyPrint -> Snick_pprint.print_program Format.std_formatter prog
		| Compile -> put "Compile mode is not yet supported!!!"

let _ = main ()

