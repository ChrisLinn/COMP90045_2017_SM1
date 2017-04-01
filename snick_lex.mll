{
open Snick_parse
}

let digit = ['0' - '9']
let digits = digit+
let floating = digits '.' digits
let alpha = ['a' - 'z' 'A' - 'Z']
let alnum = alpha | '_' | '\'' | digit
let ident = (alpha | '_') alnum*

rule token = parse
	| [' ' '\t']		{ token lexbuf } (* skip blanks*)
	| ['\n'] (*line break*)
	| '-'? digits as lxm 				{ INT_CONST (int_of_string lxm) }
	| '-'? floating as lxm				{ FLOAT_CONST (float_of_string lxm) }
	| ident as lxm						{ IDENT lxm }
	| eof								{ EOF }
  	(* keywords *)
	| "not"								{ NOT }
	| "and"								{ AND }
	| "or"								{ OR }
	| "float"							{ FLOAT }
	| "int"								{ INT }
	| "bool"							{ BOOL }
	| "false"							{ BOOL_CONST false }
	| "true"							{ BOOL_CONST true }
	| "while"							{ WHILE }
	| "do"								{ DO }
	| "od"								{ OD }
	| "if"								{ IF }
	| "then"							{ THEN }
	| "else"							{ ELSE }
	| "fi"								{ FI }
	| "proc"							{ PROC }
	| "end"								{ END }
	| "read"							{ READ }
	| "write"							{ WRITE }
	| "ref"								{ REF }
	| "val"								{ VAL }
	| ":="								{ ASSIGN }
  	| '(' 								{ LPAREN }
  	| ')' 								{ RPAREN }
  	| '[' 								{ LBRACK }
  	| ']' 								{ RBRACK }
	| '='								{ EQ }
	| '.'								{ DOT }
	| "!="								{ NE }
	| ">="								{ GE }
	| "<="								{ LE }
	| '>'								{ GT }
	| '<'								{ LT }
	| '+'								{ PLUS }
	| '-'								{ MINUS }
	| '*'								{ MULTI }
	| '/'								{ DIVID }
	| ','								{ COMMA }
	| ';'								{ SEMICOLON }
