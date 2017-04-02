{
open Snick_parse
}

let digit = ['0' - '9']
let digits = digit+
let floating = digits '.' digits
let alpha = ['a' - 'z' 'A' - 'Z']
let alnum = alpha | '_' | '\'' | digit
let ident = (alpha | '_') alnum*
let commment = '#' [^'\n']*
let string = '"' [^'"']* '"'

rule token = parse
    | commment                          { token lexbuf } (* skip comments *)
    | [' ' '\t']                        { token lexbuf } (* skip blanks*)
    | '\n'                              { Lexing.new_line lexbuf ; token lexbuf }
    | '-'? digits as lxm                { INT_CONST (int_of_string lxm) }
    | '-'? floating as lxm              { FLOAT_CONST (float_of_string lxm) }
    | eof                               { EOF }
    (*  keywords *)
    | "not"                             { NOT }
    | "and"                             { AND }
    | "or"                              { OR }
    | "float"                           { FLOAT }
    | "int"                             { INT }
    | "bool"                            { BOOL }
    | "false"                           { BOOL_CONST false }
    | "true"                            { BOOL_CONST true }
    | "while"                           { WHILE }
    | "do"                              { DO }
    | "od"                              { OD }
    | "if"                              { IF }
    | "then"                            { THEN }
    | "else"                            { ELSE }
    | "fi"                              { FI }
    | "proc"                            { PROC }
    | "end"                             { END }
    | "read"                            { READ }
    | "write"                           { WRITE }
    | "ref"                             { REF }
    | "val"                             { VAL }
    | ":="                              { ASSIGN }
    | '('                               { LPAREN }
    | ')'                               { RPAREN }
    | '['                               { LSQBRACK }
    | ']'                               { RSQBRACK }
    | '='                               { EQ }
    | '.'                               { DOT }
    | "!="                              { NE }
    | ">="                              { GE }
    | "<="                              { LE }
    | '>'                               { GT }
    | '<'                               { LT }
    | '+'                               { PLUS }
    | '-'                               { MINUS }
    | '*'                               { MULTI }
    | '/'                               { DIVID }
    | ','                               { COMMA }
    | ';'                               { SEMICOLON }
    | ident as lxm                      { IDENT lxm }
    | string as lxm                     { STRING_CONST lxm}
