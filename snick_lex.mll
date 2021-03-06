(*
** File:          snick_lex.ml
** Description:   Specification of a lexer for Snick
** Last Modified: Tue. 4th April 2017
** 
** Group name: Mainframe
** 
** Member names   | usernames
** Xianzhuo REN   | xianzhuor 
** Haoyu LIN      | haoyul3
** Zequn MA       | zequnm
*)

{
open Snick_parse

exception LexErr
}


(* some regex patterns *)
let digit = ['0' - '9']
let digits = digit+
let floating = digits '.' digits
let alpha = ['a' - 'z' 'A' - 'Z']
let alnum = alpha | '_' | '\'' | digit
let ident = (alpha | '_') alnum*
let commment = '#' [^'\n']*     (* comments *)
let string = '"' [^'"']* '"'    (* string constant for write statement *)

rule token = parse
    | commment                      { token lexbuf } (* skip comments *)
    | [' ' '\t' '\r']               { token lexbuf } (* skip blanks*)
    | '\n'                          { Lexing.new_line lexbuf ; token lexbuf }
    | '-'? digits as lxm            { INT_CONST (int_of_string lxm) }
    | '-'? floating as lxm          { FLOAT_CONST (float_of_string lxm) }
    | eof                           { EOF }
    (*  keywords *)
    | "not"                         { NOT }
    | "and"                         { AND }
    | "or"                          { OR }
    | "float"                       { FLOAT }
    | "int"                         { INT }
    | "bool"                        { BOOL }
    | "false"                       { BOOL_CONST false }
    | "true"                        { BOOL_CONST true }
    | "while"                       { WHILE }
    | "do"                          { DO }
    | "od"                          { OD }
    | "if"                          { IF }
    | "then"                        { THEN }
    | "else"                        { ELSE }
    | "fi"                          { FI }
    | "proc"                        { PROC }
    | "end"                         { END }
    | "read"                        { READ }
    | "write"                       { WRITE }
    | "ref"                         { REF }
    | "val"                         { VAL }
    | ":="                          { ASSIGN }
    | '('                           { LPAREN }
    | ')'                           { RPAREN }
    | '['                           { LSQBRACK }
    | ']'                           { RSQBRACK }
    | '='                           { EQ }
    | '.'                           { DOT }
    | "!="                          { NE }
    | ">="                          { GE }
    | "<="                          { LE }
    | '>'                           { GT }
    | '<'                           { LT }
    | '+'                           { PLUS }
    | '-'                           { MINUS }
    | '*'                           { MULTI }
    | '/'                           { DIVID }
    | ','                           { COMMA }
    | ';'                           { SEMICOLON }
    | ident as lxm                  { IDENT lxm }
    | string as lxm                 { STRING_CONST lxm}
    | _                             { raise LexErr}