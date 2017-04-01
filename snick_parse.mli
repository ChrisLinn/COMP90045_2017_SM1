type token =
  | EOF
  | PROC
  | END
  | SEMICOLON
  | COMMA
  | DOT
  | WHILE
  | DO
  | OD
  | IF
  | THEN
  | ELSE
  | FI
  | BOOL_CONST of (bool)
  | INT_CONST of (int)
  | FLOAT_CONST of (float)
  | VAL
  | REF
  | IDENT of (string)
  | BOOL
  | INT
  | FLOAT
  | WRITE
  | READ
  | ASSIGN
  | OR
  | AND
  | NOT
  | EQ
  | NE
  | LT
  | LE
  | GT
  | GE
  | PLUS
  | MINUS
  | MULTI
  | DIVID
  | LSQBRACK
  | RSQBRACK
  | LPAREN
  | RPAREN

val program :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Snick_ast.program
