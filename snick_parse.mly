/* ocamlyacc parser for snick */
%{
open Snack_ast
%}

%token EOF
%token PROC END
%token SEMICOLON
%token WHILE DO OD
%token IF THEN ELSE FI
%token <bool> BOOL_CONST
%token <int> INT_CONST
%token <float> FLOAT_CONST
%token REF VAL
%token <string> INDEX
%token <string> DIMENSION
%token <string> IDENT
%token BOOL INT FLOAT
%token WRITE READ
%token ASSIGN
%token OR
%token AND
%token NOT
%token EQ NE LT LE GT GE
%token PLUS MINUS
%token MULTI DIVID
%token UMINUS
%token LPAREN RPAREN

%nonassoc EQ NE LT GT LE GE
%left PLUS MINUS
%left MULTI DIVID
%left AND OR
%right NOT
%nonassoc UMINUS

%type <Snick_ast.program> program

%start program
%%

typespec:
    | BOOL { Bool }
    | INT { Int }
    | FLOAT { Float }

decl:
  | typespec IDENT SEMICOLON { ($1,$2) }
  | typespec IDENT DIMENSION SEMICOLON { ($1,$2,$3) }

lvalue:
  | IDENT { LId $1 }
  | IDENT INDEX { LArrayItem ($1,$2) }

/*
expr:
  | BOOL_CONST { Ebool $1 }
  | INT_CONST { Eint $1 }
  | FLOAT_CONST { Efloat $1 }
  | lvalue { Elval $1 }
  /* Binary operators */
  | expr PLUS expr { Ebinop ($1, Op_add, $3) }
  | expr MINUS expr { Ebinop ($1, Op_sub, $3) }
  | expr MULTI expr { $1 * $2 }
  | expr DIVID expr { $1 / $2 }
  | LPAREN expr RPAREN
  | expr EQ expr
  | expr NE expr
  | expr LT expr
  | expr GT expr
  | expr LE expr
  | expr GE expr
  | expr AND expr
  | expr OR expr
  | NOT expr

expr:
  | BOOL_CONST { Ebool $1 }
  | INT_CONST { Eint $1 }
  | lvalue { Elval $1 }
  | expr PLUS expr { Ebinop ($1, Op_add, $3) }
  | expr MINUS expr { Ebinop ($1, Op_sub, $3) }
  | expr MUL expr { Ebinop ($1, Op_mul, $3) }
  | expr EQ expr { Ebinop ($1, Op_eq, $3) }
  | expr LT expr { Ebinop ($1, Op_lt, $3) }
  | MINUS expr %prec UMINUS { Eunop (Op_minus, $2) }
  | LPAREN expr RPAREN { $2 }


program:
    procs   { $1 }

procs:
    PROC
    proc_header { }
    proc_body   { }
    END

proc_header:
    
proc_body:
    decls stmts { $1 ; $2 }

decl:
    | typespec IDENT SEMICOLON
    | typespec IDENT ASSIGN lvalue SEMICOLON
    | typespec IDENT dimension ASSIGN lvalue SEMICOLON

decls:
    | decls decl { $2 :: $1 }
    | { [] }

stmts:
    | LBRACK stmts stmt RBRACK { $2 :: $1 }
    | { [] }

stmt:
    stmt_body SEMICOLON { $1 }

stmt_body:
    | IDENT ASSIGN expr
    | IDENT dimension ASSIGN expr
    | READ IDENT
    | READ IDENT dimension
    | WRITE expr
    | IDENT LPAREN expr_list RPAREN

expr_list:
    | IF expr THEN stmts FI
    | IF expr THEN stmts ELSE stmts FI
    | WHILE expr DO stmts OD

*/


