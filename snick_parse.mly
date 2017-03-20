%{
open Snack_ast
%}

%token <bool> BOOL_CONST
%token <int> INT_CONST
%token <float> FLOAT_CONST
%token <string> IDENT
%token BOOL INT FLOAT
%token WRITE READ
%token ASSIGN
%token LPAREN RPAREN
%token EQ NE LT GT LE GE
%token PLUS MINUS
%token MULTI DIVID
%token SEMICOLON
%token EOF
%token NOT AND OR
%token WHILE DO OD
%token IF THEN ELSE FI
%token PROC END
%token REF VAL

%nonassoc EQ NE LT GT LE GE
%left PLUS MINUS
%left MULTI DIVID
%left NOT AND OR
(*%nonassoc UMINUS*)

%type <Snick_ast.program> program

%start program
%%

program:
	procs	{ $1 }

procs:
	PROC
	proc_header { }
	proc_body	{ }
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

typespec:
	| BOOL { Bool }
	| INT { Int }
	| FLOAT { Float }

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

expr:
	| IDENT
	| IDENT dimension
	| BOOL_CONST	{ Ebool }
	| INT_CONST		{ Eint }
	| FLOAT_CONST	{ Efloat }
	| LPAREN expr RPAREN
	| expr PLUS expr { $1 + $2 }
	| expr MINUS expr { $1 - $2 }
	| expr MULTI expr { $1 * $2 }
	| expr DIVID expr { $1 / $2 }
	| expr EQ expr
	| expr NE expr
	| expr LT expr
	| expr GT expr
	| expr LE expr
	| expr GE expr
	| expr AND expr
	| expr OR expr
	| NOT expr


