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

%nonassoc EQ NE LT GT LE GE
%left PLUS MINUS
%left MULTI DIVID
(*%nonassoc UMINUS*)

%type <Snick_ast.program> program

%start program
%%



