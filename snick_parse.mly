/* ocamlyacc parser for snick */
%{
open Snack_ast
%}

%token EOF
%token PROC END
%token SEMICOLON
%token COMMA
%token DOT
%token WHILE DO OD
%token IF THEN ELSE FI
%token <bool> BOOL_CONST
%token <int> INT_CONST
%token <float> FLOAT_CONST
%token VAL REF
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
%token LBRACK RBRACK
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

program:
  procs   { { procs = List.rev $1 } }

/* Builds procs in reverse order */
procs:
  | procs proc { $2 :: $1 }
  | proc { [$1] }

proc:
  PROC proc_header proc_body END { ($2, $3) }

proc_header:
  IDENT LPAREN pramas RPAREN { ($1, List.rev $3) }

/* Builds pramas in reverse order */
pramas:
  | params COMMA param { $3 :: $1 }
  | param { [$1] }
  | { [] }

param:
  para_indc typespec IDENT { ($1, $2, $3) }

para_indc:
  | VAL { Val }
  | REF { Ref }

typespec:
  | BOOL { Bool }
  | FLOAT { Float }
  | INT { Int }

proc_body:
  decls stmts { { decls = List.rev $1; stmts = List.rev $2 } }

/* Builds decls in reverse order */
decls :
  | decls decl { $2 :: $1 }
  | { [] }  

decl:
  | typespec IDENT SEMICOLON { ($1, $2) }
  | typespec IDENT DIMENSION SEMICOLON { ($1, $2, $3) } /*!!!!!!!!!!!!*/

/* Builds stmts in reverse order */
stmts:
  | stmts stmt { $2 :: $1 }
  | stmt { [$1] }

stmt:
  | atom_stmt SEMICOLON { $1 }
  | comp_stmt { $1 }

atom_stmt:
  | variable ASSIGN rvalue { Assign ($1, $3) }
  | READ lvalue { Read $2 }
  | WRITE expr { Write $2 }
  | IDENT LPAREN exprs RPAREN { Call ($1, List.rev $3) }

variable:
  | IDENT { SingleItem $1 }
  | IDENT LBRACK exprs LBRACK { ArrayItem ($1, List.rev $3) }




/*expr:*/
/*  | BOOL_CONST { Ebool $1 }*/
/*  | INT_CONST { Eint $1 }*/
/*  | FLOAT_CONST { Efloat $1 }*/
/*  | lvalue { Elval $1 }*/
  /* Binary operators */
/*  | expr PLUS expr { Ebinop ($1, Op_add, $3) }*/
/*  | expr MINUS expr { Ebinop ($1, Op_sub, $3) }*/
/*  | expr MULTI expr { $1 * $2 } */
/*  | expr DIVID expr { $1 / $2 } */
/*  | LPAREN expr RPAREN */
/*  | expr EQ expr */
/*  | expr NE expr */
/*  | expr LT expr */
/*  | expr GT expr */
/*  | expr LE expr */
/*  | expr GE expr */
/*  | expr AND expr */
/*  | expr OR expr */
/*  | NOT expr */

/*expr:*/
/*  | BOOL_CONST { Ebool $1 }              */
/*  | INT_CONST { Eint $1 }              */
/*  | lvalue { Elval $1 }              */
/*  | expr PLUS expr { Ebinop ($1, Op_add, $3) }              */
/*  | expr MINUS expr { Ebinop ($1, Op_sub, $3) }              */
/*  | expr MUL expr { Ebinop ($1, Op_mul, $3) }              */
/*  | expr EQ expr { Ebinop ($1, Op_eq, $3) }              */
/*  | expr LT expr { Ebinop ($1, Op_lt, $3) }              */
/*  | MINUS expr %prec UMINUS { Eunop (Op_minus, $2) }              */
/*  | LPAREN expr RPAREN { $2 }              */

/*proc_body:   */
/*    decls stmts { $1 ; $2 }   */

/*decl:   */
/*    | typespec IDENT SEMICOLON   */
/*    | typespec IDENT ASSIGN lvalue SEMICOLON   */
/*    | typespec IDENT dimension ASSIGN lvalue SEMICOLON   */

/*decls:   */
/*    | decls decl { $2 :: $1 }   */
/*    | { [] }   */

/*stmts:   */
/*    | LBRACK stmts stmt RBRACK { $2 :: $1 }   */
/*    | { [] }   */

/*stmt:   */
/*    stmt_body SEMICOLON { $1 }   */

/*stmt_body:   */
/*    | IDENT ASSIGN expr   */
/*    | IDENT dimension ASSIGN expr   */
/*    | READ IDENT   */
/*    | READ IDENT dimension   */
/*    | WRITE expr   */
/*    | IDENT LPAREN expr_list RPAREN   */

/*expr_list:   */
/*    | IF expr THEN stmts FI   */
/*    | IF expr THEN stmts ELSE stmts FI   */
/*    | WHILE expr DO stmts OD   */