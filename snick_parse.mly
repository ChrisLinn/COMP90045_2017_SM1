/*
** File:          snick_ast.ml
** Description:   Specification of a parser for Snick
** Last Modified: Sun. 9th April 2017
** 
** Group name: Mainframe
** 
** Member names   | usernames
** Xianzhuo REN   | xianzhuor 
** Haoyu LIN      | haoyul3
** Zequn MA       | zequnm
*/

/* ocamlyacc parser for snick */
%{
open Snick_ast
%}

/* tokens */
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
%token <string> STRING_CONST
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
%token LSQBRACK RSQBRACK
%token LPAREN RPAREN

/* associativity */
%left OR
%left AND
%nonassoc NOT
%nonassoc EQ NE LT GT LE GE
%left PLUS MINUS
%left MULTI DIVID
%nonassoc UMINUS

%type <Snick_ast.program> program

%start program
%%

program:
    procs   { List.rev $1 }

/* Builds procs in reverse order */
procs:
    | procs proc { $2 :: $1 }
    | proc { [$1] }

proc:
    PROC proc_header proc_body END { ($2, $3) }

proc_header:
    IDENT LPAREN params RPAREN { ($1, List.rev $3) }

/* Builds params in reverse order */
params:
    | params COMMA param { $3 :: $1 }
    | param { [$1] }
    | { [] }

param:
    param_indc typespec IDENT { ($1, $2, $3) }

param_indc:
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
    | typespec variable SEMICOLON { ($1, $2) }

variable:
    | IDENT { Variable ($1, None) }
    | IDENT dimension { Variable ($1, Some $2) }
/*    | IDENT { Single_variable $1 }  */
/*    | IDENT dimension { Array_variable ($1, $2) }   */

dimension:
    LSQBRACK intervals RSQBRACK { List.rev $2 }
  
/* Builds intervals in reverse order */
intervals:
    | intervals COMMA interval { $3 :: $1 }
    | interval { [$1] }

interval:
    INT_CONST DOT DOT INT_CONST { ($1, $4) }

/* Builds stmts in reverse order */
stmts:
    | stmts stmt { $2 :: $1 }
    | stmt { [$1] }

/* stmt: */
/*     | atom_stmt { Atom_stmt $1 } */
/*     | comps_stmt { Comps_stmt $1 } */

/* atom_stmt: */
/*     | elem ASSIGN expr SEMICOLON { Assign ($1, $3) } */
/*     | READ elem SEMICOLON { Read $2 } */
/*     | WRITE STRING_CONST SEMICOLON { Write (String $2) } */
/*     | WRITE expr SEMICOLON { Write (Expr $2) } */
/*     | IDENT LPAREN exprs_emptiable RPAREN SEMICOLON { Call ($1, List.rev $3) } */

/* comps_stmt: */
/*     | IF expr THEN stmts FI { If_then ($2, List.rev $4) } */
/*     | IF expr THEN stmts ELSE stmts FI { If_then_else ($2, List.rev $4, List.rev $6) } */
/*     | WHILE expr DO stmts OD { While ($2, List.rev $4) } */

stmt:
    | elem ASSIGN expr SEMICOLON { Assign ($1, $3) }
    | READ elem SEMICOLON { Read $2 }
    | WRITE STRING_CONST SEMICOLON { Write (String $2) }
    | WRITE expr SEMICOLON { Write (Expr $2) }
    | IDENT LPAREN exprs_emptiable RPAREN SEMICOLON { Call ($1, List.rev $3) }
    | IF expr THEN stmts FI { If_then ($2, List.rev $4) }
    | IF expr THEN stmts ELSE stmts FI { If_then_else ($2, List.rev $4, List.rev $6) }
    | WHILE expr DO stmts OD { While ($2, List.rev $4) }

elem:
    | IDENT { Elem ($1, None) }
    | IDENT LSQBRACK exprs RSQBRACK { Elem ($1, Some (List.rev $3)) }
/*    | IDENT { Single_elem $1 }    */
/*    | IDENT LSQBRACK exprs RSQBRACK { Array_elem ($1, List.rev $3) }*/

expr:
    /* Variable element */
    | elem { Eelem $1 }
    /* Constants */
    | BOOL_CONST { Ebool $1 }
    | INT_CONST { Eint $1 }
    | FLOAT_CONST { Efloat $1 }
    /* Expression inside a pair of  parentheses */
    | LPAREN expr RPAREN { Eparen $2 }
    /* Binary operators */
    | expr PLUS expr { Ebinop ($1, Op_add, $3) }
    | expr MINUS expr { Ebinop ($1, Op_sub, $3) }
    | expr MULTI expr { Ebinop ($1, Op_mul, $3) }
    | expr DIVID expr { Ebinop ($1, Op_div, $3) }
    | expr EQ expr { Ebinop ($1, Op_eq, $3) }
    | expr NE expr { Ebinop ($1, Op_ne, $3) }
    | expr LT expr { Ebinop ($1, Op_lt, $3) }
    | expr GT expr { Ebinop ($1, Op_gt, $3) }
    | expr LE expr { Ebinop ($1, Op_le, $3) }
    | expr GE expr { Ebinop ($1, Op_ge, $3) }
    | expr AND expr { Ebinop ($1, Op_and, $3) }
    | expr OR expr { Ebinop ($1, Op_or, $3) }
    /* Unary operators */
    | NOT expr { Eunop (Op_not, $2) }
    | MINUS expr %prec UMINUS { Eunop (Op_minus, $2) }


/* Builds exprs in reverse order */
exprs:
    | exprs COMMA expr { $3 :: $1 }
    | expr { [$1] }

exprs_emptiable:
    | exprs COMMA expr { $3 :: $1 }
    | expr { [$1] }
    | { [] }