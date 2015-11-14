%{ open Ast %}

%token EOF, DLIN, PLUS
%token <int> INT

%left PLUS 

%start program

%type <Ast.program> program

%%

program :
	| stmt_list EOF { $1 }

stmt_list:
    /* nothing */  { [] }
  | stmt_list stmt { $2 :: $1 }

stmt:
  | expr DLIN     { Expr($1) }

expr:
	data { $1 }
	| arith_expr                                      { $1 }

arith_expr:
  | data PLUS data    { Add($1, $3) }	


data:
  | INT           { IntLit($1) }

