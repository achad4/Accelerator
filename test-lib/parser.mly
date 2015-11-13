%{ open Ast %}

%token EOF, DLIN, PLUS
%token <int> INT

%left PLUS 

%start program

%type <Ast.program> program

%%

program : 
	stmt EOF 	  { Stmt($1) }

stmt:
  | expr DLIN     { Expr($1) }

expr:
	data { $1 }
	| arith_expr                                      { $1 }

arith_expr:
  | data PLUS data    { Binop($1, Add, $3) }	


data:
  | INT           { IntLit($1) }

