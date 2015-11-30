%{ open Ast %}

%token EOF, DLIN, PLUS, MINUS, MULT, DIV, EXPO, MOD
%token LPAREN, RPAREN, COMMA, ASSIGN, AND, OR, NOT
%token <int> INT
%token <string> ID
%token <bool> TRUE FALSE

%left PLUS MINUS 
%left MULT DIV
%left MOD
%left AND OR
%right ASSIGN NOT

%start program

%type <Ast.program> program

%%

program :
	| stmt_list EOF                        { $1 }
    
stmt_list:
  | stmt                                 { [$1] }
  | stmt_list stmt                       { $2 :: $1 }

stmt:
  | expr DLIN                            { Expr($1) }

expr:
  | ID { Id($1) }
	| num_data { $1 }
  | bool_data { $1 }
	| arith_expr                           { $1 }
  | bool_expr                            { $1 }
	| ID LPAREN actuals_opt RPAREN         { FuncCall($1, $3) }
  | ID ASSIGN expr                       { Assign($1, $3) }


actuals_opt:
  | /* nothing */                        { [] }
  | actuals_list                         { List.rev $1 }

 actuals_list:
  | expr                                 { [$1] }
  | actuals_list COMMA expr              { $3 :: $1 }

arith_expr:
  | num_data MULT num_data    { Mult($1, $3) }
  | num_data PLUS num_data    { Add($1, $3) }
  | num_data MINUS num_data   { Sub($1, $3) }
  | num_data DIV num_data     { Div($1, $3) }
  | num_data EXPO num_data    { Expo($1, $3) }
  | num_data MOD num_data     { Mod($1, $3) }

bool_expr:
  | bool_data AND bool_data   { And($1, $3) }
  | bool_data OR bool_data    { Or($1, $3) }
  | NOT bool_data             { Not($2) }

num_data:
  | INT           { IntLit($1) }

bool_data:
  | TRUE          { BoolLit($1) }
  | FALSE         { BoolLit($1) }
