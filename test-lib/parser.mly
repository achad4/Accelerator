%{ open Ast %}

%token EOF, DLIN, PLUS, MINUS, MULT, DIV, EXPO, MOD
%token LPAREN, RPAREN, COMMA, ASSIGN
%token <int> INT
%token <string> ID

%left PLUS MINUS 
%left MULT DIV
%left MOD
%right ASSIGN

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
	| data                                 { $1 }
	| arith_expr                           { $1 }
	| ID LPAREN actuals_opt RPAREN         { FuncCall($1, $3) }
  | ID ASSIGN expr                       { Assign($1, $3) }

actuals_opt:
  | /* nothing */                        { [] }
  | actuals_list                         { List.rev $1 }

 actuals_list:
  | expr                                 { [$1] }
  | actuals_list COMMA expr              { $3 :: $1 }

arith_expr:
  | data MULT data                       { Mult($1, $3) }
  | data PLUS data                       { Add($1, $3) }
  | data MINUS data                      { Sub($1, $3) }
  | data DIV data                        { Div($1, $3) }
  | data EXPO data                       { Expo($1, $3) }
  | data MOD data                        { Mod($1, $3) }

data:
  | INT                                  { IntLit($1) }

