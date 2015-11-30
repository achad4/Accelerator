%{ open Ast %}

%token EOF, DLIN, PLUS, MINUS, MULT, DIV, EXPO, MOD
%token LPAREN, RPAREN, COMMA, ASSIGN, AND, OR, NOT
%token <int> INT
%token <float> FLOAT
%token <string> ID
%token <bool> TRUE FALSE

%left PLUS MINUS 
%left MULT DIV
%left MOD
%left AND OR
%right ASSIGN NOT EXPO

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
  | arith_expr                           { $1 }
  | bool_expr                            { $1 }
  | float_expr                           { $1 }       
  | ID LPAREN actuals_opt RPAREN         { FuncCall($1, $3) }
  | ID ASSIGN expr                       { Assign($1, $3) }

actuals_opt:
  | /* nothing */                        { [] }
  | actuals_list                         { List.rev $1 }

 actuals_list:
  | expr                                 { [$1] }
  | actuals_list COMMA expr              { $3 :: $1 }

arith_expr:
  | num_data                    { $1 }
  | arith_expr MULT arith_expr  { Mult($1, $3) }
  | arith_expr PLUS arith_expr  { Add($1, $3) }
  | arith_expr MINUS arith_expr { Sub($1, $3) }
  | arith_expr DIV arith_expr   { Div($1, $3) }
  | arith_expr EXPO arith_expr  { Expo($1, $3) }
  | arith_expr MOD arith_expr   { Mod($1, $3) }

float_expr:
  | float_data                      { $1 }
  | float_expr MULT float_expr     { FMult($1, $3) }
  | float_expr PLUS float_expr     { FAdd($1, $3) }
  | float_expr MINUS float_expr    { FSub($1, $3) }
  | float_expr DIV float_expr      { FDiv($1,$3) }

bool_expr:
  | bool_data                 { $1 }
  | bool_expr AND bool_expr   { And($1, $3) }
  | bool_expr OR bool_expr    { Or($1, $3) }
  | NOT bool_expr             { Not($2) }

num_data:
  | INT           { IntLit($1) }

bool_data:
  | TRUE          { BoolLit($1) }
  | FALSE         { BoolLit($1) }

float_data:
  | FLOAT         { FloatLit($1) }
