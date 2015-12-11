%{ open Ast %}

%token EOF, DLIN, PLUS, MINUS, MULT, DIV, EXPO, MOD, RBRACE, LBRACE
%token LPAREN, RPAREN, COMMA, ASSIGN, AND, OR, NOT, IF, ELSE, EQ
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
  | expr DLIN                                  { Expr($1) }
  | LBRACE DLIN stmt_list RBRACE DLIN          { Block(List.rev $3) }
  | IF LPAREN bool_expr RPAREN DLIN stmt ELSE DLIN stmt    { If($3, $6, $9) }

expr:
  | ID                                   { Id($1) }
  | int_expr                             { $1 }
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

int_expr:
  | int_data                { $1 }
  | int_expr MULT int_expr  { Mult($1, $3) }
  | int_expr PLUS int_expr  { Add($1, $3) }
  | int_expr MINUS int_expr { Sub($1, $3) }
  | int_expr DIV int_expr   { Div($1, $3) }
  | int_expr EXPO int_expr  { Expo($1, $3) }
  | int_expr MOD int_expr   { Mod($1, $3) }

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

int_data:
  | INT           { IntLit($1) }

bool_data:
  | TRUE          { BoolLit($1) }
  | FALSE         { BoolLit($1) }

float_data:
  | FLOAT         { FloatLit($1) }
