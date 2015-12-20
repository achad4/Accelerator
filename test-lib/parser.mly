%{ open Ast %}

%token EOF, DLIN, PLUS, MINUS, MULT, DIV, EXPO, MOD, RBRACE, LBRACE, NA
%token LPAREN, RPAREN, COMMA, ASSIGN, AND, OR, NOT, IF, ELSE, VECTSTART
%token MATRIXSTART, NROW, NCOL, EQSING, EQ, FOR, IN, RANGE, LBRAC, RBRAC
%token DOUBLEQT, FUNCTION, RETURN
%token <int> INT
%token <float> FLOAT
%token <string> ID
%token <string> STRING
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
  | EOF                                                {([],[])}
  | func_def_list                                      { ($1, []) }
  | stmt_list                                          { ([], $1) }
  | func_def_list stmt_list EOF                        { ($1, $2) }
    
stmt_list:
  | stmt                                 { [$1] }
  | stmt_list stmt                       { $2 :: $1 }

func_stmt_list:
  | func_stmt                                 { [$1] }
  | func_stmt_list func_stmt                       { $2 :: $1 }

block:
  | LBRACE DLIN stmt_list RBRACE DLIN                        { Block(List.rev $3) }

func_block:
  | LBRACE DLIN func_stmt_list RBRACE DLIN                        { Block(List.rev $3) }

stmt:
  | expr DLIN                                                { Expr($1) }
/*  | LBRACE DLIN stmt_list stmt RBRACE DLIN                   { ReturnBlock(List.rev $3, $4) } */
  | IF LPAREN bool_expr RPAREN DLIN block ELSE DLIN block      { If($3, $6, $9) }
  | FOR LPAREN ID IN expr RANGE expr RPAREN block             { For($3, $5, $7, $9) }

func_def_list:
  | func_def                                 { [$1] }
  | func_def_list func_def                       { $2 :: $1 }

func_def:
   | ID ASSIGN FUNCTION LPAREN formals_opt RPAREN func_block { print_endline "func def"; FunctionDef($1, $5, $7) }

func_stmt:
  | stmt                                          { $1 }
  | RETURN expr DLIN                              { Return($2) }


expr:
  | ID                                                       { Id($1) }
  | expr MULT expr  { Mult($1, $3) }
  | expr PLUS expr  { Add($1, $3) }
  | expr MINUS expr { Sub($1, $3) }
  | expr DIV expr   { Div($1, $3) }
  | expr EXPO expr  { Expo($1, $3) }
  | expr MOD expr   { Mod($1, $3) }
  | literal                                                  { $1 }
  | bool_expr                                                { $1 }
  | string_expr                                              { $1 }
  | ID ASSIGN VECTSTART LPAREN vect_opt RPAREN               { Vector($1, $5) }
  | ID ASSIGN MATRIXSTART LPAREN VECTSTART vect_opt RPAREN COMMA 
    NROW EQSING expr COMMA NCOL EQSING expr RPAREN           { Matrix($1, $6, $11, $15) }
  | ID LPAREN actuals_opt RPAREN                             { FuncCall($1, $3) }
  | ID ASSIGN expr                                           { Assign($1, $3) }
  | ID LBRAC expr RBRAC                                      { VectAcc($1, $3) }
  | ID LBRAC expr RBRAC LBRAC expr RBRAC                     { MatrixAcc($1, $3, $6) }

vect_opt:
  | /* nothing */                        { [] }
  | lits                             { List.rev $1 }
  | bool_lits                            { List.rev $1 }

literal:
  | INT                             { IntLit($1) }
  | FLOAT                           { FloatLit($1) } 

bool_literal:
  | TRUE                            { BoolLit(true) } 
  | FALSE                           { BoolLit(false) } 

lits:  
  | literal                             { [$1] }
  | lits COMMA literal                  { $3 :: $1 }
  | lits COMMA NA                       { Na :: $1 }

bool_lits:
  | bool_expr                            { [$1] }
  | bool_lits COMMA bool_expr            { $3 :: $1 }
  | bool_lits COMMA NA                   { Na :: $1 }

formals_opt:
  | /* nothing */                        { [] }
  | formals_list                         { List.rev $1 }

formals_list:
  | formal_def                           { [$1] }
  | formals_list COMMA formal_def        { $3 :: $1 }

formal_def:
  | ID EQSING expr                       { FormalDef($1, $3) }
 
actuals_opt:
  | /* nothing */                        { [] }
  | actuals_list                         { List.rev $1 }

 actuals_list:
  | expr                                 { [$1] }
  | actuals_list COMMA expr              { $3 :: $1 }


bool_expr:
  | bool_literal                 { $1 }
  | bool_expr AND bool_expr   { And($1, $3) }
  | bool_expr OR bool_expr    { Or($1, $3) }
  | NOT bool_expr             { Not($2) }

string_expr:
  | string_data   { $1 }

string_data:
  | STRING        { StringLit($1) }

