
%{ open Ast %}

%token NA ASSIGN PLUS MINUS TIMES EOF IF FOR ELSE NOELSE COLON IN
%token DIVIDE EQ NEQ LT LEQ GT GEQ LPAREN RPAREN LBRACE RBRACE FUNCTION
%token MOD EXP AND OR NOT LBRACK RBRACK
%token NEXT BREAK
%token DLIN COMMA
%token <string> CHARACTER
%token <int> INT 
%token <float> DOUBLE 
%token <bool> BOOL
%token <string> ID

%nonassoc NOELSE 
%nonassoc ELSE
%nonassoc COLON
%right ASSIGN
%left OR
%left AND
%left EQ NEQ
%left LT GT LEQ GEQ
%left PLUS MINUS
%left TIMES DIVIDE MOD
%left EXP
%right NOT

%start program

%type <Ast.program> program

%%

program : 
 | decls EOF { $1 }

decls:
 | /* nothing */ { [],[] }
 | decls stmt    { ($2 :: fst $1), snd $1 }
 | decls fdecl   { fst $1, ($2 :: snd $1) }

fdecl:
 | ID ASSIGN FUNCTION LPAREN formals_opt RPAREN LBRACE stmt_list RBRACE DLIN
    { { fname = $1;
        formals = $5;
        body = List.rev $8 } }

formals_opt:
  | /* nothing */           { [] }
  | formal_list             { List.rev $1 }

formal_list:
  | ID                      { [$1] }
  | formal_list COMMA ID    { $3 :: $1 }

data:
  | BOOL                  { BoolLit($1) }
  | ID                    { Id($1) }
  | CHARACTER             { Character($1) }
  | DOUBLE                { DoubleLit($1) }
  | INT                   { IntLit($1) }
  | NA                    { Na }

stmt:
  | expr DLIN                                       { Expr($1) }
  | LBRACE stmt_list RBRACE                         { Block($2) }
  | LBRACE loop_stmt_list RBRACE                    { Block($2) }
  | IF LPAREN expr RPAREN stmt %prec NOELSE         { If($3, $5, Block([])) }
  | IF LPAREN expr RPAREN stmt ELSE stmt            { If($3, $5, $7) }
  | FOR LPAREN ID IN expr RPAREN loop_block_body    { For($3, $5, $7) } 

expr:
  | data                { $1 }
  | bool_expr           { $1 }
  | arith_expr          { $1 }
  | ID ASSIGN expr      { Assign($1, $3) }
  | LPAREN expr RPAREN  { $2 }
  | ID LBRACK expr COMMA expr RBRACK { MatrixAcc($1, $3, $5)}
  | ID LBRACK COMMA expr RBRACK { MatrixCol($1, $4)}
  | ID LBRACK expr COMMA RBRACK { MatrixRow($1, $3)}

arith_expr:
  | expr COLON  expr    { DualOp($1, Range, $3) }
  | expr PLUS   expr    { DualOp($1, Add,   $3) }
  | expr MINUS  expr    { DualOp($1, Sub,   $3) }
  | expr TIMES  expr    { DualOp($1, Mult,  $3) }
  | expr DIVIDE expr    { DualOp($1, Div,   $3) }
  | expr MOD    expr    { DualOp ($1, Mod, $3) }
  | expr EXP    expr    { DualOp ($1, Exp, $3) }

bool_expr:
  | expr EQ     expr    { DualOp($1, Equal, $3) }
  | expr NEQ    expr    { DualOp($1, Neq,   $3) }
  | expr LT     expr    { DualOp($1, Lthan,  $3) }
  | expr LEQ    expr    { DualOp($1, Leq,   $3) }
  | expr GT     expr    { DualOp($1, Gthan,  $3) } 
  | expr GEQ    expr    { DualOp($1, Geq,   $3) }
  | expr AND    expr    { DualOp ($1, And, $3) }
  | expr OR     expr    { DualOp ($1, Or, $3) }
  | NOT expr            { SingOp (Not, $2) }

loop_block_body: 
  | LBRACE loop_stmt_list RBRACE        { Block(List.rev $2) }

loop_stmt_list:
  | /*nothing*/                       { [] }
  | stmt_list DLIN                    { $1 }
  | loop_stmt_list loop_expr DLIN     { $2 :: $1 }

loop_expr:
  | NEXT                              { Next }
  | BREAK { Break }

stmt_list:
  | stmt                              { [$1] }
  | stmt_list stmt                    { $2 :: $1 }
