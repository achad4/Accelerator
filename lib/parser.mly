
%{ open Ast %}

%token NA ASSIGN PLUS MINUS TIMES EOF IF FOR ELSE NOELSE COLON IN
%token DIVIDE EQ NEQ LT LEQ GT GEQ LPAREN RPAREN LBRACE RBRACE FUNCTION
%token DLIN COMMA
%token <char> CHAR
%token <int> INT 
%token <float> DOUBLE 
%token <bool> BOOL
%token <string> ID

%nonassoc NOELSE
%nonassoc ELSE
%nonassoc COLON
%right ASSIGN
%left EQ NEQ
%left LT GT LEQ GEQ
%left PLUS MINUS
%left TIMES DIVIDE

%start program

%type <Ast.program> program


%%

program : 
 | decls EOF { $1 }

decls:
  /* nothing */ { [],[] }
| decls stmt { ($2 :: fst $1), snd $1 }
| decls fdecl { fst $1, ($2 :: snd $1) }

fdecl:
    ID ASSIGN FUNCTION LPAREN formals_opt RPAREN LBRACE stmt_list RBRACE DLIN
    { { fname = $1;
        formals = $5;
        body = List.rev $8 } }

formals_opt:
    /* nothing */       { [] }
  | formal_list         { List.rev $1 }

formal_list:
    ID                      { [$1] }
  | formal_list COMMA ID    { $3 :: $1 }

stmt_list:
    /* nothing */           { [] }
  | stmt_list stmt          { $2 :: $1 }

data:
  | BOOL             { BoolLit($1) }
  | CHAR             { CharLit($1) }
  | ID               { Id($1) }
  | DOUBLE           { DoubleLit($1) }
  | INT              { IntLit($1) }
  | NA               { Na }

stmt:
    expr DLIN                   { Return($1) }
  | LBRACE stmt_list RBRACE     { Block($2) }
  | IF LPAREN expr RPAREN stmt %prec NOELSE { If($3, $5, Block([])) }
  | IF LPAREN expr RPAREN stmt ELSE stmt  { If($3, $5, $7) }
  | FOR LPAREN expr IN expr RPAREN LBRACE stmt RBRACE { For($3,$5, $8) } 

expr:
    data             { $1 }
  | expr COLON  expr { Binop($1, Range, $3) }
  | expr PLUS   expr { Binop($1, Add,   $3) }
  | expr MINUS  expr { Binop($1, Sub,   $3) }
  | expr TIMES  expr { Binop($1, Mult,  $3) }
  | expr DIVIDE expr { Binop($1, Div,   $3) }
  | expr EQ     expr { Binop($1, Equal, $3) }
  | expr NEQ    expr { Binop($1, Neq,   $3) }
  | expr LT     expr { Binop($1, Lthan,  $3) }
  | expr LEQ    expr { Binop($1, Leq,   $3) }
  | expr GT     expr { Binop($1, Gthan,  $3) }
  | expr GEQ    expr { Binop($1, Geq,   $3) }
  | ID ASSIGN expr   { Assign($1, $3) }
  | LPAREN expr RPAREN { $2 }




