
%{ open Ast %}

%token INT DOUBLE BOOL CHAR NA ID ASSIGN LITERAL PLUS MINUS TIMES
%token DIVIDE EQ NEW LT LEQ GT GEQ LPAREN RPAREN NEQ LBRACE RBRACE COLON
%token DLIN COMMA
%token <int> INT 
%token <float> DOUBLE 
%token <bool> BOOL
%token <string> ID
%token <Ast.literal> LITERAL

%nonassoc NOELSE
%nonassoc ELSE
%right ASSIGN
%left EQ NEQ
%left LT GT LEQ GEQ
%left PLUS MINUS
%left TIMES DIVIDE

%start program

%type <Ast.program> program


%%

program : 
 | program stmt { $2  }

 stmt:
    expr DLIN { Expr($1) }


expr:
    LITERAL          { Literal($1) }
  | ID               { Id($1) }
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




