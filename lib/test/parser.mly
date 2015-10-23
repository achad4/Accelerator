
%{ open Ast %}

%token INT DOUBLE BOOL CHAR NA ID ASSIGN LITERAL PLUS MINUS TIMES
%token DIVIDE EQ NEW LT LEQ GT GEQ LPAREN RPAREN NEQ LBRACE RBRACE COLON
%token DLIN COMMA

%nonassoc NOELSE
%nonassoc ELSE
%right ASSIGN
%left EQ NEQ
%left LT GT LEQ GEQ
%left PLUS MINUS
%left TIMES DIVIDE

%start s

%type <int>INT 
%type <float>DOUBLE 
%type <bool>BOOL
%type <int> s
%type <char> ID

%%

s : expr { 0 }


expr:
    INT          { Literal($1) }
  | ID               { 0 }
  | expr PLUS   expr { 0 }
  | expr MINUS  expr { 0 }
  | expr TIMES  expr { 0 }
  | expr DIVIDE expr { 0 }
  | expr EQ     expr { 0 }
  | expr NEQ    expr { 0 }
  | expr LT     expr { 0 }
  | expr LEQ    expr { 0 }
  | expr GT     expr { 0 }
  | expr GEQ    expr { 0 }
  | ID ASSIGN expr   { 0 }
  | LPAREN expr RPAREN { 0 }




