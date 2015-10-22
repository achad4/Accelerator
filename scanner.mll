{ open Parser }

rule token = parse
  [' ' '\t' '\r' (*'\n' = ; in R*)]  { token lexbuf }
| '#'                                { comment lexbuf } (*comment*)
| '('       { LPAREN }      | ')'       { RPAREN }
| '{'       { LBRACE }      | '}'       { RBRACE }
| ';'       { SEMI }        | ','       { COMMA }
| '+'       { PLUS }        | '-'       { MINUS }
| '*'       { TIMES }       | '/'       { DIVIDE }
| "<-"      { ASSIGN }      | "=="      { EQ }
| "!="      { NEQ }         | '<'       { LT }
| "<="      { LEQ }         | '>'       { GT }
| ">="      { GEQ }         
| "else"    { ELSE }        | "if"      { IF }
| "for"     { FOR }         | "int"     { 
