{ 
	open Parser
}

  let dig = ['0'-'9']+
  let whitespace = [' ' '\t']
  rule token = parse
  	whitespace      { token lexbuf } 
  	| ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']* as lxm { ID(lxm) }
  	| ','           { COMMA }
  	| '('           { LPAREN }
	  | ')'           { RPAREN }
  	| dig as lit    { INT(int_of_string lit) }
    | eof           { EOF }
    | '\n'          { DLIN }
    | '+'           { PLUS }
    | '-'           { MINUS }
    | '*'           { MULT }
    | '/'           { DIV }
    | '^'           { EXPO }
    | "<-"          { ASSIGN }
    | "%%"          { MOD }
