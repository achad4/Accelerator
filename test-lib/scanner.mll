{ 
	open Parser
}

  let dig = ['0'-'9']
  let frac = '.' digit*
  let whitespace = [' ' '\t']
  let flt = dig* frac?
  rule token = parse
  	| whitespace    { token lexbuf } 
    | "true" as lit { TRUE(bool_of_string lit) }
    | "false" as lit{ FALSE(bool_of_string lit) }
  	| ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']* as lxm { ID(lxm) }
  	| ','           { COMMA }
  	| '('           { LPAREN }
	| ')'           { RPAREN }
  	| dig+ as lit    { INT(int_of_string lit) }
    | flt as lit    { FLOAT(float_of_string lit) }
    | eof           { EOF }
    | '\n'          { DLIN }
    | '+'           { PLUS }
    | '-'           { MINUS }
    | '*'           { MULT }
    | '/'           { DIV }
    | '^'           { EXPO }
    | "<-"          { ASSIGN }
    | "%%"          { MOD }
    | "&&"          { AND }
    | "||"          { OR }
    | '!'           { NOT }
