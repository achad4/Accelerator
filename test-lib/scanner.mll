{ 
	open Parser
}

  let dig = ['0'-'9']+
  let whitespace = [' ' '\t']
  rule token = parse
  	whitespace { (*print_endline "WHITE";*) token lexbuf } 
  	| dig as lit    { (*print_endline "INT";*) INT(int_of_string lit) }
    | eof { (*print_endline "EOF";*) EOF }
    | '\n'     { (*print_endline "DLIN";*) DLIN }
    | '+'      { (*print_endline "+";*) PLUS }
