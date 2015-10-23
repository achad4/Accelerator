{ 
	open Parser
}

   
  let whitespace = [' ' '\t']
  let sign = ['+''-']
  let dig = ['0'-'9']
  let frac = '.'['0'-'9']+
  let exp = ['e''E']['+''-']?['0'-'9']+
  let bool = "TRUE" | "FALSE"
  let char = ['\'']['.']['\'']
  let charvector = "c(" whitespace* (whitespace* char whitespace* ',')*(whitespace* char whitespace*)

  rule token = parse 
    [' ' '\t' '\r' '\n'] { token lexbuf } (* Whitespace *)
	| '('      { LPAREN }
	| ')'      { RPAREN }
	| '{'      { LBRACE }
	| '}'      { RBRACE }
	| ':'	   { COLON }
	| '\n'     { DLIN }
	| ','      { COMMA }
	| '+'      { PLUS }
	| '-'      { MINUS }
	| '*'      { TIMES }
	| '/'      { DIVIDE }
	| "<-"     { ASSIGN }
	| "=="     { EQ }
	| "!="     { NEQ }
	| '<'      { LT }
	| "<="     { LEQ }
	| ">"      { GT }
	| ">="     { GEQ }
	| dig+ as lit { INT(int_of_string lit) }
	| sign dig * '.' ((dig+exp) | (dig+) | (exp))+ as lit { DOUBLE(float_of_string lit) }
	| bool 	as lit	{ BOOL(lit = "TRUE") }
	| char      { CHAR }


{
	let lexbuf = Lexing.from_channel stdin in
	token lexbuf;;
}