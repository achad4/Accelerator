{ 
	open Parser
}

   
  let whitespace = [' ' '\t']
  let sign = ['+''-']
  let dig = ['0'-'9']
  let frac = '.'['0'-'9']+
  let exp = ['e''E']['+''-']?['0'-'9']+
  let bool = "TRUE" | "FALSE"
  let char = ['\'']_['\'']
  
  let int = dig+
  let double =  sign dig * '.' ((dig+exp) | (dig+) | (exp))+


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
	| ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']* as lxm { ID(lxm) }
	| int as lit { INT(int_of_string lit) }
	| double as lit { DOUBLE(float_of_string lit) }
	| bool 	as lit	{ BOOL(lit = "TRUE") }
	| char as lit   {  CHAR (String.get lit 0) }
	| "function" 	{  FUNCTION }
	| "in" 			{  IN }
	| "next"		{ NEXT }
	| "break"		{ BREAK }



{
	let lexbuf = Lexing.from_channel stdin in
	token lexbuf;;
}