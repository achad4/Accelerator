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
  let charvector = "c(" whitespace* (whitespace* char whitespace* ',')*(whitespace* char whitespace*)

  let int = dig+
  let double =  sign dig * '.' ((dig+exp) | (dig+) | (exp))+

  rule token = parse 
    [' ' '\t' '\r' '\n'] { token lexbuf } (* Whitespace *)
	| '('      { print_endline "LPAREN"; LPAREN }
	| ')'      { print_endline "RPAREN"; RPAREN }
	| '{'      { print_endline "LBRACE"; LBRACE }
	| '}'      { print_endline "RPAREN"; RBRACE }
	| ':'	   { print_endline "COLON"; COLON }
	| '\n'     { print_endline "DLIN"; DLIN }
	| ','      { print_endline "COMMA"; COMMA }
	| '+'      { print_endline "PLUS"; PLUS }
	| '-'      { print_endline "MINUS"; MINUS }
	| '*'      { print_endline "TIMES"; TIMES }
	| '/'      { print_endline "DIVIDE"; DIVIDE }
	| "<-"     { print_endline "ASSIGN"; ASSIGN }
	| "=="     { print_endline "EQ"; EQ }
	| "!="     { print_endline "NEQ"; NEQ }
	| '<'      { print_endline "LT"; LT }
	| "<="     { print_endline "LEQ"; LEQ }
	| ">"      { print_endline "GT"; GT }
	| ">="     { print_endline "GEQ"; GEQ }
	| ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']* as lxm { print_endline "ID"; ID(lxm) }
	| dig+ as lit { print_endline "INT"; INT(int_of_string lit) }
	| sign? dig * '.' ((dig+exp) | (dig+) | (exp))+ as lit { print_endline "DOUBLE"; DOUBLE(float_of_string lit) }
	| bool 	as lit	{ print_endline "DOUBLE"; BOOL(lit = "TRUE") }
	| char      { print_endline "CHAR"; CHAR }



{
	let lexbuf = Lexing.from_channel stdin in
	token lexbuf;;
}