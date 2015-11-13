{ 
	open Parser
}
   
  let whitespace = [' ' '\t']
  let sign = ['+''-']
  let dig = ['0'-'9']
  let frac = '.'['0'-'9']+
  let exp = ['e''E']['+''-']?['0'-'9']+
  let bool = "TRUE" | "FALSE"
  let character = ['\"']_+['\"']
  
  let int = dig+
  let double =  sign dig * '.' ((dig+exp) | (dig+) | (exp))+

  rule token = parse 
    whitespace { token lexbuf }
	| '('      { LPAREN }
	| ')'      { RPAREN }
	| '{'      { LBRACE }
	| '}'      { RBRACE }
	| '['	   { LBRACK }
	| ']'	   { RBRACK }
	| ':'	   { COLON }
	| '\n'     { DLIN }
	| ','      { COMMA }
	| '+'      { PLUS }
	| '-'      { MINUS }
	| '*'      { TIMES }
	| '/'      { DIVIDE }
	| "||"	   { OR }
	| "&&"     { AND }
	| '!'      { NOT }
	| '^'	   { EXP }
	| "%%"	   { MOD }
	| "<-"     { ASSIGN }
	| "=="     { EQ }
	| "!="     { NEQ }
	| '<'      { LT }
	| "<="     { LEQ }
	| '>'      { GT }
	| ">="     { GEQ }
	| '#'	   { comment lexbuf }
	| eof { EOF }
	| ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']* as lxm { ID(lxm) }
	| int as lit    { INT (int_of_string lit) }
	| double as lit { DOUBLE (float_of_string lit) }
	| bool 	as lit	{ BOOL (lit = "TRUE") }
	| character as lit   { CHARACTER (lit) }
	| "NA"			{ NA }
	| "function" 	{ FUNCTION }
	| "in" 			{ IN }
	| "next"		{ NEXT }
	| "break"		{ BREAK }
	| _ as char { raise (Failure("Illegal character " ^ Char.escaped char)) }

and comment = parse
	| '\n'	{ token lexbuf }
	| _ { comment lexbuf }

{
	let lexbuf = Lexing.from_channel stdin in
	token lexbuf;;
}