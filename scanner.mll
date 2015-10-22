{ 
	open Parser 
}

let sign = ['+'|'-']?
let whitespace = [' ' '\t']
let dig = ['0'-'9']
let frac = '.'['0'-'9']+
let exp = ['e''E']['+''-']?['0'-'9']+
let bool = "TRUE" | "FALSE"
let char = ["\'"][.]["\'"]
let charvector = ["c("]whitespace*[whitespace* char whitespace* ',']*[whitespace* char whitespace*]

rule token = parse
  [' ' '\t' '\r' '\n'] { token lexbuf } (* Whitespace *)
| "/*"     { comment lexbuf }           (* Comments *)
| '('      { LPAREN }
| ')'      { RPAREN }
| '{'      { LBRACE }
| '}'      { RBRACE }
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
| "if"     { IF }
| "else"   { ELSE }
| "for"    { FOR }
| "while"  { WHILE }
| "return" { RETURN }
| "int"    { INT }
| sign dig * '.' ((dig+exp) | (dig+) | (exp))+ { DOUBLE }
| bool 		{ BOOL }
| char      { CHAR }
| "NA"      { NA }
| ['A'-'Z'|'a'-'z']
| ['+'|'-']?['1'-'9']+'.'
| ['0'-'9']+ as lxm { LITERAL(int_of_string lxm) }
| ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']* as lxm { ID(lxm) }
| eof { EOF }
| _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

and comment = parse
  "*/" { token lexbuf }
| _    { comment lexbuf }

{
	let lexbuf = Lexing.from_channel stdin in
	token lexbuf;;
}
 
