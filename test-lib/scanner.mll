{ 
	open Parser
  open Lexing
}

  let dig = ['-']?['0'-'9']
  let frac = '.' dig*
  let whitespace = [' ' '\t']
  let flt = dig* frac
  rule token = parse
  	| whitespace    { token lexbuf } 
    | "NA"          { NA }
    | "true" as lit { TRUE(bool_of_string lit) }
    | "false" as lit{ FALSE(bool_of_string lit) }
    | "if"          { IF }
    | "else"        { ELSE }
    | "for"         { FOR }
    | ":"           { RANGE }
    | "function"    { FUNCTION }
    | "return"      { RETURN }
    | "c("           { VECTSTART }
    | "matrix("      { MATRIXSTART }
    | "nrow"        { NROW }
    | "ncol"        { NCOL }
    | "="           { EQSING }
    | "in"          { IN }
  	| ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']* as lxm { ID(lxm) }
    | '\"' [^'\"']* '\"' { let str = lexeme lexbuf in
                           STRING (String.sub str 1 (String.length str - 2)) }
  	| ','           { COMMA }
  	| '('           { LPAREN }
	  | ')'           { RPAREN }
    | '{'           { LBRACE }
    | '}'           { RBRACE }
  	| dig+ as lit   { INT(int_of_string lit) }
    | flt as lit    { FLOAT(float_of_string lit) }
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
    | "=="          { EQ }
    | '['           { LBRAC }
    | ']'           { RBRAC }
    | '"'           { DOUBLEQT }
    | eof           { EOF } 