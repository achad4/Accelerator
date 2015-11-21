type op = Add

type expr =
 	IntLit of int
 	| Add of expr * expr
 	| FuncCall of string * expr list 
 	| Assign of string * expr

type stmt = 
	Expr of expr

type program = stmt list




let rec string_of_expression = function
	IntLit(e) -> string_of_int e
	| Add(e1, e2) -> (string_of_expression e1) ^ "+" ^ (string_of_expression e2)

let string_of_statement = function
	Expr(e) -> string_of_expression e

let string_of_program program =
	String.concat "\n" (List.map string_of_statement program)