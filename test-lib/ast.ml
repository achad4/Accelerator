type op = Add

type expr =
 	IntLit of int
 	| Binop of expr * op * expr

type stmt = 
	Expr of expr

type program =
    Stmt of stmt


let rec string_of_expression = function
	IntLit(e) -> string_of_int e
	| Binop(e1, op, e2) -> (string_of_expression e1) ^ "+" ^ (string_of_expression e2)

let string_of_statement = function
	Expr(e) -> string_of_expression e

let string_of_program = function
	Stmt(s) -> string_of_statement s