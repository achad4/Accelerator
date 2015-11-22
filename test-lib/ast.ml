type op = 
    Add
    | Sub
    | Mult
    | Div
    | Expo

type expr =
 	IntLit of int
 	| Add of expr * expr
    | Sub of expr * expr
    | Mult of expr * expr
    | Div of expr * expr
    | Expo of expr * expr
 	| FuncCall of string * expr list 
 	| Assign of string * expr

type stmt = 
	Expr of expr

type program = stmt list

let rec string_of_expression = function
	IntLit(e) -> string_of_int e
	| Add(e1, e2) -> (string_of_expression e1) ^ "+" ^ (string_of_expression e2)
    | Sub(e1, e2 ) -> (string_of_expression e1) ^ "-" ^ (string_of_expression e2)
    | Mult(e1, e2) -> (string_of_expression e1) ^ "*" ^ (string_of_expression e2)
    | Div(e1, e2) -> (string_of_expression e1) ^ "/" ^ (string_of_expression e2)
    | Expo(e1, e2) -> (string_of_expression e1) ^ "^" ^ (string_of_expression e2)
    | FuncCall( s, e ) -> "function call " ^ s
    | Assign( s, e ) -> "assign " ^ s ^ " = " ^ (string_of_expression e)

let string_of_statement = function
	Expr(e) -> string_of_expression e

let string_of_program program =
	String.concat "\n" (List.map string_of_statement program)
