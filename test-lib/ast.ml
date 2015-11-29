type op = 
    Add
    | Sub
    | Mult
    | Div
    | Expo
    | Mod
    | Assign
    | And
    | Or
    | Not

type expr =
 	IntLit of int
    | BoolLit of bool
 	| Add of expr * expr
    | Sub of expr * expr
    | Mult of expr * expr
    | Div of expr * expr
    | Expo of expr * expr
    | Mod of expr * expr
 	| FuncCall of string * expr list 
 	| Assign of string * expr
    | And of expr * expr
    | Or of expr * expr
    | Not of expr

type stmt = 
	Expr of expr

type program = stmt list

let rec string_of_expression = function
	IntLit(e) -> string_of_int e
    | BoolLit(b) -> string_of_bool b
	| Add(e1, e2) -> (string_of_expression e1) ^ "+" ^ (string_of_expression e2)
    | Sub(e1, e2 ) -> (string_of_expression e1) ^ "-" ^ (string_of_expression e2)
    | Mult(e1, e2) -> (string_of_expression e1) ^ "*" ^ (string_of_expression e2)
    | Div(e1, e2) -> (string_of_expression e1) ^ "/" ^ (string_of_expression e2)
    | Expo(e1, e2) -> (string_of_expression e1) ^ "^" ^ (string_of_expression e2)
    | Mod(e1, e2) -> (string_of_expression e1) ^ "%" ^ (string_of_expression e2)
    | FuncCall( s, e ) -> "function call " ^ s
    | Assign( s, e ) -> "assign " ^ s ^ " = " ^ (string_of_expression e)
    | And( b1, b2) -> (string_of_expression b1) ^ " && " ^ (string_of_expression b2)
    | Or( b1, b2) -> (string_of_expression b1) ^ " || " ^ (string_of_expression b2)
    | Not( b1 ) -> "!" ^ (string_of_expression b1)
    

let string_of_statement = function
	Expr(e) -> string_of_expression e

let string_of_program program =
	String.concat "\n" (List.map string_of_statement program)
