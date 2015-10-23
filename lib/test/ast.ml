
type op = Add | Sub | Mul | Div | Assign | Geq | Leq | Gthan | Lthan

let null l = (l = [])
type literal = 
	  IntLit of int
	| DoubleLit of float
	| BoolLit of bool
	| CharLit of char
	| NALit 


type expr =
 	  Literal of literal
	| Id of string
	| Binop of expr * op * expr
	| Assign of string * expr
	| Call of string * expr list
	| Noexpr

