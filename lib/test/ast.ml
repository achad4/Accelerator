
type op = Add | Sub | Mult | Div | Assign | Equal | Neq | Geq | Leq | Gthan | Lthan

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
	| Noexpr

type stmt =
	Expr of expr


type program = stmt 
