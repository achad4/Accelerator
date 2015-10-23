
type operator = Add | Sub | Mul | Div | Assign | Geq | Leq | Gthan | Lthan

type Literal = 
	 IntLit of int
	| DoubleLit of float
	| BoolLit of bool
	| CharLit of char
	| NALit of null

type expr =
 	  Literal
	| Id of string
	| Binop of expr * op * expr
	| Assign of string * expr
	| Call of string * expr list
	| Noexpr