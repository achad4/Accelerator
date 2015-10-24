
type op = Add | Sub | Mult | Div | Assign | Equal | Neq | Geq | Leq | Gthan | Lthan | Range

type literal = 
	  IntLit of int
	| DoubleLit of float
	| BoolLit of bool
	| CharLit of char
	| NALit 

type expr =
 	  Literal of literal
	| Id of string
	| Bool of bool
	| Char of char
	| Double of float
	| Binop of expr * op * expr
	| Assign of string * expr
	| Noexpr


type stmt =
	Expr of expr

type func_decl = {
    fname : string;
    formals : string list;
    locals : string list;
    body : stmt list;
  }



type program = stmt list * func_decl list 
