
type op = Add | Sub | Mult | Div | Assign | Equal | Neq | Geq | Leq | Gthan | Lthan | Range


type expr =
 	| Int of int
	| Double of float
	| Bool of bool
	| Char of char
	| Na
	| Id of string
	| Binop of expr * op * expr
	| Assign of string * expr
	| Noexpr

type stmt =
	Expr of expr
	| If of expr * stmt * stmt
	| Block of stmt list
	| For of expr * expr * stmt
	| Return of expr

type func_decl = {
    fname : string;
    formals : string list;
    body : stmt list;
  }



type program = stmt list * func_decl list 
