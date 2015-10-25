
type op = Add | Sub | Mult | Div | Assign | Equal | Neq | Geq | Leq | Gthan | Lthan | Range

type vector = 
	  IntVector of int list
	| DoubleVector of float list
	| BoolVector of bool list
	| CharVector of char list

type matrix =
	 Matrix of vector list

type expr =
 	  IntLit of int
	| DoubleLit of float
	| BoolLit of bool
	| CharLit of char
	| Na
	| Id of string
	| Binop of expr * op * expr
	| Assign of string * expr
	| Noexpr

type stmt =
	  Expr of expr
	| If of expr * stmt * stmt
	| Block of stmt list
	| For of string * expr * stmt
	| Next
	| Break

type func_decl = {
    fname : string;
    formals : string list;
    body : stmt list;
  }

type program = stmt list * func_decl list 
