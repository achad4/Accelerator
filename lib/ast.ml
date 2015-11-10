
type dual_op = Add | Sub | Mult | Div | Mod | Exp | Assign | Equal | Neq | Geq | Leq | Gthan | Lthan | Range | And | Or
type sing_op = Not

type vector = 
	| IntVector of int list
	| DoubleVector of float list
	| BoolVector of bool list
	| CharacterVector of string list
	| CharVector of char list

type matrix =
	| Matrix of vector list

type expr =
 	| IntLit of int
	| DoubleLit of float
	| BoolLit of bool
	| Character of string
	| Na
	| Id of string
	| DualOp of expr * dual_op * expr
	| SingOp of sing_op * expr
	| Assign of string * expr
	| MatrixAccElem of string * int * int
	| MatrixAccRow of string * int
    | MatrixAccCol of string * int
    | FuncCal of string * expr list
	| Noexpr

type stmt =
	| Expr of expr
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
