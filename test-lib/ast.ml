type op = 
  | Eq
  | Neq
  | Add
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
  | Na
  | Id of string
	| IntLit of int
  | BoolLit of bool
  | FloatLit of float
  | StringLit of string
  | Vector of string * expr list
  | VectAcc of string * expr
  | Matrix of string * expr list * expr * expr
  | MatrixAcc of string * expr * expr
  | Eq of expr * expr
  | Neq of expr * expr
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
  | FormalDef of string * expr

type stmt = 
	| Expr of expr
  | Block of stmt list
  | If of expr * stmt * stmt
  | For of string * expr * expr * stmt
  | While of expr * stmt
  | Return of expr

type func_def = 
  | FunctionDef of string * expr list * stmt

type program = func_def list * stmt list

