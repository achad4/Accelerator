type op = 
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

type t = 
  | String
  | Int
  | Bool
  | Na

type id = 
  | Id of string

type expr_detail = 
   | IdLit of string
   | IntLit of int
   | BoolLit of bool
   | Add of expr_detail * expr_detail
   | Sub of expr_detail * expr_detail
   | Mult of expr_detail * expr_detail
   | Div of expr_detail * expr_detail
   | Expo of expr_detail * expr_detail
   | Mod of expr_detail * expr_detail
   | FuncCall of id * expr_detail list
   | Assign of id * expr_detail
   | And of expr_detail * expr_detail
   | Or of expr_detail * expr_detail
   | Not of expr_detail

type detail = 
  | ExprDet of expr_detail

type expression = 
  | Sexpr of expr_detail * t
  | Sadd of expression * expression
  | Ssub of expression * expression
  | Smult of expression * expression
  | Sdiv of expression * expression
  | Sexpo of expression * expression
  | Smod of expression * expression
  | SfuncCall of expression list * t
  | Sassign of expression * t
  | Sand of expression * expression
  | Sor of expression * expression
  | Snot of expression

type stmt_detail = 
  Expr of expression

type statement = 
  Sstmt of stmt_detail * t

type func_decl_detail = 
  func_decl * t

type func_decl = {
    fname : string;
    formals : string list;
    body : stmt list;
}

type program = 
  func_decl list







