module VarMap = Map.Make(String);;

exception NoEnvironmentException;;

(* exception EmptyEnvironmentError;;
exception NameAlreadyBoundError of string;;
exception VariableNotFound of string;;
exception VariableAlreadyDeclared;;  *)

type dtype = 
    | Na
    | Int
    | Bool
    | Float
    | String
    | Vector of dtype
    | Matrix of dtype

type def_variable = {
    id: string;
    dtype: dtype;
}

type env = {
    symb_tbl_stk: dtype VarMap.t list; 
}

let reassign_symb_tbl_stk stk = {
    symb_tbl_stk = stk;
}

let assign_current_scope var env =
  let curr, rest =
    ( match env.symb_tbl_stk with
      | curr :: rest -> curr, rest
      | [] -> raise NoEnvironmentException ) in
    let updated = VarMap.add var.id var.dtype in
    reassign_symb_tbl_stk (updated::rest)

(* 
let push_scope env =  
    update_only_scope (VariableMap.empty::env.var_stack) env

let pop_scope env = 
   match env.var_stack with 
    | popped_scope::other_scopes -> 
        update_only_scope other_scopes env
    | [] -> raise EmptyEnvironmentError 
 *)

(* type op = 
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
  | FMult
  | FAdd
  | FSub
  | FDiv *)

(* type expr =
  | Na
  | None
  | Id of string
	| IntLit of int
  | BoolLit of bool
  | FloatLit of float
  | StringLit of string
  | Vector of string * expr list
  | VectIdAcc of string * string
  | VectIntAcc of string * expr
  | Matrix of string * expr list * expr * expr
  | MatrixIdAcc of string * string * string
  | MatrixIntAcc of string * expr * expr
  | Add of expr * expr
  | Sub of expr * expr
  | Mult of expr * expr
  | Div of expr * expr
  | Expo of expr * expr
  | Mod of expr * expr
  | FAdd of expr * expr
  | FSub of expr * expr
  | FMult of expr * expr
  | FDiv of expr * expr
 	| FuncCall of string * expr list 
 	| Assign of string * expr
  | And of expr * expr
  | Or of expr * expr
  | Not of expr
 *)
let rec attach_envr_expr env = function
  | 

let rec attach_envr_stmt env = function
  | 
(* type stmt = 
	| Expr of expr
  | Block of stmt list
  | If of expr * stmt * stmt
  | For of expr * expr * expr * stmt

type program = stmt list *)

let program program = 
  List.map stmt program