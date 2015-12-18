module VarMap = Map.Make(String);;

exception NoEnvironmentException;;
exception UnassignedVarException of string;;
exception IncorrectTypeException;;

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
  | Float
  | Bool
  | Vector
  | Na

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

type stmt = 
  | Expr of expr
  | Block of stmt list
  | If of expr * stmt * stmt
  | For of string * expr * expr * stmt

type program = stmt list



type environment = {
    symb_tbl_stk: t VarMap.t list; 
}

let init_env = {
    symb_tbl_stk = VarMap.empty::[];
}

let type_match = function
  | StringLit(s) -> String
  | IntLit(i) -> Int
  | FloatLit(f) -> Float
  | BoolLit(b) -> Bool
  | Na -> Na

let reassign_symb_tbl_stk stk = {
    symb_tbl_stk = stk;
}

let assign_current_scope var vtype env =
  let curr, rest =
    ( match env.symb_tbl_stk with
      | curr :: rest -> curr, rest
      | [] -> raise NoEnvironmentException ) in
    let updated = VarMap.add var vtype curr in
  reassign_symb_tbl_stk (updated::rest)

let find_dtype_top_stack id env =
  let stack = env.symb_tbl_stk in
    match stack with
    | [] -> raise (UnassignedVarException id)
    | top :: rest ->
      if VarMap.mem id top then
        VarMap.find id top
      else
        raise (UnassignedVarException id)

let rec scope_expr_detail env = function
  | Ast.Na -> Na, env
  | Ast.Id(s) -> Id(s), env
  | Ast.IntLit(i) -> IntLit(i), env
  | Ast.BoolLit(b) -> BoolLit(b), env
  | Ast.FloatLit(f) -> FloatLit(f), env
  | Ast.StringLit(s) -> StringLit(s) , env
  | Ast.Assign(s,e) ->
    let e1 = scope_expr_detail env e in
    let t = type_match (fst e1) in
    let new_env = assign_current_scope s t env in
    Assign(s, fst(scope_expr_detail env e)), new_env
  | Ast.Vector(s, el) ->
(*       let new_env = assign_current_scope s (type_match t) env in
 *)      
      let helper e = fst (scope_expr_detail env e) in
      Vector(s, List.map helper el), env
  | Ast.VectAcc(s, expr) -> VectAcc(s, fst(scope_expr_detail env expr)), env
  | Ast.Matrix(s, el, e1, e2) -> 
      let helper e = fst (scope_expr_detail env e) in
      Matrix(s, List.map helper el, fst(scope_expr_detail env e1), fst(scope_expr_detail env e2)), env
  | Ast.MatrixAcc(s, e1, e2) -> 
      MatrixAcc(s, fst(scope_expr_detail env e1), fst(scope_expr_detail env e2)), env
  | Ast.Add(expr1,expr2) ->
      let e1, v1 = scope_expr_detail env expr1
      and e2, v2 = scope_expr_detail env expr2 in
      Add(e1, e2), env
  | Ast.Sub(expr1,expr2) ->
      let e1, v1 = scope_expr_detail env expr1
      and e2, v2 = scope_expr_detail env expr2 in
      Sub(e1, e2), env
  | Ast.Mult(expr1,expr2) ->
      let e1, v1 = scope_expr_detail env expr1
      and e2, v2 = scope_expr_detail env expr2 in
      Mult(e1, e2), env
  | Ast.Div(expr1,expr2) ->
      let e1, v1 = scope_expr_detail env expr1
      and e2, v2 = scope_expr_detail env expr2 in
      Div(e1, e2), env
  | Ast.Expo(expr1,expr2) ->
      let e1, v1 = scope_expr_detail env expr1
      and e2, v2 = scope_expr_detail env expr2 in
      Expo(e1, e2), env
  | Ast.Mod(expr1,expr2) ->
      let e1, v1 = scope_expr_detail env expr1
      and e2, v2 = scope_expr_detail env expr2 in
      Mod(e1, e2), env
  | Ast.And(expr1,expr2) ->
      let e1, v1 = scope_expr_detail env expr1
      and e2, v2 = scope_expr_detail env expr2 in
      And(e1, e2), env
  | Ast.Or(expr1,expr2) ->
      let e1, v1 = scope_expr_detail env expr1
      and e2, v2 = scope_expr_detail env expr2 in
      Or(e1, e2), env
  | Ast.Not(expr1) ->
      let e1, v1 = scope_expr_detail env expr1 in
      Not(e1), env
  | Ast.FuncCall(s, el) -> 
      let helper e = fst (scope_expr_detail env e) in
      FuncCall(s, List.map helper el), env

let rec scope_stmt env = function
  | Ast.Expr(expr) -> let e, v = scope_expr_detail env expr in 
                      Expr(e), v
  | Ast.Block(blk) -> 
      let helper e = fst (scope_stmt env e) in
      Block(List.map helper blk), env
  | Ast.If(expr,stmt1,stmt2) -> let i, v = scope_expr_detail env expr in
      If(i, fst (scope_stmt env stmt1), fst (scope_stmt env stmt2)), env
  | Ast.For(str,expr2,expr3,stmt) ->
      let new_env = assign_current_scope str Int env in
      let e2, v2 = scope_expr_detail new_env expr2
      and e3, v3 = scope_expr_detail new_env expr3
    in For(str, e2, e3, fst (scope_stmt new_env stmt)), new_env

let run env stmts =
  let helper henv hstmts = snd (scope_stmt henv hstmts) in
  List.fold_left helper env stmts

let program program = 
  let program_rev = List.rev program in
  let new_env = run init_env program_rev in
  let helper env e = fst(scope_stmt env e) in
  List.map (helper new_env) program

(* 
let push_scope env =  
    update_only_scope (VariableMap.empty::env.var_stack) env

let pop_scope env = 
   match env.var_stack with 
    | popped_scope::other_scopes -> 
        update_only_scope other_scopes env
    | [] -> raise EmptyEnvironmentError 
 *)
