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
  | FMult
  | FAdd
  | FSub
  | FDiv

type dtype = 
    | Na
    | Int
    | Bool
    | Float
    | String
    | Vector of dtype
    | Matrix of dtype

type expr =
  | Na
  | None
  | Id of string * dtype
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

type stmt = 
  | Expr of expr
  | Block of stmt list
  | If of expr * stmt * stmt
  | For of expr * expr * expr * stmt

type def_variable = {
    id: string;
    dtype: dtype;
}

type env = {
    symb_tbl_stk: dtype VarMap.t list; 
}

let init_env = {
    symb_tbl_stk = VarMap.empty::[];
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

let find_dtype_top_stack id env =
  let stack = env.symb_tbl_stk in
    match stack with
    | [] -> raise (UnassignedVarException id)
    | top :: rest ->
      if VarMap.mem id top then
        VarMap.find id top
      else
        raise (UnassignedVarException id)

let rec scope_expr env = function
  | Ast.Na -> Na, env
  | Ast.None -> None, env
  | Ast.Id(s) -> Id(s, find_dtype_top_stack s env), env
  | Ast.Assign(s,e) ->
      let new_env = assign_current_scope s env in
      Assign(s, scope_expr env e), new_env
  | Ast.IntLit(i) -> IntLit(i), env
  | Ast.BoolLit(b) -> BoolLit(b), env
  | Ast.FloatLit(f) -> FloatLit(f), env
  | Ast.StringLit(s) -> StringLit(s) , env
  | Ast.Vector(s, el) ->
      let helper e = fst (scope_expr env e) in
      Vector(s, List.map helper el), env
  | Ast.VectorIdAcc(s1, s2) ->
      let t = find_dtype_top_stack s2 env in
      if (t == Int) then
        VectorIdAcc(s1, s2), env
      else
        raise (IncorrectTypeException)
  | Ast.VectorIntAcc(s, expr) -> VectorIntAcc(s, scope_expr env expr)
  | Ast.Matrix(s, el, e1, e2) -> 
      let helper e = fst (scope_expr env e) in
      Matrix(s, List.map helper el, scope_expr env e1, scope_expr env e2)
  | Ast.MatrixIdAcc(s1, s2, s3) ->
      let t1 = find_dtype_top_stack s2 env
      and t2 = find_dtype_top_stack s3 env in
      if (t1 == Int && t2 == Int) then
        MatrixIdAcc(s1, s2, s3), env
      else
        raise (IncorrectTypeException)
  | Ast.MatrixIntAcc(s, e1, e2) -> 
      MatrixIntAcc(s, scope_expr env e1, scope_expr env e2)
  | Ast.Add(expr1,expr2) ->
      let e1, v1 = scope_expr env expr1
      and e2, v2 = scope_expr env expr2 in
      Add(e1, e2), env
  | Ast.Sub(expr1,expr2) ->
      let e1, v1 = scope_expr env expr1
      and e2, v2 = scope_expr env expr2 in
      Sub(e1, e2), env
  | Ast.Mult(expr1,expr2) ->
      let e1, v1 = scope_expr env expr1
      and e2, v2 = scope_expr env expr2 in
      Mult(e1, e2), env
  | Ast.Div(expr1,expr2) ->
      let e1, v1 = scope_expr env expr1
      and e2, v2 = scope_expr env expr2 in
      Div(e1, e2), env
  | Ast.Expo(expr1,expr2) ->
      let e1, v1 = scope_expr env expr1
      and e2, v2 = scope_expr env expr2 in
      Expo(e1, e2), env
  | Ast.Mod(expr1,expr2) ->
      let e1, v1 = scope_expr env expr1
      and e2, v2 = scope_expr env expr2 in
      Mod(e1, e2), env
  | Ast.FAdd(expr1,expr2) ->
      let e1, v1 = scope_expr env expr1
      and e2, v2 = scope_expr env expr2 in
      FAdd(e1, e2), env
  | Ast.FSub(expr1,expr2) ->
      let e1, v1 = scope_expr env expr1
      and e2, v2 = scope_expr env expr2 in
      FSub(e1, e2), env
  | Ast.FMult(expr1,expr2) ->
      let e1, v1 = scope_expr env expr1
      and e2, v2 = scope_expr env expr2 in
      FMult(e1, e2), env
  | Ast.FDiv(expr1,expr2) ->
      let e1, v1 = scope_expr env expr1
      and e2, v2 = scope_expr env expr2 in
      FDiv(e1, e2), env
  | Ast.And(expr1,expr2) ->
      let e1, v1 = scope_expr env expr1
      and e2, v2 = scope_expr env expr2 in
      And(e1, e2), env
  | Ast.Or(expr1,expr2) ->
      let e1, v1 = scope_expr env expr1
      and e2, v2 = scope_expr env expr2 in
      Or(e1, e2), env
  | Ast.Not(expr1,expr2) ->
      let e1, v1 = scope_expr env expr1
      and e2, v2 = scope_expr env expr2 in
      Not(e1, e2), env
  | Ast.FuncCall(s, el) -> 
      let helper e = fst (expr e) in
      FuncCall(s, List.map helper el)

let rec scope_stmt env = function
  | Ast.Expr(expr) -> let e, v = scope_expr env expr in Expr(e), env
  | Ast.Block(blk) -> let b, v = List.map scope_stmt env blk in Block(b), env
  | Ast.If(expr,stmt1,stmt2) -> let i, v = scope_expr env expr in
      If(i, scope_stmt env stmt1, scope_stmt env stmt2), env
  | Ast.For(expr1,expr2,expr3,stmt) -> 
      let e1, v1 = scope_expr env expr1
      and e2, v2 = scope_expr env expr2
      and e3, v3 = scope_expr env expr3
    in For(e1, e2, e3, scope_stmt env stmt), env

let program program = 
  List.map scope_stmt init_env program

(* 
let push_scope env =  
    update_only_scope (VariableMap.empty::env.var_stack) env

let pop_scope env = 
   match env.var_stack with 
    | popped_scope::other_scopes -> 
        update_only_scope other_scopes env
    | [] -> raise EmptyEnvironmentError 
 *)
