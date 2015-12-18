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
  | FAdd
  | FSub
  | FMult
  | FDiv
  | Assign
  | And
  | Or
  | Not

type t = 
  | String
  | Int
  | Float
  | Bool
  | Na
  | IdType
  | Vector of t
  | Matrix of t

type id = 
  | Id of string

type expr_detail = 
  | NaExpr of t
  | IdLit of string * t
  | IntLit of int
  | IntExpr of expr_detail * t
  | BoolLit of bool
  | FloatLit of float
  | StringLit of string
  | Vector of string * expr_detail list * t
  | VectIdAcc of string * string * t
  | VectIntAcc of string * expr_detail * t
  | Matrix of string * expr_detail list * expr_detail * expr_detail * t
  | MatrixIdAcc of string * string * string * t
  | MatrixIntAcc of string * expr_detail * expr_detail * t
  | Add of expr_detail * expr_detail * t
  | Sub of expr_detail * expr_detail  * t
  | Mult of expr_detail * expr_detail * t
  | Div of expr_detail * expr_detail * t
  | FAdd of expr_detail * expr_detail * t
  | FSub of expr_detail * expr_detail * t
  | FMult of expr_detail * expr_detail * t
  | FDiv of expr_detail * expr_detail * t
  | Expo of expr_detail * expr_detail * t
  | Mod of expr_detail * expr_detail * t
  | FuncCall of string * expr_detail list * t
  | Assign of string * expr_detail * t
  | And of expr_detail * expr_detail * t
  | Or of expr_detail * expr_detail * t
  | Not of expr_detail * t

type detail = 
  | ExprDet of expr_detail

type expression = 
  | Sexpr of expr_detail * t
  | Sadd of expression * expression * t
  | Ssub of expression * expression * t
  | Smult of expression * expression * t
  | SFAdd of expression * expression * t
  | SFSub of expression * expression * t
  | SFMult of expression * expression * t
  | SFDiv of expression * expression * t
  | Sdiv of expression * expression * t
  | Sexpo of expression * expression * t
  | Smod of expression * expression * t
  | SfuncCall of expression list * t
  | Sassign of expression * t
  | Sand of expression * expression * t
  | Sor of expression * expression * t
  | Snot of expression * t

type statement = 
  | Sstmt of expression * t
  | Sblock of statement list * t
  | Sif of expression * statement * statement * t
  | Sfor of expression * expression * expression * statement * t

(* type dtype = 
  | String
  | Int
  | Float
  | Bool
  | Na
  | IdType
  | Vector of dtype
  | Matrix of dtype *)

type environment = {
    symb_tbl_stk: t VarMap.t list; 
}

let init_env = {
    symb_tbl_stk = VarMap.empty::[];
}

let type_match = function
  | Sast.String -> String
  | Sast.Int -> Int
  | Sast.Float -> Float
  | Sast.Bool -> Bool
  | Sast.Na -> Na
  | Sast.IdType -> IdType

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
  | Sast.Na(t) -> NaExpr(type_match t), env
  | Sast.IdLit(s) -> IdLit(s, find_dtype_top_stack s env), env
  | Sast.Assign(s,e,t) ->
      let new_env = assign_current_scope s (type_match t) env in
      Assign(s, fst(scope_expr_detail env e), type_match t), new_env
  | Sast.IntLit(i) -> IntLit(i), env
  | Sast.BoolLit(b) -> BoolLit(b), env
  | Sast.FloatLit(f) -> FloatLit(f), env
  | Sast.StringLit(s) -> StringLit(s) , env
  | Sast.IntExpr(e,t) -> IntExpr(fst(scope_expr_detail env e), type_match t), env
  | Sast.Vector(s, el, t) ->
      let new_env = assign_current_scope s (type_match t) env in
      
      let helper e = fst (scope_expr_detail env e) in
      Vector(s, List.map helper el, type_match t), new_env
  | Sast.VectIdAcc(s1, s2, t) -> 
      let ty = find_dtype_top_stack s2 env in 
      if (ty == Int) then
        VectIdAcc(s1, s2, type_match t), env
      else
        raise (IncorrectTypeException)
  | Sast.VectIntAcc(s, expr, t) -> VectIntAcc(s, fst(scope_expr_detail env expr), type_match t), env
  | Sast.Matrix(s, el, e1, e2, t) -> 
      let helper e = fst (scope_expr_detail env e) in
      Matrix(s, List.map helper el, fst(scope_expr_detail env e1), fst(scope_expr_detail env e2), type_match t), env
  | Sast.MatrixIdAcc(s1, s2, s3, t) ->
      let t1 = find_dtype_top_stack s2 env
      and t2 = find_dtype_top_stack s3 env in
      if (t1 == Int && t2 == Int) then
        MatrixIdAcc(s1, s2, s3, type_match t), env
      else
        raise (IncorrectTypeException)
  | Sast.MatrixIntAcc(s, e1, e2, t) -> 
      MatrixIntAcc(s, fst(scope_expr_detail env e1), fst(scope_expr_detail env e2), type_match t), env
  | Sast.Add(expr1,expr2,t) ->
      let e1, v1 = scope_expr_detail env expr1
      and e2, v2 = scope_expr_detail env expr2 in
      Add(e1, e2,type_match t), env
  | Sast.Sub(expr1,expr2,t) ->
      let e1, v1 = scope_expr_detail env expr1
      and e2, v2 = scope_expr_detail env expr2 in
      Sub(e1, e2,type_match t), env
  | Sast.Mult(expr1,expr2, t) ->
      let e1, v1 = scope_expr_detail env expr1
      and e2, v2 = scope_expr_detail env expr2 in
      Mult(e1, e2,type_match t), env
  | Sast.Div(expr1,expr2, t) ->
      let e1, v1 = scope_expr_detail env expr1
      and e2, v2 = scope_expr_detail env expr2 in
      Div(e1, e2,type_match t), env
  | Sast.Expo(expr1,expr2, t) ->
      let e1, v1 = scope_expr_detail env expr1
      and e2, v2 = scope_expr_detail env expr2 in
      Expo(e1, e2,type_match t), env
  | Sast.Mod(expr1,expr2,t) ->
      let e1, v1 = scope_expr_detail env expr1
      and e2, v2 = scope_expr_detail env expr2 in
      Mod(e1, e2,type_match t), env
  | Sast.FAdd(expr1,expr2,t) ->
      let e1, v1 = scope_expr_detail env expr1
      and e2, v2 = scope_expr_detail env expr2 in
      FAdd(e1, e2,type_match t), env
  | Sast.FSub(expr1,expr2,t) ->
      let e1, v1 = scope_expr_detail env expr1
      and e2, v2 = scope_expr_detail env expr2 in
      FSub(e1, e2,type_match t), env
  | Sast.FMult(expr1,expr2,t) ->
      let e1, v1 = scope_expr_detail env expr1
      and e2, v2 = scope_expr_detail env expr2 in
      FMult(e1, e2,type_match t), env
  | Sast.FDiv(expr1,expr2,t) ->
      let e1, v1 = scope_expr_detail env expr1
      and e2, v2 = scope_expr_detail env expr2 in
      FDiv(e1, e2,type_match t), env
  | Sast.And(expr1,expr2,t) ->
      let e1, v1 = scope_expr_detail env expr1
      and e2, v2 = scope_expr_detail env expr2 in
      And(e1, e2,type_match t), env
  | Sast.Or(expr1,expr2,t) ->
      let e1, v1 = scope_expr_detail env expr1
      and e2, v2 = scope_expr_detail env expr2 in
      Or(e1, e2,type_match t), env
  | Sast.Not(expr1,t) ->
      let e1, v1 = scope_expr_detail env expr1 in
      Not(e1, type_match t), env
  | Sast.FuncCall(s, el,t) -> 
      let helper e = fst (scope_expr_detail env e) in
      FuncCall(s, List.map helper el, type_match t), env

let rec scope_expr env = function
  | Sast.Sexpr(e, t) -> let e1, v1 = scope_expr_detail env e in Sexpr(e1, type_match t), v1
  | Sast.Sadd(e1, e2, t) -> Sadd(fst (scope_expr env e1), fst (scope_expr env e2), type_match t), env
  | Sast.Ssub(e1, e2, t) -> Ssub(fst (scope_expr env e1), fst (scope_expr env e2), type_match t), env
  | Sast.Smult(e1, e2, t) -> Smult(fst (scope_expr env e1), fst (scope_expr env e2), type_match t), env
  | Sast.Sdiv(e1, e2, t) -> Sdiv(fst (scope_expr env e1), fst (scope_expr env e2),type_match  t), env
  | Sast.SFAdd(e1, e2, t) -> Sadd(fst (scope_expr env e1), fst (scope_expr env e2), type_match t), env
  | Sast.SFSub(e1, e2, t) -> Ssub(fst (scope_expr env e1), fst (scope_expr env e2), type_match t), env
  | Sast.SFMult(e1, e2, t) -> Smult(fst (scope_expr env e1), fst (scope_expr env e2), type_match t), env
  | Sast.SFDiv(e1, e2, t) -> Sdiv(fst (scope_expr env e1),fst (scope_expr env e2), type_match t), env
  | Sast.Sexpo(e1, e2, t) -> Sexpo(fst (scope_expr env e1), fst (scope_expr env e2), type_match t), env
  | Sast.Smod(e1, e2, t) -> Smod(fst (scope_expr env e1), fst(scope_expr env e2), type_match t), env
  | Sast.SfuncCall(el, t) -> 
      let helper e = fst (scope_expr env e) in
      SfuncCall(List.map helper el, type_match t), env
  | Sast.Sassign(e, t) -> Sassign(fst (scope_expr env e), type_match t), env
  | Sast.Sand(e1, e2, t) -> Sand(fst (scope_expr env e1), fst(scope_expr env e2), type_match t), env
  | Sast.Sor(e1, e2, t) -> Sor(fst(scope_expr env e1), fst (scope_expr env e2), type_match t), env
  | Sast.Snot(e, t) -> Snot(fst (scope_expr env e), type_match t), env

let rec scope_stmt env = function
  | Sast.Sstmt(expr,t) -> let e, v = scope_expr env expr in Sstmt(e, type_match t), v
  | Sast.Sblock(blk,t) -> 
      let helper e = fst (scope_stmt env e) in
      Sblock(List.map helper blk, type_match t), env
  | Sast.Sif(expr,stmt1,stmt2,t) -> let i, v = scope_expr env expr in
      Sif(i, fst (scope_stmt env stmt1), fst (scope_stmt env stmt2), type_match t), env
  | Sast.Sfor(expr1,expr2,expr3,stmt,t) -> 
      let e1, v1 = scope_expr env expr1
      and e2, v2 = scope_expr env expr2
      and e3, v3 = scope_expr env expr3
    in Sfor(e1, e2, e3, fst (scope_stmt env stmt), type_match t), env

let run env stmts =
  let helper henv hstmts = snd (scope_stmt henv hstmts) in
  List.fold_left helper env stmts

let program program = 
  let new_env = run init_env program in
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
