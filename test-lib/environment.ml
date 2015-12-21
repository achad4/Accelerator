module VarMap = Map.Make(String);;
module FuncMap = Map.Make(String);;

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
  | Matrix
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
  | FuncCall of string * expr list 
  | Assign of string * expr
  | And of expr * expr
  | Or of expr * expr
  | Not of expr
  | FormalDef of string * expr

type stmt = 
  | Expr of expr
  | Block of stmt list
(*   | ReturnBlock of stmt list * stmt *)
  | If of expr * stmt * stmt
  | For of string * expr * expr * stmt
  | Return of expr

type func_def = 
  | FunctionDef of string * expr list * stmt

type program = func_def list * stmt list

type environment = {
    symb_tbl_stk: t VarMap.t list; 
    func_tbl: t FuncMap.t;
}

let init_env = {
    symb_tbl_stk = VarMap.empty::[];
    func_tbl = FuncMap.empty;
}

(* Rename to any stack  *)
let find_type id env =
  let rec search_scope_lvl lvl =
  match lvl with
    | [] -> raise (UnassignedVarException id)
    | top :: rest ->
      if VarMap.mem id top then
        VarMap.find id top
      else
        search_scope_lvl rest
  in search_scope_lvl env.symb_tbl_stk 

let rec type_match env = function
  | StringLit(s) -> print_endline "String"; String
  | IntLit(i) -> print_endline "Int"; Int
  | FloatLit(f) -> print_endline "Int"; Float
  | BoolLit(b) -> print_endline "Int"; Bool
  | Na -> print_endline "Na"; Na
  | Matrix(a,b,c,d) -> print_endline "Int"; Matrix
  | Vector(a,b) -> print_endline "Int"; Vector
  | Id(id) -> print_endline "string"; find_type id env
  | Add(expr1,expr2) -> type_match env expr1
(*   | _ -> print_endline "Na"; Na *) 

let rec type_of_stmt env = function
  | Expr(e) -> type_match env e
  | Block(sl) -> let last_stmt = List.nth sl (List.length sl - 1) in
                     type_of_stmt env last_stmt
  | If(e,s1,s2) -> Na
  | For(s1,e1,e2,s2) -> Na
  | Return(e) -> type_match env e

let reassign_symb_tbl_stk stk func = {
    symb_tbl_stk = stk;
    func_tbl = func;
}

let push_env_scope env =  
    reassign_symb_tbl_stk (VarMap.empty::env.symb_tbl_stk) env.func_tbl

let pop_env_scope env = 
   match env.symb_tbl_stk with 
    | [] -> raise NoEnvironmentException 
    | curr::rest -> 
        reassign_symb_tbl_stk rest env.func_tbl

let assign_current_scope var vtype env =
  let curr, rest =
    ( match env.symb_tbl_stk with
      | curr :: rest -> curr, rest
      | [] -> raise NoEnvironmentException ) in
    let updated = VarMap.add var vtype curr in
  reassign_symb_tbl_stk (updated::rest) env.func_tbl

let init_func_args fname var env =
  let updated = FuncMap.add fname var env.func_tbl in
  reassign_symb_tbl_stk env.symb_tbl_stk updated

(* let rec init_args fname args env = 
  match args with
  | [] -> env
  | first :: rest -> 
    let new_sym = assign_current_scope first Na env in
    let new_func = init_func_args fname first env in
    let new_env = reassign_symb_tbl_stk new_sym.symb_tbl_stk new_func.func_tbl in
    init_args fname rest new_env *)

let rec scope_expr_detail env = function
  | Ast.Na -> Na, env
  | Ast.Id(s) -> Id(s), env
  | Ast.IntLit(i) -> IntLit(i), env
  | Ast.BoolLit(b) -> BoolLit(b), env
  | Ast.FloatLit(f) -> FloatLit(f), env
  | Ast.StringLit(s) -> StringLit(s) , env
  | Ast.Assign(s,e) ->
    let e1 = scope_expr_detail env e in
    print_endline "in assign";
    let t = type_match env (fst e1) in
    let new_env = assign_current_scope s t env in
    Assign(s, fst(e1)), new_env
  | Ast.Vector(s, el) ->
(*       let new_env = assign_current_scope s (type_match t) env in
 *)   
      let head = List.hd el in
      let e1 = scope_expr_detail env head in
      let t = type_match env (fst e1) in
      let new_env = assign_current_scope s t env in
      let helper e = fst (scope_expr_detail env e) in
      Vector(s, List.map helper el), new_env
  | Ast.VectAcc(s, expr) -> VectAcc(s, fst(scope_expr_detail env expr)), env
  | Ast.Matrix(s, el, e1, e2) ->
      let head = List.hd el in
      let mtype = type_match env (fst (scope_expr_detail env head)) in
      let new_env = assign_current_scope s mtype env in
      let helper e = fst (scope_expr_detail env e) in
      Matrix(s, List.map helper el, fst(scope_expr_detail env e1), fst(scope_expr_detail env e2)), new_env
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
  | Ast.FormalDef(id,e) -> 
      let e1 = scope_expr_detail env e in
      let t = type_match env (fst e1) in
      let new_env = assign_current_scope id t env in
      FormalDef(id, fst e1), new_env

let rec scope_stmt env = function
  | Ast.Expr(expr) -> let e, env = scope_expr_detail env expr in 
                      Expr(e), env
  | Ast.Block(blk) -> 
      let rec pass_envs env = function
       | [] -> []
       | [s] -> let (s, new_env) = scope_stmt env s in 
                [s]
       | hd :: tl ->  let (s, new_env) = scope_stmt env hd in
                      print_int (List.length tl);
                      (pass_envs new_env tl)@[s] in

        let helper henv hstmts = snd (scope_stmt henv hstmts) in
        let block_env = List.fold_left helper env blk in

      Block(pass_envs env (List.rev blk)), block_env 
  | Ast.If(expr,stmt1,stmt2) -> let i, v = scope_expr_detail env expr in
      If(i, fst (scope_stmt env stmt1), fst (scope_stmt env stmt2)), env
  | Ast.For(str,expr2,expr3,stmt) ->
      print_endline str;
      let new_env = assign_current_scope str Int env in
      let e2, v2 = scope_expr_detail new_env expr2
      and e3, v3 = scope_expr_detail new_env expr3 in
      let r = (scope_stmt new_env stmt) in
      For(str, e2, e3, fst r), snd r
  | Ast.Return(e) -> 
      let e1, v1 = scope_expr_detail env e in
      Return(e1), v1

let scope_func env = function
  | Ast.FunctionDef(str, el, stmt) -> 
      let init_env = assign_current_scope str Na (push_env_scope env) in
      let init_formals env1 forms =
        let helper henv hforms = snd (scope_expr_detail henv hforms) in
        List.fold_left helper env1 forms in
      let new_env = init_formals init_env el in
      let helper2 e = fst (scope_expr_detail new_env e) in
      let block = scope_stmt env stmt in
      let new_env = assign_current_scope str (type_of_stmt env (fst block)) env in
      FunctionDef(str, List.map helper2 el, fst block), new_env

let run_stmts env stmts =
  let helper henv hstmts = snd (scope_stmt henv hstmts) in
  List.fold_left helper env stmts
(* 
let run_funcs env funcs =
  let helper henv hfuncs = snd (scope_func henv hfuncs) in
  List.fold_left helper env funcs *)

let program program = 
  let funcs_rev = List.rev (fst program) in
  let stmts_rev = List.rev (snd program) in
(*   let new_env = run_funcs init_env funcs_rev in  *)
  let new_env = run_stmts init_env stmts_rev in
  let helper1 env e = (scope_func env e) in
  let helper2 env e = (scope_stmt env e) in
  (List.map (helper1 new_env) funcs_rev), (List.map (helper2 new_env) (snd program))

