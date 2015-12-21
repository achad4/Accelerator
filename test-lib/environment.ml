module VarMap = Map.Make(String);;
module FuncMap = Map.Make(String);;

exception NoEnvironmentException;;
exception UnassignedVarException of string;;
exception IncorrectTypeException;;

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
  | FormalDef of string * expr * t

type stmt = 
  | Expr of expr
  | Block of stmt list
(*   | ReturnBlock of stmt list * stmt *)
  | If of expr * stmt * stmt
  | For of string * expr * expr * stmt
  | While of expr * stmt
  | Return of expr

type func_def = 
  | FunctionDef of string * expr list * stmt

type program = func_def list * stmt list

type environment = {
    symb_tbl_stk: t VarMap.t list; 
    func_tbl: t FuncMap.t;
    func_tbl_formals: t list FuncMap.t;
}

let init_env = {
    symb_tbl_stk = VarMap.empty::[];
    func_tbl = FuncMap.empty;
    func_tbl_formals = FuncMap.empty;
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
  | StringLit(s) -> String
  | IntLit(i) -> Int
  | FloatLit(f) -> Float
  | BoolLit(b) -> Bool
  | Na -> Na
  | Matrix(a,b,c,d) -> Matrix
  | Vector(a,b) -> Vector
  | Id(id) -> find_type id env
  | FormalDef(id, e, t) -> type_match env e
  | FuncCall(s, el) ->
      if (s = "print") then
        Na
      else if FuncMap.mem s env.func_tbl then
        FuncMap.find s env.func_tbl
      else
        failwith "Function does not exist"
  | _ -> Na

let rec type_of_stmt env = function
  | Expr(e) -> type_match env e
  | Block(sl) -> let last_stmt = List.nth sl (List.length sl - 1) in
                     type_of_stmt env last_stmt
  | If(e,s1,s2) -> Na
  | For(s1,e1,e2,s2) -> Na
  | While(e,s) -> Na
  | Return(e) -> type_match env e

let reassign_symb_tbl_stk stk func forms = {
    symb_tbl_stk = stk;
    func_tbl = func;
    func_tbl_formals = forms;
}

let push_env_scope env =  
    reassign_symb_tbl_stk (VarMap.empty::env.symb_tbl_stk) env.func_tbl env.func_tbl_formals

let pop_env_scope env = 
   match env.symb_tbl_stk with 
    | [] -> raise NoEnvironmentException 
    | curr::rest -> 
        reassign_symb_tbl_stk rest env.func_tbl env.func_tbl_formals

let assign_current_scope fname var vtype env =
  let curr, rest =
    ( match env.symb_tbl_stk with
      | curr :: rest -> curr, rest
      | [] -> raise NoEnvironmentException ) in
    let updated = VarMap.add var vtype curr in
  reassign_symb_tbl_stk (updated::rest) env.func_tbl env.func_tbl_formals

let assign_current_scope var vtype env =
  let curr, rest =
    ( match env.symb_tbl_stk with
      | curr :: rest -> curr, rest
      | [] -> raise NoEnvironmentException ) in
    let updated = VarMap.add var vtype curr in
  reassign_symb_tbl_stk (updated::rest) env.func_tbl env.func_tbl_formals

(* let init_func_args fname var env =
  let updated = FuncMap.add fname var env.func_tbl in
  reassign_symb_tbl_stk env.symb_tbl_stk updated env.func_tbl_formals *)

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
  | Ast.Eq(expr1,expr2) ->
      let e1, v1 = scope_expr_detail env expr1 in
      let e2, v2 = scope_expr_detail v1 expr2 in
      Eq(e1, e2), v2
  | Ast.Neq(expr1,expr2) ->
      let e1, v1 = scope_expr_detail env expr1 in
      let e2, v2 = scope_expr_detail v1 expr2 in
      Neq(e1, e2), v2
  | Ast.Add(expr1,expr2) ->
      let e1, v1 = scope_expr_detail env expr1 in
      let e2, v2 = scope_expr_detail v1 expr2 in
      Add(e1, e2), v2
  | Ast.Sub(expr1,expr2) ->
      let e1, v1 = scope_expr_detail env expr1 in
      let e2, v2 = scope_expr_detail v1 expr2 in
      Sub(e1, e2), env
  | Ast.Mult(expr1,expr2) ->
      let e1, v1 = scope_expr_detail env expr1 in
      let e2, v2 = scope_expr_detail v1 expr2 in
      Mult(e1, e2), env
  | Ast.Div(expr1,expr2) ->
      let e1, v1 = scope_expr_detail env expr1 in
      let e2, v2 = scope_expr_detail v1 expr2 in
      Div(e1, e2), env
  | Ast.Expo(expr1,expr2) ->
      let e1, v1 = scope_expr_detail env expr1 in
      let e2, v2 = scope_expr_detail v1 expr2 in
      Expo(e1, e2), env
  | Ast.Mod(expr1,expr2) ->
      let e1, v1 = scope_expr_detail env expr1 in
      let e2, v2 = scope_expr_detail v1 expr2 in
      Mod(e1, e2), env
  | Ast.And(expr1,expr2) ->
      let e1, v1 = scope_expr_detail env expr1 in
      let e2, v2 = scope_expr_detail v1 expr2 in
      And(e1, e2), env
  | Ast.Or(expr1,expr2) ->
      let e1, v1 = scope_expr_detail env expr1 in
      let e2, v2 = scope_expr_detail v1 expr2 in
      Or(e1, e2), env
  | Ast.Not(expr1) ->
      let e1, v1 = scope_expr_detail env expr1 in
      Not(e1), v1
  | Ast.FuncCall(s, el) -> 
      let helper e = fst (scope_expr_detail env e) in
      FuncCall(s, List.map helper el), env
  | Ast.FormalDef(id,e) ->
      let e1 = scope_expr_detail env e in
      let t = type_match env (fst e1) in
      let new_env = assign_current_scope id t env in
      FormalDef(id, fst e1, t), new_env

let rec scope_stmt env = function
  | Ast.Expr(expr) -> let e, new_env = scope_expr_detail env expr in 
                      Expr(e), new_env
  | Ast.Block(blk) -> 
      let rec pass_envs env = function
       | [] -> []
       | [s] -> let (s, new_env) = scope_stmt env s in [s]
       | hd :: tl ->  let (s, new_env) = scope_stmt env hd in
                      (pass_envs new_env tl)@[s] in

        let helper henv hstmts = snd (scope_stmt henv hstmts) in
        let block_env = List.fold_left helper env (List.rev blk) in
      
      Block(pass_envs env (List.rev blk)), block_env 
  | Ast.If(expr,stmt1,stmt2) -> let i, v = scope_expr_detail env expr in
      If(i, fst (scope_stmt env stmt1), fst (scope_stmt env stmt2)), env
  | Ast.For(str,expr2,expr3,stmt) ->
      let new_env = assign_current_scope str Int env in
      let e2, v2 = scope_expr_detail new_env expr2
      and e3, v3 = scope_expr_detail new_env expr3 in
      let r = (scope_stmt new_env stmt) in
      For(str, e2, e3, fst r), snd r
  | Ast.While(expr,stmt) -> 
      let compare = scope_expr_detail env expr in
      let block = scope_stmt env stmt in
      While(fst compare, fst block), snd block
  | Ast.Return(e) -> 
      let e1, v1 = scope_expr_detail env e in
      Return(e1), v1

let scope_func env = function
  | Ast.FunctionDef(str, el, stmt) ->
      (* let init_env = assign_current_scope str Na (push_env_scope env) in *)
      (* Initialize formal arguments *)
      let init_formals env1 forms =
        let helper henv hforms = 
         snd (scope_expr_detail henv hforms) in
        List.fold_left helper env1 forms in
      let new_env = init_formals init_env el in
      let block, new_env = scope_stmt new_env stmt in
      let ret_type = (type_of_stmt new_env block) in
      let new_fname_map = FuncMap.add str ret_type new_env.func_tbl in
      let new_env = reassign_symb_tbl_stk new_env.symb_tbl_stk new_fname_map env.func_tbl_formals in
      (* let new_env = assign_current_scope str ret_type new_env in
       *)
      let helper2 e = fst (scope_expr_detail new_env e) in
      (* Add function formals to env *)
      let rec formal_type_list env = function
        | [] -> []
        | hd::tl -> type_match env hd :: formal_type_list env tl in
      let my_formal_type_list = formal_type_list new_env (List.map helper2 el) in
      let new_form_map = FuncMap.add str my_formal_type_list new_env.func_tbl_formals in
      let new_env = reassign_symb_tbl_stk new_env.symb_tbl_stk new_fname_map new_form_map in
      
      FunctionDef(str, List.map helper2 el, block), new_env

let run_stmts env stmts =
  let helper henv hstmts = snd (scope_stmt henv hstmts) in
  List.fold_left helper env stmts

let run_funcs env funcs =
  let helper henv hfuncs = snd (scope_func henv hfuncs) in
  List.fold_left helper env funcs

let program program =
  let init_print = FuncMap.add "print" (type_match init_env Na) init_env.func_tbl in
  let init_env = reassign_symb_tbl_stk init_env.symb_tbl_stk init_print init_env.func_tbl_formals in

  let funcs_rev = List.rev (fst program) in
  let stmts_rev = List.rev (snd program) in
  (* let func_helper1 f = fst (scope_func init_env f) in
  let func_helper2 f = snd (scope_func init_env f) in
  let funcs = List.map func_helper1 funcs_rev in
  let funcs_env = List.map func_helper2 funcs_rev in *)

  let new_env1 = run_funcs init_env funcs_rev in 
  let new_env2 = run_stmts new_env1 stmts_rev in
  let helper1 env e = (scope_func env e) in
  let helper2 env e = (scope_stmt env e) in
  (List.map (helper1 new_env1) funcs_rev), (List.map (helper2 new_env2) stmts_rev)

