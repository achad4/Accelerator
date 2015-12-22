open Environment

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

type ct = 
  | String
  | Int
  | Float
  | Bool
  | Void
  | Vector
  | Matrix
  | IdType

type cexpr_detail = 
   | Na of string * ct
   | Id of string * ct
   | IntLit of int
   | IntExpr of cexpr_detail * ct
   | FloatLit of float
   | BoolLit of bool
   | StringLit of string
   | Vector of string * cexpr_detail list * ct
   | VectAcc of string * cexpr_detail * ct
   | Matrix of string * cexpr_detail list * cexpr_detail * cexpr_detail * ct
   | MatrixAcc of string * cexpr_detail * cexpr_detail * ct
   | Eq of cexpr_detail * cexpr_detail * ct
   | Neq of cexpr_detail * cexpr_detail * ct
   | StrEq of cexpr_detail * cexpr_detail * ct
   | StrNeq of cexpr_detail * cexpr_detail * ct
   | Add of cexpr_detail * cexpr_detail * ct
   | Sub of cexpr_detail * cexpr_detail * ct
   | Mult of cexpr_detail * cexpr_detail * ct
   | Div of cexpr_detail * cexpr_detail * ct
   | FAdd of cexpr_detail * cexpr_detail * ct
   | FSub of cexpr_detail * cexpr_detail * ct
   | FMult of cexpr_detail * cexpr_detail * ct
   | FDiv of cexpr_detail * cexpr_detail * ct
(*    | MatrixAdd of cexpr_detail * cexpr_detail * ct
   | MatrixMult of cexpr_detail * cexpr_detail * ct *)
   | Expo of cexpr_detail * cexpr_detail * ct
   | Mod of cexpr_detail * cexpr_detail * ct
   | FuncCall of string * cexpr_detail list * ct
   | PrintCall of cexpr_detail * ct
   | PrintMatrixCall of cexpr_detail * ct
   | Assign of string * cexpr_detail * ct
   | Update of string * cexpr_detail * ct
(*    | MatrixAssign of string * cexpr_detail * ct
 *)   | And of cexpr_detail * cexpr_detail * ct
   | Or of cexpr_detail * cexpr_detail * ct
   | Not of cexpr_detail * ct
   | FormalDef of string * cexpr_detail * ct

type cexpression = 
  | Cexpr of cexpr_detail * ct
  | Ceq of cexpression * cexpression * ct
  | Cneq of cexpression * cexpression * ct
  | Cadd of cexpression * cexpression * ct
  | Csub of cexpression * cexpression * ct
  | Cmult of cexpression * cexpression * ct
  | Cdiv of cexpression * cexpression * ct
  | CFAdd of cexpression * cexpression * ct
  | CFSub of cexpression * cexpression * ct
  | CFMult of cexpression * cexpression * ct
  | CFDiv of cexpression * cexpression * ct
  | CMatrixAcc of cexpression * cexpression * ct
  | CMatrixAdd of cexpression * cexpression * ct
  | CMatrixMult of cexpression * cexpression * ct
  | Cexpo of cexpression * cexpression * ct
  | Cmod of cexpression * cexpression * ct
  | CfuncCall of cexpression list * ct
  | Cassign of cexpression * ct
  | Cand of cexpression * cexpression * ct
  | Cor of cexpression * cexpression * ct
  | Cnot of cexpression * ct

type statement = 
  | Cstmt of cexpression * ct
  | Cblock of statement list * ct
(*   | CReturnBlock of statement list * statement * ct *)
  | Cif of cexpression * statement * statement * ct
  | Cfor of string * cexpression * cexpression * statement * ct
  | Cwhile of cexpr_detail * statement * ct
  | Creturn of cexpression * ct

type func_decl_detail = {
    fname : string;
    formals : cexpr_detail list;
    body : statement;
}

type func_decl = 
  | CFunctionDef of func_decl_detail * ct

type program = 
  func_decl list

let rec string_of_matrix_assign = function
  | Matrix(s, el, e, e1, ct) -> s
  | Assign(s, e, ct) -> string_of_matrix_assign e
  | Id(s, ct) -> s
  | _ -> failwith "matrix required"



let rec string_of_ctype = function
  | String -> "string"
  | Float -> "float"
  | Int -> "int"
  | Bool -> "bool"
  | Void -> "void"
  | IdType -> "IdType"
  | Vector -> "Vector"
  | Matrix -> "vector<vector<int> >"

let rec type_match = function
  | Environment.String -> String
  | Environment.Int -> Int
  | Environment.Float -> Float
  | Environment.Bool -> Bool
  | Environment.Na -> Void
  | Environment.Vector -> Vector
  | Environment.Matrix -> Matrix

let rec cexpr_detail = function
 | Sast.Id(s) ->  Id(s, String)
 | Sast.IntLit(i) -> IntLit(i)
 | Sast.FloatLit(i) -> FloatLit(i)
 | Sast.BoolLit(b) -> BoolLit(b)
 | Sast.StringLit(s) -> StringLit(s)
 | Sast.IntExpr(e,t) -> IntExpr(cexpr_detail e, type_match t)
 | Sast.Vector(s, v, t) -> let ct = type_match t in
         Vector(s, List.map cexpr_detail v, ct)
 | Sast.VectAcc(s, e1, t) ->
        let cexp1 = cexpr_detail e1 in
        VectAcc(s, cexp1, type_match t)
 | Sast.NaLit(t) -> Na("Void", type_match t)
 | Sast.Matrix(s, v, nr, nc, t) -> 
                                let ct = type_match t in
                                Matrix(s, List.map cexpr_detail v, cexpr_detail nr, cexpr_detail nc, ct)
 | Sast.MatrixAcc(s, e1, e2, t) ->
        let cexp1 = cexpr_detail e1 in
         let cexp2 = cexpr_detail e2 in
        MatrixAcc(s, cexp1, cexp2, type_match t)
 (*Expand when you pull in Alan's Fadd etc.*)
 | Sast.Neq(e1, e2, t) -> Neq((cexpr_detail e1), (cexpr_detail e2), type_match t)
 | Sast.Eq(e1, e2, t) -> Eq((cexpr_detail e1), (cexpr_detail e2), type_match t)
 | Sast.StrEq(e1, e2, t) -> StrEq((cexpr_detail e1), (cexpr_detail e2), type_match t)
 | Sast.StrNeq(e1, e2, t) -> StrNeq((cexpr_detail e1), (cexpr_detail e2), type_match t)
 | Sast.Add(e1, e2, t) -> Add((cexpr_detail e1), (cexpr_detail e2), type_match t)
 | Sast.Sub(e1, e2, t) -> Sub(cexpr_detail e1, cexpr_detail e2, type_match t)
 | Sast.Mult(e1, e2, t) -> Mult(cexpr_detail e1, cexpr_detail e2, type_match t)
 | Sast.Div(e1, e2, t) -> Div(cexpr_detail e1, cexpr_detail e2, type_match t)
(*  | Sast.FAdd(e1, e2, t) -> Add((cexpr_detail e1), (cexpr_detail e2), Float)
 | Sast.FSub(e1, e2, t) -> Sub(cexpr_detail e1, cexpr_detail e2, Float)
 | Sast.FMult(e1, e2, t) -> Mult(cexpr_detail e1, cexpr_detail e2, Float)
 | Sast.FDiv(e1, e2, t) -> Div(cexpr_detail e1, cexpr_detail e2, Float) *)
(*  | Sast.MatrixAdd(e1, e2, t) ->  MatrixAdd(cexpr_detail e1, cexpr_detail e2, Matrix)
 | Sast.MatrixMult(e1, e2, t) -> MatrixMult(cexpr_detail e1, cexpr_detail e2, Matrix) *)
 | Sast.Expo(e1, e2, t) -> Expo(cexpr_detail e1, cexpr_detail e2, Int)
 | Sast.Mod(e1, e2, t) -> Mod(cexpr_detail e1, cexpr_detail e2, Int)
 | Sast.FuncCall(id, el, t) -> let ct = type_match t in
                               FuncCall(id, List.map cexpr_detail el, ct)
 | Sast.PrintCall(e, t) -> let ct = type_match t in
                                PrintCall(cexpr_detail e, ct)
 | Sast.PrintMatrixCall(e, t) -> let ct = type_match t in
                                PrintMatrixCall(cexpr_detail e, ct)
 | Sast.Assign(id, e, t) -> let ct = type_match t in
                             Assign(id, cexpr_detail e, ct)
 | Sast.Update(id, e, t) -> let ct = type_match t in
                              Update(id, cexpr_detail e, ct)
(*  | Sast.MatrixAssign(id, e, t) -> let ct = type_match t in
                                  MatrixAssign(id, cexpr_detail e, ct) *)
 | Sast.And(e1, e, t) -> let ct = type_match t in 
                          And(cexpr_detail e1, cexpr_detail e, ct)
 | Sast.Or(e1, e2, t) ->  let ct = type_match t in
                          Or(cexpr_detail e1, cexpr_detail e2, ct)
 | Sast.Not(e, t) -> let ct = type_match t in
                      Not(cexpr_detail e, ct)
 | Sast.FormalDef(id, e, t, env) -> 
      let t = Environment.find_type id env in
      FormalDef(id, cexpr_detail e, type_match t)
  

let rec cexpr = function
  | Sast.Sexpr(e, t) -> Cexpr(cexpr_detail e, type_match t)
  | Sast.Sadd(e1, e2, t) -> Cadd(cexpr e1, cexpr e2, type_match t)
  | Sast.Ssub(e1, e2, t) -> Csub(cexpr e1, cexpr e2, type_match t)
  | Sast.Smult(e1, e2, t) -> Cmult(cexpr e1, cexpr e2, type_match t)
  | Sast.Sdiv(e1, e2, t) -> Cdiv(cexpr e1, cexpr e2, type_match t)
  | Sast.SFAdd(e1, e2, t) -> Cadd(cexpr e1, cexpr e2, type_match t)
  | Sast.SFSub(e1, e2, t) -> Csub(cexpr e1, cexpr e2, type_match t)
  | Sast.SFMult(e1, e2, t) -> Cmult(cexpr e1, cexpr e2, type_match t)
  | Sast.SFDiv(e1, e2, t) -> Cdiv(cexpr e1, cexpr e2, type_match t)
(*   | Sast.SMatrixAdd(e1, e2, t) -> CMatrixAdd(cexpr e1, cexpr e2, type_match t)
(*  *)  | Sast.SMatrixMult(e1, e2, t) -> CMatrixMult(cexpr e1, cexpr e2, type_match t)
 *)  | Sast.Sexpo(e1, e2, t) -> Cexpo(cexpr e1, cexpr e2, type_match t)
  | Sast.Smod(e1, e2, t) -> Cmod(cexpr e1, cexpr e2, type_match t)
  | Sast.SfuncCall(el, t) -> CfuncCall((List.map cexpr el), type_match t)
  | Sast.Sand(e1, e2, t) -> Cand(cexpr e1, cexpr e2, type_match t)
  | Sast.Sor(e1, e2, t) -> Cor(cexpr e1, cexpr e2, type_match t)
  | Sast.Snot(e, t) -> Cnot(cexpr e, type_match t)


let rec stmt = function
  | Sast.Sstmt(e, t) -> let r = cexpr e in
                        Cstmt(r, type_match t)
  | Sast.Sblock(sl, t) -> let stmt_rev = sl in
                        let l = List.map stmt stmt_rev in
                        Cblock(List.rev l, type_match t)
(*   | Sast.SReturnBlock(sl, s, t) -> 
                          let l = List.map stmt sl in
                          CReturnBlock(l, stmt s, type_match t) *)
  | Sast.Sif(e, s1, s2, t) -> let r = cexpr e in
                              Cif(r, stmt s1, stmt s2, type_match t)
  | Sast.Sfor(id, e1, e2, s, t) -> Cfor(id, cexpr e1, cexpr e2, stmt s, Void)
  | Sast.Swhile(e, s, t) -> Cwhile(cexpr_detail e, stmt s , type_match t)
  | Sast.Sreturn(e, t) -> let r = cexpr e in
                         Creturn(r, type_match t)


let rec add_return = function
  | Cblock(sl, t) ->      
                          let last_stmt = List.nth sl (List.length sl - 1) in
                          let sl = List.tl (List.rev sl) in
                          let sl = List.rev sl in
                          let return = match last_stmt with
                              | Cstmt(e,t) -> Creturn(e, t) 
                              | Cblock(sl, t) -> let last_stmt = List.nth sl (List.length sl - 1) in
                                                 add_return last_stmt
                              | Cif(e, s1, s2, t) -> Cif(e, add_return s1, add_return s2, t)
                              | Cfor(s1, e1, e2, s2, t) -> add_return s2
                              | Cwhile(e,s,t) -> Cwhile(e, add_return s ,t)
                              | Creturn(e, t) -> Creturn(e, t)
                          in
                          Cblock(sl@[return], t)
  | _ -> failwith "Unable to identify a block"

let func_def = function
  | Sast.FunctionDef(s, frmls, b, t) -> let block = 
                                          if(type_match t != Void) then
                                            add_return (stmt b)
                                          else
                                            stmt b in
                                        let func_det = 
                                                      {
                                                        fname = s;
                                                        formals = List.map cexpr_detail frmls;
                                                        body = block
                                                      } in
                                        CFunctionDef(func_det, type_match t)

 
 
let program sast = 
  let functions = List.map func_def (fst sast) in
  let main = CFunctionDef({
                 fname = "main";
                 formals = [];
                 body = Cblock(List.rev (List.map stmt (snd sast)), Void)
              }, Int) in
  functions@[main]

