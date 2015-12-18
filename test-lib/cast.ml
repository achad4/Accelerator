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

type ct = 
  | String
  | Int
  | Float
  | Bool
  | Void
  | Vector of ct
  | Matrix of ct
  | IdType

type id = 
  | Id of string

type cexpr_detail = 
   | Na of string * ct
   | IdLit of string * ct
   | IntLit of int
   | IntExpr of cexpr_detail * ct
   | FloatLit of float
   | BoolLit of bool
   | StringLit of string
   | Vector of string * cexpr_detail list * ct
   | VectIdAcc of string * string * ct
   | VectIntAcc of string * cexpr_detail * ct
   | Matrix of string * cexpr_detail list * cexpr_detail * cexpr_detail * ct
   | MatrixIntAcc of string * cexpr_detail * cexpr_detail * ct
   | MatrixIdAcc of string * string * string * ct
   | Add of cexpr_detail * cexpr_detail * ct
   | Sub of cexpr_detail * cexpr_detail * ct
   | Mult of cexpr_detail * cexpr_detail * ct
   | Div of cexpr_detail * cexpr_detail * ct
   | FAdd of cexpr_detail * cexpr_detail * ct
   | FSub of cexpr_detail * cexpr_detail * ct
   | FMult of cexpr_detail * cexpr_detail * ct
   | FDiv of cexpr_detail * cexpr_detail * ct
   | Expo of cexpr_detail * cexpr_detail * ct
   | Mod of cexpr_detail * cexpr_detail * ct
   | FuncCall of string * cexpr_detail list * ct
   | Assign of string * cexpr_detail * ct
   | And of cexpr_detail * cexpr_detail * ct
   | Or of cexpr_detail * cexpr_detail * ct
   | Not of cexpr_detail * ct

type cexpression = 
  | Cexpr of cexpr_detail * ct
  | Cadd of cexpression * cexpression * ct
  | Csub of cexpression * cexpression * ct
  | Cmult of cexpression * cexpression * ct
  | Cdiv of cexpression * cexpression * ct
  | CFAdd of cexpression * cexpression * ct
  | CFSub of cexpression * cexpression * ct
  | CFMult of cexpression * cexpression * ct
  | CFDiv of cexpression * cexpression * ct
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
  | Cif of cexpression * statement * statement * ct
  | Cfor of string * cexpression * cexpression * statement * ct

type func_decl_detail = {
    fname : string;
    formals : string list;
    body : statement list;
}

type func_decl = 
  func_decl_detail * ct

type program = 
  func_decl list

let rec string_of_ctype = function
  | String -> "string"
  | Float -> "float"
  | Int -> "int"
  | Bool -> "bool"
  | Void -> "Na"
  | IdType -> "IdType"
  | Vector(t) -> "Vector of " ^ string_of_ctype t
  | Matrix(t) -> "Matrix of " ^ string_of_ctype t

let string_of_id = function
  Id(s) -> s 

let rec type_match = function
  | Environment.String -> String
  | Environment.Int -> Int
  | Environment.Float -> Float
  | Environment.Bool -> Bool
  | Environment.Na -> Void
  | Environment.IdType -> IdType
  | Environment.Vector(t) -> Vector(type_match t)
  | Environment.Matrix(t) -> Vector(type_match t)

let id_match = function
  | Environment.Id(s) -> Id(s)

let rec cexpr_detail = function
 | Environment.IdLit(s, t) ->  IdLit(s, type_match t)
 | Environment.IntLit(i) -> IntLit(i)
 | Environment.FloatLit(i) -> FloatLit(i)
 | Environment.BoolLit(b) -> BoolLit(b)
 | Environment.StringLit(s) -> StringLit(s)
 | Environment.IntExpr(e,t) -> IntExpr(cexpr_detail e, type_match t)
 | Environment.Vector(s, v, t) -> let ct = type_match t in
         Vector(s, List.map cexpr_detail v, ct)
 | Environment.VectIdAcc(e1, e2, t) ->
        VectIdAcc(e1, e2, type_match t)
 | Environment.VectIntAcc(e1, e2, t) ->
        VectIntAcc(e1, cexpr_detail e2, type_match t) 
 | Environment.NaExpr(t) -> Na("Void", type_match t)
 | Environment.Matrix(s, v, nr, nc, t) -> let ct = type_match t in
        Matrix(s, List.map cexpr_detail v, cexpr_detail nr, cexpr_detail nc, ct)
 | Environment.MatrixIdAcc(s, id1, id2, t) ->
        MatrixIdAcc(s, id1, id2, type_match t)
 | Environment.MatrixIntAcc(s, i1, i2, t) ->
        MatrixIntAcc(s, cexpr_detail i1, cexpr_detail i2, type_match t)
 (*Expand when you pull in Alan's Fadd etc.*)
 | Environment.Add(e1, e2, t) -> Add((cexpr_detail e1), (cexpr_detail e2), Int)
 | Environment.Sub(e1, e2, t) -> Sub(cexpr_detail e1, cexpr_detail e2, Int)
 | Environment.Mult(e1, e2, t) -> Mult(cexpr_detail e1, cexpr_detail e2, Int)
 | Environment.Div(e1, e2, t) -> Div(cexpr_detail e1, cexpr_detail e2, Int)
 | Environment.FAdd(e1, e2, t) -> Add((cexpr_detail e1), (cexpr_detail e2), Float)
 | Environment.FSub(e1, e2, t) -> Sub(cexpr_detail e1, cexpr_detail e2, Float)
 | Environment.FMult(e1, e2, t) -> Mult(cexpr_detail e1, cexpr_detail e2, Float)
 | Environment.FDiv(e1, e2, t) -> Div(cexpr_detail e1, cexpr_detail e2, Float)
 | Environment.Expo(e1, e2, t) -> Expo(cexpr_detail e1, cexpr_detail e2, Int)
 | Environment.Mod(e1, e2, t) -> Mod(cexpr_detail e1, cexpr_detail e2, Int)
 | Environment.FuncCall(id, el, t) -> let ct = type_match t in
                               FuncCall(id, 
                               List.map cexpr_detail el, ct)
 | Environment.Assign(id, e, t) -> let ct = type_match t in
                             Assign(id, cexpr_detail e, ct)
 | Environment.And(e1, e, t) -> let ct = type_match t in 
                          And(cexpr_detail e1, cexpr_detail e, ct)
 | Environment.Or(e1, e2, t) ->  let ct = type_match t in
                          Or(cexpr_detail e1, cexpr_detail e2, ct)
 | Environment.Not(e, t) -> let ct = type_match t in
                      Not(cexpr_detail e, ct)

let rec cexpr = function
  | Environment.Sexpr(e, t) -> Cexpr(cexpr_detail e, type_match t)
  | Environment.Sadd(e1, e2, t) -> Cadd(cexpr e1, cexpr e2, type_match t)
  | Environment.Ssub(e1, e2, t) -> Csub(cexpr e1, cexpr e2, type_match t)
  | Environment.Smult(e1, e2, t) -> Cmult(cexpr e1, cexpr e2, type_match t)
  | Environment.Sdiv(e1, e2, t) -> Cdiv(cexpr e1, cexpr e2, type_match t)
  | Environment.SFAdd(e1, e2, t) -> Cadd(cexpr e1, cexpr e2, type_match t)
  | Environment.SFSub(e1, e2, t) -> Csub(cexpr e1, cexpr e2, type_match t)
  | Environment.SFMult(e1, e2, t) -> Cmult(cexpr e1, cexpr e2, type_match t)
  | Environment.SFDiv(e1, e2, t) -> Cdiv(cexpr e1, cexpr e2, type_match t)
  | Environment.Sexpo(e1, e2, t) -> Cexpo(cexpr e1, cexpr e2, type_match t)
  | Environment.Smod(e1, e2, t) -> Cmod(cexpr e1, cexpr e2, type_match t)
  | Environment.SfuncCall(el, t) -> CfuncCall((List.map cexpr el), type_match t)
  | Environment.Sassign(e, t) -> Cassign(cexpr e, type_match t)
  | Environment.Sand(e1, e2, t) -> Cand(cexpr e1, cexpr e2, type_match t)
  | Environment.Sor(e1, e2, t) -> Cor(cexpr e1, cexpr e2, type_match t)
  | Environment.Snot(e, t) -> Cnot(cexpr e, type_match t)

let rec stmt = function
  | Environment.Sstmt(e, t) -> let r = cexpr e in
                        Cstmt(r, type_match t)
  | Environment.Sblock(sl, t) -> let l = List.map stmt sl in
                        Cblock(l, type_match t)
  | Environment.Sif(e, s1, s2, t) -> let r = cexpr e in
                              Cif(r, stmt s1, stmt s2, type_match t)
  | Environment.Sfor(id, e1, e2, s, t) -> Cfor(id, cexpr e1, cexpr e2, stmt s, Void)
 
 (*return a c program in the form of a single *)
let program cast = 
  [({
    fname = "main";
    formals = [];
    body = List.map stmt cast
  }, Int)]

