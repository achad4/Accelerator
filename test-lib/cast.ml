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

type id = 
  | Id of string

type cexpr_detail = 
   | Na of string * ct
   | IdLit of string
   | IntLit of int
   | IntExpr of cexpr_detail * ct
   | FloatLit of float
   | BoolLit of bool
   | StringLit of string
   | Vector of cexpr_detail * cexpr_detail list * ct
   | VectIdAcc of cexpr_detail * cexpr_detail * ct
   | VectIntAcc of cexpr_detail * cexpr_detail * ct
   | Matrix of cexpr_detail * cexpr_detail list * cexpr_detail 
             * cexpr_detail * ct
   | MatrixIntAcc of cexpr_detail * cexpr_detail * cexpr_detail * ct
   | MatrixIdAcc of cexpr_detail * cexpr_detail * cexpr_detail * ct
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
   | FuncCall of id * cexpr_detail list * ct
   | Assign of id * cexpr_detail * ct
   | And of cexpr_detail * cexpr_detail * ct
   | Or of cexpr_detail * cexpr_detail * ct
   | Not of cexpr_detail * ct
   | Eq of cexpr_detail * cexpr_detail * ct
   | NEq of cexpr_detail * cexpr_detail * ct
   | StringEq of cexpr_detail * cexpr_detail * ct
   | StringNEq of cexpr_detail * cexpr_detail * ct

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
  | Cfor of cexpression * cexpression * cexpression * statement * ct

type func_decl_detail = {
    fname : string;
    formals : string list;
    body : statement list;
}

type func_decl = 
  func_decl_detail * ct

type program = 
  func_decl list


let string_of_ctype = function
  | String -> "string"
  | Float -> "float"
  | Int -> "int"
  | Bool -> "bool"
  | Void -> "Na"

let string_of_id = function
  Id(s) -> s 

let type_match = function
  | Sast.String -> String
  | Sast.Int -> Int
  | Sast.Float -> Float
  | Sast.Bool -> Bool
  | Sast.Na -> Void

let id_match = function
  | Sast.Id(s) -> Id(s)

let rec cexpr_detail = function
 | Sast.IdLit(s) ->  IdLit(s)
 | Sast.IntLit(i) -> IntLit(i)
 | Sast.FloatLit(i) -> FloatLit(i)
 | Sast.BoolLit(b) -> BoolLit(b)
 | Sast.StringLit(s) -> StringLit(s)
 | Sast.IntExpr(e,t) -> IntExpr(cexpr_detail e, type_match t)
 | Sast.Vector(s, v, t) -> let ct = type_match t in
         Vector(cexpr_detail s, List.map cexpr_detail v, ct)
 | Sast.VectIdAcc(e1, e2, t) ->
        VectIdAcc(cexpr_detail e1, cexpr_detail e2, type_match t)
 | Sast.VectIntAcc(e1, e2, t) ->
        VectIntAcc(cexpr_detail e1, cexpr_detail e2, type_match t) 
 | Sast.Na(t) -> Na("Void", type_match t)
 | Sast.Matrix(s, v, nr, nc, t) -> let ct = type_match t in
        Matrix(cexpr_detail s, List.map cexpr_detail v, cexpr_detail nr, 
        cexpr_detail nc, ct)
 | Sast.MatrixIdAcc(s, id1, id2, t) ->
        MatrixIdAcc(cexpr_detail s, cexpr_detail id1, cexpr_detail id2,
        type_match t)
 | Sast.MatrixIntAcc(s, i1, i2, t) ->
        MatrixIntAcc(cexpr_detail s, cexpr_detail i1, cexpr_detail i2, 
        type_match t)
 | Sast.Add(e1, e2, t) -> Add((cexpr_detail e1), (cexpr_detail e2), Int)
 | Sast.Sub(e1, e2, t) -> Sub(cexpr_detail e1, cexpr_detail e2, Int)
 | Sast.Mult(e1, e2, t) -> Mult(cexpr_detail e1, cexpr_detail e2, Int)
 | Sast.Div(e1, e2, t) -> Div(cexpr_detail e1, cexpr_detail e2, Int)
 | Sast.FAdd(e1, e2, t) -> Add((cexpr_detail e1), (cexpr_detail e2), Float)
 | Sast.FSub(e1, e2, t) -> Sub(cexpr_detail e1, cexpr_detail e2, Float)
 | Sast.FMult(e1, e2, t) -> Mult(cexpr_detail e1, cexpr_detail e2, Float)
 | Sast.FDiv(e1, e2, t) -> Div(cexpr_detail e1, cexpr_detail e2, Float)
 | Sast.Expo(e1, e2, t) -> Expo(cexpr_detail e1, cexpr_detail e2, Int)
 | Sast.Mod(e1, e2, t) -> Mod(cexpr_detail e1, cexpr_detail e2, Int)
 | Sast.FuncCall(id, el, t) -> let ct = type_match t in
                               FuncCall(id_match id, 
                               List.map cexpr_detail el, ct)
 | Sast.Assign(id, e, t) -> let ct = type_match t in
                             Assign(id_match id, cexpr_detail e, ct)
 | Sast.And(e1, e, t) -> let ct = type_match t in 
                          And(cexpr_detail e1, cexpr_detail e, ct)
 | Sast.Or(e1, e2, t) ->  let ct = type_match t in
                          Or(cexpr_detail e1, cexpr_detail e2, ct)
 | Sast.Not(e, t) -> let ct = type_match t in
                      Not(cexpr_detail e, ct)
 | Sast.Eq(e1, e2, t) -> let ct = type_match t in
                            Eq(cexpr_detail e1, cexpr_detail e2, ct)
 | Sast.NEq(e1, e2, t) -> let ct = type_match t in
                            NEq(cexpr_detail e1, cexpr_detail e2, ct)
 | Sast.StringEq(e1, e2, t) -> let ct = type_match t in
                            StringEq(cexpr_detail e1, cexpr_detail e2, ct)
 | Sast.StringNEq(e1, e2, t) -> let ct = type_match t in
                            StringNEq(cexpr_detail e1, cexpr_detail e2, ct)

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
  | Sast.Sexpo(e1, e2, t) -> Cexpo(cexpr e1, cexpr e2, type_match t)
  | Sast.Smod(e1, e2, t) -> Cmod(cexpr e1, cexpr e2, type_match t)
  | Sast.SfuncCall(el, t) -> CfuncCall((List.map cexpr el), type_match t)
  | Sast.Sassign(e, t) -> Cassign(cexpr e, type_match t)
  | Sast.Sand(e1, e2, t) -> Cand(cexpr e1, cexpr e2, type_match t)
  | Sast.Sor(e1, e2, t) -> Cor(cexpr e1, cexpr e2, type_match t)
  | Sast.Snot(e, t) -> Cnot(cexpr e, type_match t)

let rec stmt = function
  | Sast.Sstmt(e, t) -> let r = cexpr e in
                        Cstmt(r, type_match t)
  | Sast.Sblock(sl, t) -> let l = List.map stmt sl in
                        Cblock(l, type_match t)
  | Sast.Sif(e, s1, s2, t) -> let r = cexpr e in
                              Cif(r, stmt s1, stmt s2, type_match t)
  | Sast.Sfor(id, e1, e2, s, t) -> Cfor(cexpr id, cexpr e1, cexpr e2, stmt s, 
                                        Void)
 
 (*return a c program in the form of a single *)
let program sast = 
  [({
    fname = "main";
    formals = [];
    body = List.map stmt sast
  }, Int)]

