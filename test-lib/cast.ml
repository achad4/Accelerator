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
  | Bool
  | Void

type id = 
  | Id of string

type cexpr_detail = 
   | IdLit of string
   | IntLit of int
   | BoolLit of bool
   | Add of cexpr_detail * cexpr_detail * ct
   | Sub of cexpr_detail * cexpr_detail * ct
   | Mult of cexpr_detail * cexpr_detail * ct
   | Div of cexpr_detail * cexpr_detail * ct
   | Expo of cexpr_detail * cexpr_detail * ct
   | Mod of cexpr_detail * cexpr_detail * ct
   | FuncCall of id * cexpr_detail list * ct
   | Assign of id * cexpr_detail * ct
   | And of cexpr_detail * cexpr_detail * ct
   | Or of cexpr_detail * cexpr_detail * ct
   | Not of cexpr_detail * ct

type cexpression = 
  | Cexpr of cexpr_detail * ct
  | Cadd of cexpression * cexpression * ct
  | Csub of cexpression * cexpression * ct
  | Cmult of cexpression * cexpression * ct
  | Cdiv of cexpression * cexpression * ct
  | Cexpo of cexpression * cexpression * ct
  | Cmod of cexpression * cexpression * ct
  | CfuncCall of cexpression list * ct
  | Cassign of cexpression * ct
  | Cand of cexpression * cexpression * ct
  | Cor of cexpression * cexpression * ct
  | Cnot of cexpression * ct

type statement = 
  Cstmt of cexpression * ct

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
  | Int -> "int"
  | Bool -> "bool"
  | Void -> "Na"

let string_of_id = function
  Id(s) -> s 

let type_match = function
  | Sast.String -> String
  | Sast.Int -> Int
  | Sast.Bool -> Bool
  | Sast.Na -> Void

let id_match = function
  | Sast.Id(s) -> Id(s)

let rec cexpr_detail = function
 | Sast.IdLit(s) ->  IdLit(s)
 | Sast.IntLit(i) -> IntLit(i)
 | Sast.BoolLit(b) -> BoolLit(b)
 (*Expand when you pull in Alan's Fadd etc.*)
 | Sast.Add(e1, e2, t) -> Add((cexpr_detail e1), (cexpr_detail e2), Int)
 | Sast.Sub(e1, e2, t) -> Sub(cexpr_detail e1, cexpr_detail e2, Int)
 | Sast.Mult(e1, e2, t) -> Mult(cexpr_detail e1, cexpr_detail e2, Int)
 | Sast.Div(e1, e2, t) -> Div(cexpr_detail e1, cexpr_detail e2, Int)
 | Sast.Expo(e1, e2, t) -> Expo(cexpr_detail e1, cexpr_detail e2, Int)
 | Sast.Mod(e1, e2, t) -> Mod(cexpr_detail e1, cexpr_detail e2, Int)
 | Sast.FuncCall(id, el, t) -> 
                              let ct = type_match t in

                              FuncCall(id_match id, List.map cexpr_detail el, ct)
 | Sast.Assign(id, e, t) -> 
                            let ct = type_match t in
                             Assign(id_match id, cexpr_detail e, ct)
 | Sast.And(e1, e, t) -> let ct = type_match t in 
                          And(cexpr_detail e1, cexpr_detail e, ct)
 | Sast.Or(e1, e2, t) ->  let ct = type_match t in
                          Or(cexpr_detail e1, cexpr_detail e2, ct)
 | Sast.Not(e, t) -> let ct = type_match t in
                      Not(cexpr_detail e, ct)


let rec cexpr = function
  | Sast.Sexpr(e, t) -> Cexpr(cexpr_detail e, type_match t)
      
(*   | Sast.Sadd(e1, e2) -> 
      cexpr e1, 
      cexpr e2
  | Sast.Ssub(e1, e2) -> cexpr e1, cexpr e2
  | Sast.Smult(e1, e2) -> cexpr e1, cexpr e2
  | Sast.Sdiv(e1, e2) -> cexpr e1, cexpr e2
  | Sast.Sexpo(e1, e2) -> cexpr e1, cexpr e2
  | Sast.Smod(e1, e2) -> cexpr e1, cexpr e2
  | Sast.SfuncCall(el, t) -> (List.map cexpr e), t
  | Sast.Sassign(e, t) -> cexpr e, t
  | Sast.Sand(e1, e2) -> cexpr e1, cexpr e2
  | Sast.Sor(e1, e2) -> cexpr e1, cexpr e2
  | Sast.Snot(e) -> cexpr e *)



(* let stmt_detail = function
  Sast.Expr( e ) -> 
   print_endline (Ast.string_of_cexpression e);
   let r = cexpr e in
    ( (fst r), (snd r) ), (snd r) *)

let stmt = function
  Sast.Sstmt(e, t) ->
    let r = cexpr e in
    Cstmt(r, type_match t)
 
 (*return a c program in the form of a single *)
let program sast = 
  [({
    fname = "main";
    formals = [];
    body = List.map stmt sast
  }, Int)]








