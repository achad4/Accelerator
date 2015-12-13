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

type expr =
  | Id of string
	| IntLit of int
  | BoolLit of bool
  | FloatLit of float
  | Na
  | None
  | Vector of string * expr list
  | VectIdAcc of string * string
  | VectIntAcc of string * expr
  | Matrix of string * expr list * expr * expr
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

type program = stmt list

let rec string_of_expression = function
  | Id(s) -> s
  | Na -> "null"
  | IntLit(e) -> string_of_int e
  | BoolLit(b) -> string_of_bool b
  | FloatLit(f) -> string_of_float f
  | Vector(s, vl) -> "vector<type> " ^ s ^ "(" ^ (String.concat ", " (List.map string_of_expression vl)) ^ ")"
  | VectIdAcc(s, accid) -> s ^ "[" ^  accid ^ "]"
  | VectIntAcc(s, ind) -> s ^ "[" ^ string_of_expression ind ^ "]"
  | Matrix(s, v, nr, nc) -> "vector<vector<type>>" ^ s ^ "("  ^ (String.concat ", " (List.map string_of_expression v)) ^ ")" ^
    " with nrow=" ^ string_of_expression nr ^ " and ncol=" ^ string_of_expression nc
  | MatrixIntAcc(s, ind1, ind2) -> s ^ "[" ^ string_of_expression ind1 ^ "]" ^ "[" ^ string_of_expression ind2 ^ "]"
  | Add(e1, e2) -> (string_of_expression e1) ^ "+" ^ (string_of_expression e2)
  | Sub(e1, e2 ) -> (string_of_expression e1) ^ "-" ^ (string_of_expression e2)
  | Mult(e1, e2) -> (string_of_expression e1) ^ "*" ^ (string_of_expression e2)
  | Div(e1, e2) -> (string_of_expression e1) ^ "/" ^ (string_of_expression e2)
  | Expo(e1, e2) -> (string_of_expression e1) ^ "^" ^ (string_of_expression e2)
  | Mod(e1, e2) -> (string_of_expression e1) ^ "%" ^ (string_of_expression e2)
  | FMult(f1, f2) -> (string_of_expression f1) ^ "*" ^ (string_of_expression f1)
  | FAdd(f1, f2) -> (string_of_expression f1) ^ "+" ^ (string_of_expression f2)
  | FSub(f1, f2) -> (string_of_expression f1) ^ "-" ^ (string_of_expression f2)
  | FDiv(f1, f2) -> (string_of_expression f1) ^ "/" ^ (string_of_expression f2)
  | FuncCall( s, e ) -> "function call " ^ s
  | Assign( s, e ) -> "assign " ^ s ^ " = " ^ (string_of_expression e)
  | And( b1, b2) -> (string_of_expression b1) ^ " && " ^ (string_of_expression b2)
  | Or( b1, b2) -> (string_of_expression b1) ^ " || " ^ (string_of_expression b2)
  | Not( b1 ) -> "!" ^ (string_of_expression b1)
  | None -> "None"
    
let rec string_of_statement = function
  | Expr(e) -> string_of_expression e
  | Block(sl) -> let string_list l = List.map string_of_statement l in
                "{\n" ^ String.concat "" (string_list sl) ^ "}\n"
  | If(e, s1, s2) -> "if(" ^ (string_of_expression e) ^ ")" ^ 
                     string_of_statement s1 ^ 
                     "else" ^ string_of_statement s2
  | For (e, ie1, ie2, sl) -> 
          "for(" ^ string_of_expression e ^
          " in " ^ string_of_expression ie1 ^
          ":" ^ string_of_expression ie2 ^
          "){\n" ^ string_of_statement sl
                
let string_of_program program =
	String.concat "\n" (List.map string_of_statement program)
