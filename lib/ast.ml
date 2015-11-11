
type dual_op = Add | Sub | Mult | Div | Mod | Exp | Assign | Equal | Neq | Geq | Leq | Gthan | Lthan | Range | And | Or
type sing_op = Not

type vector = 
	| IntVector of int list
	| DoubleVector of float list
	| BoolVector of bool list
	| CharacterVector of string list
	| CharVector of char list

type matrix =
	| Matrix of vector list

type expr =
 	| IntLit of int
	| DoubleLit of float
	| BoolLit of bool
	| Character of string
	| Na
	| Id of string
	| DualOp of expr * dual_op * expr
	| SingOp of sing_op * expr
	| Assign of string * expr
	| MatrixAccElem of string * expr * expr
	| MatrixAccRow of string * expr
    | MatrixAccCol of string * expr
    | FuncCall of string * expr list
	| Noexpr

type stmt =
	| Expr of expr
	| If of expr * stmt * stmt
	| Block of stmt list
	| For of string * expr * stmt
	| Next
	| Break

type func_decl = {
    fname : string;
    formals : string list;
    body : stmt list;
  }

type program = stmt list * func_decl list 

let rec string_of_expr = function
    IntLit(l) -> string_of_int l
  | Id(s) -> s
  | Noexpr -> ""

let rec string_of_stmt = function
    Block(stmts) ->
      "{\n" ^ String.concat "" (List.map string_of_stmt stmts) ^ "}\n"
  | Expr(expr) -> print_endline "expr"; string_of_expr expr ^ ";\n";
  | If(e, stmt, Block([])) -> "if (" ^ string_of_expr e ^ ")\n" ^ string_of_stmt stmt
  | If(e, stmt1, stmt2) ->  "if (" ^ string_of_expr e ^ ")\n" ^
      string_of_stmt stmt1 ^ "else\n" ^ string_of_stmt stmt2
  | For(s, e, stmt) ->
      "for (" ^ s  ^ " ; " ^ string_of_expr e ^ " ; " ^
      string_of_stmt stmt  ^ ") " ^ string_of_stmt stmt
