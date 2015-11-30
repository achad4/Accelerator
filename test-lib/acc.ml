open Parser
open Scanner
open Ast
open Sast

let _ =
  let lexbuf = Lexing.from_channel stdin in
  let program = Parser.program Scanner.token lexbuf in
  let sast = Sast.program program in

let rec compile_detail = function
  | IdLit(s) -> s
  | IntLit(l) -> string_of_int l
  | FloatLit(f) -> string_of_float f
  | BoolLit(b) -> string_of_bool b
  | FuncCall(id, el) -> let helper e = compile_detail e in
		"cout << " ^ String.concat "" (List.map helper el) ^ "; cout << endl;"
  | And(b1, b2) -> (compile_detail b1) ^ " && " ^ (compile_detail b2)
  | Or(b1, b2) -> (compile_detail b1) ^ " || " ^ (compile_detail b2)
  | Not(b1) -> "! " ^ (compile_detail b1)
  | Assign(id, e) -> "int " ^ (string_of_id id) ^ "=" ^ (compile_detail e)
  | Mod(e1, e2) -> (compile_detail e1) ^ " % " ^ (compile_detail e2) 
  | Expo(e1, e2) -> "pow(" ^ (compile_detail e1) ^ ",  " ^ (compile_detail e2) ^ ")"
  | Div(e1, e2) -> (compile_detail e1) ^ " / " ^ (compile_detail e2)
  | Mult(e1, e2) -> (compile_detail e1) ^ " * " ^ (compile_detail e2)
  | Sub(e1, e2) -> (compile_detail e1) ^ " - " ^ (compile_detail e2) 
  | Add(e1, e2) -> (compile_detail e1) ^ " + " ^ (compile_detail e2) in

let rec compile_expr = function
	| Sexpr(e, t) -> compile_detail e ^ ";" 
	| SfuncCall(el, t) -> String.concat "" (List.map compile_expr el)
	| Sassign(e, t) -> print_endline ("here: " ^ (string_of_type t)); (string_of_type t) ^ compile_expr e
  | Smod(e1, e2) -> compile_expr e1 ^ compile_expr e2 
  | Sexpo(e1, e2) -> compile_expr e1 ^ compile_expr e2
  | Sdiv(e1, e2) -> compile_expr e1 ^ compile_expr e2 
  | Smult(e1, e2) -> compile_expr e1 ^ compile_expr e2
  | Ssub(e1, e2) -> compile_expr e1 ^ compile_expr e2 
	| Sadd(e1, e2) -> compile_expr e1 ^ compile_expr e2 
  | Sand(b1, b2) -> compile_expr b1 ^ compile_expr b2 
  | Sor(b1, b2) -> compile_expr b1 ^ compile_expr b2
  | Snot(b1) -> (compile_expr b1) in

let compile sast =
	let string_list = List.map compile_expr sast in
    let rev_list = List.rev string_list in
	String.concat "" rev_list ^ ";" in

  let c_begin = "#include<iostream>\n
                 #include<stdio.h>\n
                 #include<math.h>\n
                 using namespace std;\n 
                 int main () {\n " in
let c_end = "}" in
print_endline ( c_begin ^ (compile sast) ^ c_end)
