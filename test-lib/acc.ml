open Parser
open Scanner
open Ast
open Sast

let _ =
  let lexbuf = Lexing.from_channel stdin in
  let program = Parser.program Scanner.token lexbuf in
(*   print_endline (string_of_program program);
 *)  let sast = Sast.program program in



let rec compile_detail = function
	IntLit(l) -> 
		(*print_endline "intlit";*)
		string_of_int l
	| FuncCall(id, e) -> "cout << " ^ compile_detail e ^ " endl;"
	| Add(e1, e2) -> 
		(compile_detail e1) ^ " + " ^ (compile_detail e2) in


let rec compile_expr = function
	Sexpr(e, t) -> compile_detail e
	| SfuncCall(e, t) -> compile_expr e
	| Sadd(e1, e2) ->
		compile_expr e1 ^
		compile_expr e2 in


(* 	| Sast.Sbinop(e1, op, e2) -> 
		print_endline "sbinop";
		compile_expr e1 ^ " " ^
		(match op with
			| Operator(o, t) -> compile_detail (OpDet(o))
		) ^
		compile_expr e2 in *)

(* let rec compile_stmt_detail = function
	Expr(e) -> compile_expr e in

let rec compile_stmt = function
	Sstmt(s, t) -> compile_stmt_detail s in *)

let compile sast =
	let string_list = List.map compile_expr sast in
	String.concat "" string_list in

let c_begin = "int main () { " in
let c_end = "}" in
print_endline ( c_begin ^ (compile sast) ^ c_end)

