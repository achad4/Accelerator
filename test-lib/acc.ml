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
	| FuncCall(id, el) -> 
		
		let helper e =
			compile_detail e in

		"cout << " ^ String.concat "" (List.map helper el) ^ ";"
	| Assign(id, e) -> (string_of_id id) ^ "=" ^compile_detail e ^ ";"
	| Add(e1, e2) -> 
		(compile_detail e1) ^ " + " ^ (compile_detail e2) in


let rec compile_expr = function
	Sexpr(e, t) -> compile_detail e
	| SfuncCall(el, t) -> String.concat "" (List.map compile_expr el)
	| Sassign(e, t) -> print_endline ("here: " ^ (string_of_type t)); (string_of_type t) ^ compile_expr e
	| Sadd(e1, e2) ->
		compile_expr e1 ^
		compile_expr e2 in

let compile sast =
	let string_list = List.map compile_expr sast in
	String.concat "" string_list in

let c_begin = "#include<iostream>\n using namespace std; int main () { " in
let c_end = "}" in
print_endline ( c_begin ^ (compile sast) ^ c_end)

