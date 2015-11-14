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
	ExprDet(l) -> 
		print_endline "intlit";
		(match l with
			| IntLit(l) -> string_of_int l
		)
	| OpDet(op) -> 
		print_endline "op"; 
		(match op with
			| IntBinop(op) -> string_of_op op
		) in

let rec compile_expr = function
	Sast.Sexpr(e, t) -> compile_detail e
	| Sast.Sbinop(e1, op, e2) -> 
		print_endline "sbinop";
		compile_expr e1 ^ " " ^
		(match op with
			| Operator(o, t) -> compile_detail (OpDet(o))
		) ^
		compile_expr e2 in

(* let rec compile_stmt_detail = function
	Expr(e) -> compile_expr e in

let rec compile_stmt = function
	Sstmt(s, t) -> compile_stmt_detail s in *)

let compile sast =
	let string_list = List.map compile_expr sast in
	String.concat "" string_list in

print_endline (compile sast)











