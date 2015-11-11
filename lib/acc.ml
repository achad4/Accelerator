open Parser
open Scanner
open Ast

let _ =
  let lexbuf = Lexing.from_channel stdin in
  let program = Parser.program Scanner.token lexbuf in
  let stmts = fst program in
  let decls = snd program in
  print_int (List.length stmts);
  print_int (List.length decls);
  print_endline;
  let print_stmt stmt = 
  	print_endline "here";
  	print_endline (string_of_stmt stmt) in
  print_endline "sanity check";

  List.iter print_stmt stmts;





(*   let str = read_line () in
	let e = Parser.program Scanner.token (Lexing.from_string str) in
	let n = Eval.eval e in
	print_endline (string_of_int n) *)
  