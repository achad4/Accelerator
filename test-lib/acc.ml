open Parser
open Scanner
open Ast


let translate program = 
    let translate expr = 
      IntLit i -> [IntLit i] in

let _ =
  let lexbuf = Lexing.from_channel stdin in
  let program = Parser.program Scanner.token lexbuf in
  print_endline (string_of_program program)



