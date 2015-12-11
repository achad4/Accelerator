open Parser
open Scanner
open Ast
open Sast
open Cast

let _ =
  let lexbuf = Lexing.from_channel stdin in
  let program = Parser.program Scanner.token lexbuf in
  let sast = Sast.program program in
  let cast = Cast.program sast in



let rec compile_detail = function
  | Cast.IdLit(s) -> s
	| Cast.IntLit(i) -> string_of_int i
  | Cast.FloatLit(f) -> string_of_float f
  | Cast.BoolLit(b) -> string_of_bool b
  | Cast.IntVector(s, iv, t) -> let helper e = compile_detail e in
          let holder = s ^ "holder" in
          "int " ^ holder ^ "[] = {" ^ 
          (String.concat ", " (List.map helper iv)) ^ "};\n" ^
          "vector<int> " ^ s ^ " (" ^ holder ^ ", " ^ holder ^
          " + sizeof(" ^ holder ^ ") / sizeof(int))"

  | Cast.Na(s, t) -> s
	| Cast.FuncCall(id, el, t) -> let helper e = compile_detail e in
		"cout << " ^ String.concat "" (List.map helper el) ^ "; cout << endl"
  | Cast.And(b1, b2, t) -> (compile_detail b1) ^ " && " ^ (compile_detail b2)
  | Cast.Or(b1, b2, t) -> (compile_detail b1) ^ " || " ^ (compile_detail b2)
  | Cast.Not(b1, t) -> "! " ^ (compile_detail b1)
  | Cast.Assign(id, e, t) -> "int " ^ (string_of_id id) ^ "=" ^ (compile_detail e)
  | Cast.Mod(e1, e2, t) -> (compile_detail e1) ^ " % " ^ (compile_detail e2) 
  | Cast.Expo(e1, e2, t) -> "pow(" ^ (compile_detail e1) ^ ",  " ^ (compile_detail e2) ^ ")"
  | Cast.Div(e1, e2, t) -> (compile_detail e1) ^ " / " ^ (compile_detail e2)
  | Cast.Mult(e1, e2, t) -> (compile_detail e1) ^ " * " ^ (compile_detail e2)
  | Cast.Sub(e1, e2, t) -> (compile_detail e1) ^ " - " ^ (compile_detail e2) 
  | Cast.Add(e1, e2, t) -> (compile_detail e1) ^ " + " ^ (compile_detail e2)
  | Cast.FAdd(e1, e2, t) -> (compile_detail e1) ^ " + " ^ (compile_detail e2)
  | Cast.FSub(e1, e2, t) -> (compile_detail e1) ^ " - " ^ (compile_detail e2)
  | Cast.FMult(e1, e2, t) -> (compile_detail e1) ^ " * " ^ (compile_detail e2)
  | Cast.FDiv(e1, e2, t) -> (compile_detail e1) ^ " / " ^ (compile_detail e2) in

let rec compile_expr = function
	| Cexpr(e, t) -> compile_detail e 
	| CfuncCall(el, t) -> String.concat "" (List.map compile_expr el)
	| Cassign(e, t) -> (string_of_ctype t) ^ compile_expr e
  | Cmod(e1, e2, t) -> compile_expr e1 ^ compile_expr e2 
  | Cexpo(e1, e2, t) -> compile_expr e1 ^ compile_expr e2
  | Cdiv(e1, e2, t) -> compile_expr e1 ^ compile_expr e2 
  | Cmult(e1, e2, t) -> compile_expr e1 ^ compile_expr e2
  | Csub(e1, e2, t) -> compile_expr e1 ^ compile_expr e2 
	| Cadd(e1, e2, t) -> compile_expr e1 ^ compile_expr e2 
  | CFAdd(e1, e2, t) -> compile_expr e1 ^ compile_expr e2  
  | CFSub(e1, e2, t) -> compile_expr e1 ^ compile_expr e2 
  | CFMult(e1, e2, t) -> compile_expr e1 ^ compile_expr e2 
  | CFDiv(e1, e2, t) -> compile_expr e1 ^ compile_expr e2 
  | Cand(b1, b2, t) -> compile_expr b1 ^ compile_expr b2 
  | Cor(b1, b2, t) -> compile_expr b1 ^ compile_expr b2
  | Cnot(b1, t) -> (compile_expr b1) in




(*
let string_list l = List.map string_of_statement l in
                          "if(" ^ (string_of_expression e) ^ "){" ^ (String.concat "" (string_list sl1)) ^ 
                          "}else{" ^ (String.concat "" (string_list sl1)) ^ "}"
*)
let rec compile_stmt = function
  | Cstmt(e, t) -> compile_expr e ^ ";"
  | Cblock(sl, t) -> let string_list l = List.map compile_stmt l in
                  "{\n" ^ String.concat "" (string_list sl) ^ "}\n"
  | Cif(e, s1, s2, t) ->   "if(" ^ compile_expr e ^ ")" 
                           ^ (compile_stmt s1)  ^ 
                           "else" 
                           ^  (compile_stmt s2)
  (*for(int i = start; i < end; i++)*)
  | Cfor(id, e1, e2, s, t) -> "for( int " ^ compile_expr id ^ "="  ^ compile_expr e1 ^ "; " ^ "i <= " ^ 
                          compile_expr e2 ^ "; i++)\n" ^
                          compile_stmt s in

let compile_func (f, t) = 
  let stmt_string_list = List.map compile_stmt (List.rev f.body) in
    string_of_ctype t ^ " " ^ f.fname
    ^ "(" ^ String.concat "," f.formals ^ ")\n{\n"
    ^ String.concat "; " stmt_string_list
    ^ "\n}" in



let compile cast = 
  let string_list = List.map compile_func cast in
  String.concat "" string_list in

  let c_begin = "#include<iostream>\n
                 #include<stdio.h>\n
                 #include<math.h>\n
                 #include<vector>\n
                 using namespace std;\n" in

print_endline ( c_begin ^ (compile cast))
