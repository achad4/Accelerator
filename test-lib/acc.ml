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
  | Cast.Na(s, t) -> s
  | Cast.IdLit(s) -> s
  | Cast.IntLit(i) -> string_of_int i
  | Cast.IntExpr(e, t) -> compile_detail e
  | Cast.FloatLit(f) -> string_of_float f
  | Cast.BoolLit(b) -> string_of_bool b
  | Cast.StringLit(s) -> " \"" ^ s ^ "\" "
  | Cast.Vector(s, v, t) -> 
          let helper e = compile_detail e in
          let holder = compile_detail s ^ "holder" in
          let ty = Cast.string_of_ctype t in
          ty ^ " " ^ holder ^ "[] = {" ^ 
          (String.concat ", " (List.map helper v)) ^ "};\n" ^
          "vector<" ^ ty ^ "> " ^ compile_detail s ^ 
          " (" ^ holder ^ ", " ^ holder ^ " + sizeof(" ^ 
          holder ^ ") / sizeof(" ^ ty ^"))"
  | Cast.VectIdAcc(e1, e2, t) -> 
          compile_detail e1 ^ "[" ^ compile_detail e2 ^ "-1]"
  | Cast.VectIntAcc(e1, e2, t) ->
          compile_detail e1 ^ "[" ^ compile_detail e2 ^ "-1]"
  | Cast.Matrix(s, v, nr, nc, t) ->
          let helper e = compile_detail e in
          let holder = compile_detail s ^ "Holder" in
          let nr_str = compile_detail nr in
          let nc_str = compile_detail nc in 
          let matrix = compile_detail s in 
          let ty = Cast.string_of_ctype t in
          ty ^ " " ^ holder ^ "[] = {" ^ 
          (String.concat ", " (List.map helper v)) ^ "};\n" ^
          "vector<" ^ ty ^ "> " ^ matrix ^ "Vector" ^
          " (" ^ holder ^ ", " ^ holder ^ " + " ^
          nc_str ^ " / sizeof(" ^ ty ^ "));\n" ^
          "vector<vector<" ^ ty ^ "> > " ^ matrix ^ " (" ^ nr_str ^ ");\n" ^
          "int count=0;\n" ^
          "for(int i=0; i<" ^ nr_str ^ "; i++) { \n" ^
          matrix ^ "[i].resize(" ^ nc_str ^ ");\n" ^
          "for(int j=0; j<" ^ nc_str ^ "; j++) { \n" ^
          matrix ^ "[i][j] = " ^ holder ^ "[count++];\n}\n}"
  | Cast.MatrixIdAcc(m, r, c, t) -> 
          compile_detail m ^ "[" ^ compile_detail r ^ "][" ^ compile_detail c ^ "]"
  | Cast.MatrixIntAcc(m, r, c, t) ->
          compile_detail m ^ "[" ^ compile_detail r ^ "][" ^ compile_detail c ^ "]"
  | Cast.FuncCall(id, el, t) -> let helper e = compile_detail e in
		"cout << " ^ String.concat "" (List.map helper el) ^ 
        "; cout << endl"
  | Cast.And(b1, b2, t) -> (compile_detail b1) ^ " && " ^ 
                           (compile_detail b2)
  | Cast.Or(b1, b2, t) -> (compile_detail b1) ^ " || " ^ 
                          (compile_detail b2)
  | Cast.Not(b1, t) -> "! " ^ (compile_detail b1)
  | Cast.Assign(id, e, t) -> string_of_ctype t ^ " " ^ (string_of_id id) ^ " = " ^ 
                             (compile_detail e)
  | Cast.Mod(e1, e2, t) -> (compile_detail e1) ^ " % " ^ 
                           (compile_detail e2) 
  | Cast.Expo(e1, e2, t) -> "pow(" ^ (compile_detail e1) ^ ",  " ^ 
                            (compile_detail e2) ^ ")"
  | Cast.Div(e1, e2, t) -> (compile_detail e1) ^ " / " ^ 
                           (compile_detail e2)
  | Cast.Mult(e1, e2, t) -> (compile_detail e1) ^ " * " ^ 
                            (compile_detail e2)
  | Cast.Sub(e1, e2, t) -> (compile_detail e1) ^ " - " ^ 
                           (compile_detail e2) 
  | Cast.Add(e1, e2, t) -> (compile_detail e1) ^ " + " ^
                           (compile_detail e2)
  | Cast.FAdd(e1, e2, t) -> (compile_detail e1) ^ " + " ^ 
                            (compile_detail e2)
  | Cast.FSub(e1, e2, t) -> (compile_detail e1) ^ " - " ^ 
                            (compile_detail e2)
  | Cast.FMult(e1, e2, t) -> (compile_detail e1) ^ " * " ^ 
                             (compile_detail e2)
  | Cast.FDiv(e1, e2, t) -> (compile_detail e1) ^ " / " ^ 
                            (compile_detail e2) in

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

let rec compile_stmt = function
  | Cstmt(e, t) -> compile_expr e ^ ";\n"
  | Cblock(sl, t) -> let string_list l = List.map compile_stmt l in
                  "{\n" ^ String.concat "" (string_list sl) ^ "}\n"
  | Cif(e, s1, s2, t) ->   "if(" ^ compile_expr e ^ ")" 
                           ^ (compile_stmt s1)  ^ 
                           "else" 
                           ^  (compile_stmt s2)
  | Cfor(id, e1, e2, s, t) -> "for( int " ^ compile_expr id ^ "="  ^ compile_expr e1 ^ "; " ^ 
                          compile_expr id ^" <= " ^ compile_expr e2 ^ "; " ^ compile_expr id ^
                          "++)\n" ^ compile_stmt s 
  | Ccsv(id, )in

let compile_func (f, t) = 
  let stmt_string_list = List.map compile_stmt (List.rev f.body) in
    string_of_ctype t ^ " " ^ f.fname
    ^ "(" ^ String.concat "," f.formals ^ ")\n{\n"
    ^ String.concat "; " stmt_string_list
    ^ "\n}" in

let compile cast = 
  let string_list = List.map compile_func cast in
  String.concat "" string_list in

  let c_begin = 
  "#include<iostream>\n"^
  "#include<stdio.h>\n" ^
  "#include<math.h>\n" ^
  "#include<vector>\n" ^
  "#include<string>\n" ^
  "using namespace std;\n" in

print_endline ( c_begin ^ (compile cast))
