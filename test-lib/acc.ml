open Parser
open Scanner
open Ast
open Sast
open Environment
open Cast 

let _ =
  let lexbuf = Lexing.from_channel stdin in
  let program = Parser.program Scanner.token lexbuf in
  let envr = Environment.program program in
  let sast = Sast.program envr in
  let cast = Cast.program sast in 

let rec compile_detail = function
  | Cast.Na(s, t) -> s
  | Cast.Id(s, t) -> s
  | Cast.IntLit(i) -> string_of_int i
  | Cast.IntExpr(e, t) -> compile_detail e
  | Cast.FloatLit(f) -> string_of_float f
  | Cast.BoolLit(b) -> string_of_bool b
  | Cast.StringLit(s) -> " \"" ^ s ^ "\" "
  | Cast.Vector(s, v, t) -> 
          let helper e = compile_detail e in
          let holder = s ^ "holder" in
          let ty = Cast.string_of_ctype t in
          ty ^ " " ^ holder ^ "[] = {" ^ 
          (String.concat ", " (List.map helper v)) ^ "};\n" ^
          "vector<" ^ ty ^ "> " ^ s ^ 
          " (" ^ holder ^ ", " ^ holder ^ " + sizeof(" ^ 
          holder ^ ") / sizeof(" ^ ty ^"))"
  (* What is this minus 1 stuff and do we need it for vectors too? *)
  | Cast.VectAcc(e1, e2, t) ->
          e1 ^ "[" ^ compile_detail e2 ^ "-1]"
  | Cast.Matrix(s, v, nr, nc, t) ->
(*           "matrix c++"
 *)          let helper e = compile_detail e in
          let holder = s ^ "Holder" in
          let count = s ^ "count" in
          let nr_str = compile_detail nr in
          let nc_str = compile_detail nc in 
          let matrix = s in 
          let ty = Cast.string_of_ctype t in
          ty ^ " " ^ holder ^ "[] = {" ^ 
          (String.concat ", " (List.map helper v)) ^ "};\n" ^
          "vector<" ^ ty ^ "> " ^ matrix ^ "Vector" ^
          " (" ^ holder ^ ", " ^ holder ^ " + " ^
          nc_str ^ " / sizeof(" ^ ty ^ "));\n" ^
          "vector<vector<" ^ ty ^ "> > " ^ matrix ^ " (" ^ nr_str ^ ");\n" ^
          "int " ^ count ^ "=0;\n" ^
          "for(int i=0; i<" ^ nr_str ^ "; i++) { \n" ^
          matrix ^ "[i].resize(" ^ nc_str ^ ");\n" ^
          "for(int j=0; j<" ^ nc_str ^ "; j++) { \n" ^
          matrix ^ "[i][j] = " ^ holder ^ "[" ^ count ^ "++];\n}\n}"
  | Cast.MatrixAcc(m, r, c, t) ->
          m ^ "[" ^ compile_detail r ^ "][" ^ compile_detail c ^ "]"
  | Cast.FuncCall(id, el, t) -> let helper e = compile_detail e in
        id ^ "(" ^ (String.concat ", " (List.map helper el)) ^ ")"
  | Cast.PrintCall(e, t) -> "cout << " ^ (compile_detail e) ^ ";\n" ^ 
                            "cout << endl"
  | Cast.And(b1, b2, t) -> (compile_detail b1) ^ " && " ^ 
                           (compile_detail b2)
  | Cast.Or(b1, b2, t) -> (compile_detail b1) ^ " || " ^ 
                          (compile_detail b2)
  | Cast.Not(b1, t) -> "! " ^ (compile_detail b1)
  | Cast.Assign(id, e, t) -> 
          let ty = (string_of_ctype t) in
          ty ^ " " ^ id ^ " = " ^ (compile_detail e)
  | Cast.MatrixAssign(id, e, t) ->
(*             "matrix assign c++"
 *)           let ty = (Cast.string_of_ctype t) in
          "\n\n\n\n MATRIX ASSIGN \n\n\n " ^
          "int rows = temp.size();\n" ^
          "vector<vector<" ^ty^ "> " ^ id ^ ";\n" ^
          "for(int i = 0; i < rows; i++){\n" ^
          "\t" ^ "vector<" ^ty^ "> row (temp[i]);\n" ^
          "\t" ^ id ^ ".push_back(row);\n" ^
          "};"  
              
  | Cast.Mod(e1, e2, t) -> (compile_detail e1) ^ " % " ^ 
                           (compile_detail e2) 
  | Cast.Expo(e1, e2, t) -> "pow(" ^ (compile_detail e1) ^ ",  " ^ 
                            (compile_detail e2) ^ ")"
  | Cast.Eq(e1, e2, t) -> "(" ^ (compile_detail e1) ^ " == " ^
                           (compile_detail e2) ^ ")"
  | Cast.Neq(e1, e2, t) -> "(" ^ (compile_detail e1) ^ " != " ^
                           (compile_detail e2) ^ ")"
  | Cast.StrEq(e1, e2, t) -> "(strcmp(" ^ 
                           (compile_detail e1) ^ "," ^
                           (compile_detail e2) ^ ") == 0)"
  | Cast.StrNeq(e1, e2, t) -> "(strcmp(" ^ 
                           (compile_detail e1) ^ "," ^
                           (compile_detail e2) ^ ") != 0)"
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
                            (compile_detail e2)
  | Cast.MatrixAdd(e1, e2, t) ->
          let mid1 = compile_detail e1 in
          let mid2 = compile_detail e2 in
          let ty = (Cast.string_of_ctype t) in
          let row1 = mid1 ^ "row" in
          let row2 = mid2 ^ "row" in
          let col1 = mid1 ^ "col" in
          let col2 = mid2 ^ "col" in

          mid1

         (* e1 and e2 should be strings representing our matrix ids *)
          
         (*  "\n\n\n\n MATRIX ADD \n\n\n" ^
          "int " ^ row1 ^ " = " ^ mid1 ^ ".size();\n" ^
          "int " ^ row2 ^ " = " ^ mid2 ^ ".size();\n" ^
          "if (" ^ row1 ^ " != " ^ row2 ^ "){\n" ^
          "\t cout << \"matrix size mismatch\";\n\t count << endl;\n" ^
          "int " ^ col1 ^ " = " ^ mid1 ^ "[0].size();\n" ^
          "int " ^ col2 ^ " = " ^ mid1 ^ "[0].size();\n" ^
          "if (" ^ col1 ^ " != " ^ col2 ^ "){\n" ^
          "\t cout << \"matrix size mismatch\";\n\t count << endl;\n" ^
          (* checked num rows and num cols, now create a result matrix *)
          "vector<vector<" ^ ty ^ "> > temp;\n" ^
          "for(int i = 0; i < " ^ row1 ^ "; i++){\n" ^
          "\t"^ "vector<int> row;\n" ^
          "\t"^ "for(int j = 0; j < " ^ col1 ^ "; i++){\n" ^
          "\t\t" ^ "row.push_back("^mid1^"[i][j] + "^mid2^"[i][j];\n" ^
          "\t}\n" ^
          "\t" ^ "temp.push_back(row);\n" ^
          "}" *)

  | Cast.FormalDef(id, e, t) -> Cast.string_of_ctype t ^ " " ^ id ^ "=" ^ (compile_detail e)

  in

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
  | CMatrixAdd(e1, e2, t) -> compile_expr e1 ^ compile_expr e2 ^ "cmatrixadd"
  | CMatrixAcc(e1, e2, t) -> compile_expr e1 ^ compile_expr e2
  | Ceq(e1, e2, t) -> compile_expr e1 ^ compile_expr e2 
  | Cneq(e1, e2, t) -> compile_expr e1 ^ compile_expr e2 
  | CFDiv(e1, e2, t) -> compile_expr e1 ^ compile_expr e2 
  | Cand(b1, b2, t) -> compile_expr b1 ^ compile_expr b2 
  | Cor(b1, b2, t) -> compile_expr b1 ^ compile_expr b2
  | Cnot(b1, t) -> (compile_expr b1) in

let rec compile_stmt = function
  | Cstmt(e, t) -> compile_expr e ^ ";\n"
  | Cblock(sl, t) -> let string_list l = List.map compile_stmt l in
                  "{\n" ^ String.concat "" (string_list sl) ^ "}\n"
(*   | CReturnBlock(sl, s, t) -> let string_list l = List.map compile_stmt l in
              "{\n" ^ String.concat "" (string_list sl) ^
              compile_stmt s ^ "}\n" *)
  | Cif(e, s1, s2, t) ->   "if(" ^ compile_expr e ^ ")" 
                           ^ (compile_stmt s1)  ^ 
                           "else" 
                           ^  (compile_stmt s2)
  | Cfor(id, e1, e2, s, t) -> "for( int " ^ id ^ "="  ^ compile_expr e1 ^ "; " ^ 
                          id ^" <= " ^ compile_expr e2 ^ "; " ^ id ^
                          "++)\n" ^ compile_stmt s 
  | Creturn(e, t) -> "return (" ^ (compile_expr e) ^ ");\n"
 
      
  in

let compile_func_detail f = 
  let helper e = compile_detail e in
  let string_formals = List.map helper f.formals in
    f.fname
    ^ 
    "(" ^ String.concat "," string_formals ^ ")"
    ^ 
    compile_stmt f.body
    in

let compile_func = function 
    | CFunctionDef(f, t) -> 
        string_of_ctype t ^ " " ^
        compile_func_detail f in


(*         let string_list_stmt l = List.map compile_stmt l in
        let string_list_det l = List.map compile_detail l in
        Cast.string_of_ctype t ^ " " ^ s ^ "(" ^ String.concat ", " (string_list_det frmls) ^ ")"
        ^ compile_stmt block in *)


let compile cast = 
  let string_list = List.map compile_func cast in

  String.concat ""  string_list in

  let c_begin = 
  "#include<iostream>\n"^
  "#include<stdio.h>\n" ^
  "#include<math.h>\n" ^
  "#include<vector>\n" ^
  "#include<string>\n" ^
  "#include<string.h>\n" ^
  "using namespace std;\n" in

print_endline ( c_begin ^ (compile cast))
