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
  | Cast.VectAcc(e1, e2, t) ->
          e1 ^ "[" ^ compile_detail e2 ^ "-1]"
  | Cast.Matrix(s, v, nr, nc, t) ->
        let helper e = compile_detail e in
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
  | Cast.PrintMatrixCall(e, t) -> "print_matrix("^ (compile_detail e) ^")"
 
  | Cast.And(b1, b2, t) -> (compile_detail b1) ^ " && " ^ 
                           (compile_detail b2)
  | Cast.Or(b1, b2, t) -> (compile_detail b1) ^ " || " ^ 
                          (compile_detail b2)
  | Cast.Not(b1, t) -> "! " ^ (compile_detail b1)
  | Cast.Update(id, e, t) -> id ^ " = " ^ (compile_detail e)
  | Cast.Assign(id, e, t) -> 
          if(t = Matrix) then (
              "vector<vector<int> > " ^id^ " = " ^ (compile_detail e)
          ) else(
            let ty = (string_of_ctype t) in
            ty ^ " " ^ id ^ " = " ^ (compile_detail e)
          )
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
  | Cast.Mult(e1, e2, t) -> if(t = Matrix) then (
                               let mid1 = string_of_matrix_assign e1 in
                              let mid2 = string_of_matrix_assign e2 in
                              "matrix_mult("^mid1^", "^ mid2 ^ ")"
                            )else(
                                (compile_detail e1) ^ " + " ^
                                (compile_detail e2)
                              )
  | Cast.Sub(e1, e2, t) -> (compile_detail e1) ^ " - " ^ 
                           (compile_detail e2) 
  | Cast.Add(e1, e2, t) -> if(t = Matrix) then (
                               let mid1 = string_of_matrix_assign e1 in
                              let mid2 = string_of_matrix_assign e2 in
                              "matrix_add("^mid1^", "^ mid2 ^ ")"
                            )else(
                                (compile_detail e1) ^ " + " ^
                                (compile_detail e2)
                              )
  | Cast.FormalDef(id, e, t) -> Cast.string_of_ctype t ^ " " ^ id ^ "=" ^ (compile_detail e)

  in

let rec compile_expr = function
	| Cexpr(e, t) -> compile_detail e 
	| CfuncCall(el, t) -> String.concat "" (List.map compile_expr el)
  | Cmod(e1, e2, t) -> compile_expr e1 ^ compile_expr e2 
  | Cexpo(e1, e2, t) -> compile_expr e1 ^ compile_expr e2
  | Cdiv(e1, e2, t) -> compile_expr e1 ^ compile_expr e2 
  | Cmult(e1, e2, t) -> compile_expr e1 ^ compile_expr e2
  | Csub(e1, e2, t) -> compile_expr e1 ^ compile_expr e2 
  | Cadd(e1, e2, t) -> compile_expr e1 ^ compile_expr e2 
  | CFAdd(e1, e2, t) -> compile_expr e1 ^ compile_expr e2  
  | CFSub(e1, e2, t) -> compile_expr e1 ^ compile_expr e2 
  | CFMult(e1, e2, t) -> compile_expr e1 ^ compile_expr e2 
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
  | Cif(e, s1, s2, t) ->   "if(" ^ compile_expr e ^ ")" 
                           ^ (compile_stmt s1)  ^ 
                           "else" 
                           ^  (compile_stmt s2)
  | Cfor(id, e1, e2, s, t) -> "for( int " ^ id ^ "="  ^ compile_expr e1 ^ "; " ^ 
                          id ^" <= " ^ compile_expr e2 ^ "; " ^ id ^
                          "++)\n" ^ compile_stmt s 
  | Creturn(e, t) -> "return (" ^ (compile_expr e) ^ ");\n"
  | Cwhile(e, s, t) -> "while " ^ (compile_detail e) ^ compile_stmt s 
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
        if(t = Matrix) then (
            string_of_ctype t ^ " " ^
            compile_func_detail f
        )else(
            string_of_ctype t ^ " " ^
            compile_func_detail f
        ) in
        
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
   "using namespace std;\n" ^
   "
   vector<vector<int> > matrix_add(vector<vector<int> > a, vector<vector<int> > b){
       vector<vector<int> > c;
       for(int i = 0; i < a.size(); i++){
           vector<int> row;
            for(int j = 0; j < a[0].size(); j++){
                row.push_back(a[i][j] + b[i][j]);
            }
           c.push_back(row);
       }
       return c;
   }

   vector<vector<int> > matrix_mult(vector<vector<int> > a, vector<vector<int> > b){
   vector<vector<int> > c;
   int i,j,k;
   #pragma omp parallel shared(a,b,c) private(i,j,k)
   {
   #pragma omp for  schedule(static)
      for (i=0; i<a.size(); i=i+1){
                vector<int> row;
                for (j=0; j<b[0].size(); j=j+1){
                    row.push_back(0);
                    for (k=0; k<b.size(); k=k+1){
                        row[j] += ((a[i][k])*(b[k][j]));
                    }
                }
                c.push_back(row);
            }
        }
        return c;
    }
   void print_matrix(vector<vector<int> > a){
       for(int i = 0; i<a.size(); i++){
           cout<<endl;
           for(int j = 0; j<a[0].size(); j++){
               cout << a[i][j] << \" \";
           }
       }
   }\n" in

  print_endline ( c_begin ^ (compile cast))
