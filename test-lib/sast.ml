open Environment

exception CSVFormatFailure;;

type op = 
  | Add
  | Sub
  | Mult
  | Div
  | Expo
  | Mod
  | FAdd
  | FSub
  | FMult
  | FDiv
  | Assign
  | And
  | Or
  | Not

type expr_detail = 
  | Id of string
  | NaLit of t 
  | IntLit of int
  | IntExpr of expr_detail * t
  | BoolLit of bool
  | FloatLit of float
  | StringLit of string
  | Vector of string * expr_detail list * t
  | VectAcc of string * expr_detail * t
  | Matrix of string * expr_detail list * expr_detail * expr_detail * t
  | MatrixAcc of string * expr_detail * expr_detail * t
  | Add of expr_detail * expr_detail * t
  | Sub of expr_detail * expr_detail  * t
  | Mult of expr_detail * expr_detail * t
  | Div of expr_detail * expr_detail * t
  | FAdd of expr_detail * expr_detail * t
  | FSub of expr_detail * expr_detail * t
  | FMult of expr_detail * expr_detail * t
  | FDiv of expr_detail * expr_detail * t
  | Expo of expr_detail * expr_detail * t
  | Mod of expr_detail * expr_detail * t
  | FuncCall of string * expr_detail list * t
  | Assign of string * expr_detail * t
  | And of expr_detail * expr_detail * t
  | Or of expr_detail * expr_detail * t
  | Not of expr_detail * t
  | FormalDef of string * expr_detail * t * environment

type detail = 
	| ExprDet of expr_detail

type expression = 
  | Sexpr of expr_detail * t
  | Sadd of expression * expression * t
  | Ssub of expression * expression * t
  | Smult of expression * expression * t
  | SFAdd of expression * expression * t
  | SFSub of expression * expression * t
  | SFMult of expression * expression * t
  | SFDiv of expression * expression * t
  | Sdiv of expression * expression * t
  | Sexpo of expression * expression * t
  | Smod of expression * expression * t
  | SfuncCall of expression list * t
  | Sassign of expression * t
  | Sand of expression * expression * t
  | Sor of expression * expression * t
  | Snot of expression * t

type statement = 
  | Sstmt of expression * t
  | Sblock of statement list * t
(*   | SReturnBlock of statement list * statement * t *)
  | Sif of expression * statement * statement * t
  | Scsv of string * string * bool * int * int * t
  | Sfor of string * expression * expression * statement * t
  | Sreturn of expression * t

type function_definition =
  | FunctionDef of string * expr_detail list * statement * t

let csv_string_type input =
  if (Str.string_match (Str.regexp "['-']?['0'-'9']+") input 0) then Int
  else if Str.string_match 
          (Str.regexp "['-']?['0'-'9']+['.']['0'-'9']+") 
           input 0 then Float 
  else if Str.string_match
          (Str.regexp "['\"'][.]*['\"']") 
          input 0 then String
  else if Str.string_match 
          (Str.regexp "true") 
          input 0 then Bool
  else if Str.string_match 
          (Str.regexp "false") 
          input 0 then Bool
  else if Str.string_match 
          (Str.regexp "Na") 
          input 0 then Na
  else raise IncorrectTypeException

let rec type_of_stmt = function
  | Sstmt(e,t) -> t
  | Sblock(sl, t) -> let last_stmt = List.nth sl (List.length sl - 1) in
                     type_of_stmt last_stmt
  | Sif(e,s1,s2,t) -> t
  | Sfor(s1,e1,e2,s2,t) -> t
  | Sreturn(e, t) -> t
  | Scsv(id, fl, opt, row, col, t) -> t 


let rec expr = function
  | Environment.Id (s), env -> Id(s), Environment.find_type s env
  | Environment.Assign(id, e), env -> 
        let e1 = expr (e, env) in
        Assign(id, fst e1, snd e1), snd e1
  | Environment.IntLit(c), env -> IntLit(c), Int
  | Environment.FloatLit(f), env -> FloatLit(f), Float
  | Environment.BoolLit(b), env -> BoolLit(b), Bool
  | Environment.StringLit(s), env -> StringLit(s), String
  | Environment.Vector(s, vl), env ->let head = List.hd vl in
                        let _, vtype = expr (head, env) in
                        let helper e = fst (expr (e, env)) in
                        Vector(s, (List.map helper vl), vtype), Vector
  | Environment.VectAcc(s, e), env ->

        let e1 = expr (e, env) in
        let t = snd e1 in

        if (t = Int) then
            (
              let v_type = Environment.find_type s env in 
              VectAcc(s,
                        fst e1,
                        v_type), v_type
            )
        else
            failwith "Type incompatibility"
  | Environment.Matrix(s, v, nr, nc), env ->
        let head = List.hd v in
        let _, vtype = expr (head, env) in
        let nrv, nrt = expr (nr, env) in
        let ncv, nct = expr (nc, env) in
        let helper e = fst (expr (e, env)) in
        if (nrt != Int || nct != Int) then
          failwith "nrow and ncol must be integers"
        else
          (
            Matrix(s, (List.map helper v), helper nr , helper nc, vtype), vtype
          )  
  | Environment.MatrixAcc(s, e1, e2), env ->
        let ed1 = expr (e1, env) in
        let ed2 = expr (e2, env) in
        let t1 = snd ed1 in
        let t2 = snd ed2 in
        if (t1 = Int && t2 = Int) then
        (
          let m_type = Environment.find_type s env in 
          MatrixAcc(s,
                          fst ed1,
                          fst ed2,
                          m_type), m_type
        )
        else
            failwith "Type incompatibility"
  | Environment.Na, env -> NaLit(Na), Na
	| Environment.FuncCall(id, el), env -> 		
    		(*iterate over list of expressions and pull out the expression_detail from each one*)
    		let helper e = fst (expr (e, env)) in
    		FuncCall(id, (List.map helper el), Na), Na
	| Environment.Add( e1, e2), env ->
		let e1 = expr (e1, env)
		and e2 = expr (e2, env) in

		let _, t1 = e1
		and _, t2 = e2 in

		if (t1 == t2 && (t1 == Int || t1 == Float)) then
			(
				Add((fst e1), (fst e2), t1), t1
			)
		else
			failwith "Type incompatibility"
  | Environment.Sub( e1, e2 ), env ->
          let e1 = expr (e1, env)
          and e2 = expr (e2, env) in

          let _, t1 = e1
          and _, t2 = e2 

        in

 		if (t1 == t2 && (t1 == Int || t1 == Float)) then
              (
              Sub((fst e1),(fst e2), t1), t1
              )
          else
              failwith "Type incompatability"
  | Environment.Mult( e1, e2 ), env ->
          let e1 = expr (e1, env)
          and e2 = expr (e2, env) in

          let _, t1 = e1
          and _, t2 = e2 in
          
	if (t1 == t2 && (t1 == Int || t1 == Float)) then
              (
              Mult((fst e1),(fst e2), t1), t1
              )
          else
              failwith "Type incompatability"

  | Environment.Div( e1, e2 ), env ->
          let e1 = expr (e1, env)
          and e2 = expr (e2, env) in

          let _, t1 = e1
          and _, t2 = e2 in

      if (t1 == t2 && (t1 == Int || t1 == Float)) then
              (
              Div((fst e1),(fst e2), t1), t1
              )
          else
              failwith "Type incompatability"

  | Environment.Expo( e1, e2 ), env ->
          let e1 = expr (e1, env)
          and e2 = expr (e2, env) in

          let _, t1 = e1
          and _, t2 = e2 in

	if ((t1 == Int || t1 == Float) && (t2 == Int)) then
              (
              Expo((fst e1),(fst e2), t1), t1
              )
          else
              failwith "Type incompatability"

  | Environment.Mod( e1, e2 ), env ->
          let e1 = expr (e1, env)
          and e2 = expr (e2, env) in

          let _, t1 = e1
          and _, t2 = e2 in

	if (t1 == Int || t1 == Float && t2 == Int) then
              (
              Mod((fst e1),(fst e2), t1), Int
              )
          else
              failwith "Type incompatability"
  | Environment.And( b1, b2), env ->
          let b1 = expr (b1, env)
          and b2 = expr (b2, env) in

          let _, t1 = b1
          and _, t2 = b2 in

	if (t1 == t2 && (t1 == Bool)) then
              (
                 And((fst b1),(fst b2), Bool), Bool
              )
          else
              failwith "Type incompatibility"
  | Environment.Or( b1, b2), env ->
          let b1 = expr (b1, env)
          and b2 = expr (b2, env) in

          let _, t1 = b1
          and _, t2 = b2 in

	if (t1 == t2 && (t1 == Bool)) then
              (
                 Or((fst b1),(fst b2), Bool), Bool
              )
          else
              failwith "Type incompatibility"
  | Environment.Not( b1 ), env ->
          let b1 = expr (b1, env) in
          let _, t1 = b1 in
          if ( t1 == Bool) then
              (
                  Not((fst b1), Bool), Bool
              )
          else
              failwith "Type incompatibility"
  | Environment.FormalDef(id,e), env -> 
          let e1 = expr (e, env) in
          let _, t1 = e1 in
          FormalDef(id, fst e1, t1, env), t1

let rec stmt = function
	| Environment.Expr( e ), env -> 
        let r = expr (e, env) in
	      Sstmt(Sexpr(fst r, snd r), (snd r))
  | Environment.Block( sl ), env -> 
          let helper s = stmt (s, env) in
          let l = List.map helper sl in
          let last_stmt = List.nth l (List.length l - 1) in
          Sblock(l, type_of_stmt last_stmt)
  | Environment.If(e, s1, s2), env -> 
          let r = expr (e, env) in

          Sif(Sexpr( (fst r), (snd r) ), 
              stmt (s1, env), stmt (s2, env), Na)
  | Environment.For(s, ie1, ie2, sl), env -> 
          let new_env = Environment.assign_current_scope s Int env
          and rie1 = expr (ie1, env)
          and rie2 = expr (ie2, env) in
          Sfor(s, 
               Sexpr(fst(rie1),snd(rie1)), 
               Sexpr(fst(rie2),snd(rie2)), 
               stmt (sl, new_env),
               Na)
  | Environment.Csv(id, fl, b, t), env -> 
          (* get the first line, type of 1st elem, number of elements *)
          (* iterate across all newline sep rows, checking length and
           * type *)
          let read_csv_channel chan =
              let buf = Buffer.create 4096 in
                let rec loop () =
                  let line = input_line chan in
                  Buffer.add_string buf line;
                  Buffer.add_char buf '\n';
                  loop () in
                try
                  loop ()
                with
                  End_of_file -> Buffer.contents buf in 
                  
          let read_csv_file filename =
              let channel = open_in filename in
              read_csv_channel channel in
            
          (* get csv as a string, using above file reading functions*)
          let csv_str = read_csv_file fl in
          let nl_split = Str.split (Str.regexp "\n") in
          let comma_split = Str.split (Str.regexp ",") in
          
          (* split csv string into a list, separated by newline *)
          let row_list = nl_split csv_str in
          (* get number of rows, for c++ matrix building *)
          let num_row = List.length row_list in
          let first_row = List.hd row_list in
          let first_row_list = comma_split first_row in
          (* get number of elements in first row *)
          let row_len = List.length first_row_list in
          (* get type of first element *)
          let first_elem = List.hd first_row_list in
          let first_elem_type = csv_string_type first_elem in
          if first_elem_type == Na then raise IncorrectTypeException
          else
              (* iterate across all rows *)
              let row_help row = 
                  let curr_row_list = comma_split row in
                  if List.length curr_row_list != row_len then
                      raise CSVFormatFailure
                  else
                      let elem_help elem =
                          let elem_type = csv_string_type elem in
                          if (elem_type != first_elem_type && elem_type != Na)
                          then
                              raise CSVFormatFailure
                          else
                              ()
                      in
                      List.iter elem_help curr_row_list in
              List.iter row_help row_list;
              Scsv(id, fl, b, num_row, row_len, first_elem_type)

  | Environment.Return(e), env -> 
          let e1 = expr (e, env) in
          Sreturn(Sexpr(fst e1, snd e1), snd e1)

let func_def = function
  | Environment.FunctionDef(s, ids, b), env ->
          let helper2 e = fst (expr (e, env)) in
          let forms = List.map helper2 ids in
          let block = stmt (b, env) in
          FunctionDef(s, forms, block, type_of_stmt block)

let program program = 
	List.map func_def (fst program), List.map stmt (snd program)
