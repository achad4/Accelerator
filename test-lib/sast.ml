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

type t = 
  | String
  | Int
  | Float
  | Bool
  | Na

type id = 
	| Id of string

type expr_detail = 
  | Na of t
  | IdLit of string
  | IntLit of int
  | IntExpr of expr_detail * t
  | BoolLit of bool
  | FloatLit of float
  | Vector of expr_detail * expr_detail list * t
  | VectIdAcc of expr_detail * expr_detail * t
  | VectIntAcc of expr_detail * expr_detail * t
  | Matrix of expr_detail * expr_detail list * expr_detail * expr_detail * t
  | MatrixIntAcc of expr_detail * expr_detail * expr_detail * t
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
  | FuncCall of id * expr_detail list * t
  | Assign of id * expr_detail * t
  | And of expr_detail * expr_detail * t
  | Or of expr_detail * expr_detail * t
  | Not of expr_detail * t

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
  | Sif of expression * statement * statement * t
  | Sfor of expression * expression * expression * statement * t

let string_of_type = function
  | String -> "string"
  | Int -> "int"
  | Bool -> "bool"
  | Float -> "float"
  | Na -> "Na"

let string_of_id = function
	Id(s) -> s 

let rec expr = function
  | Ast.Id ( s ) -> IdLit(s), String
  | Ast.IntLit( c ) -> IntLit(c), Int
  | Ast.FloatLit( f ) -> FloatLit(f), Float
  | Ast.BoolLit(b) -> BoolLit(b), Bool
  | Ast.Vector(s, vl) ->
        let head = List.hd vl in
        let _, vtype = expr head in
        let helper e = fst (expr e) in
        Vector(IdLit(s), (List.map helper vl), vtype), vtype
  | Ast.VectIdAcc(v, ind) ->
        let ve = expr (Ast.Id(v))
        and inde = expr (Ast.Id(ind)) in
        let _, idt = ve
        and _, indt = inde in
        if (idt == String && indt == String) then
            (
              VectIdAcc(IdLit(v),
                        IdLit(ind),
                        Na), Na
            )
        else
            failwith "Type incompatibility"
  | Ast.VectIntAcc(vid, indInt) ->
        let vide = expr (Ast.Id(vid))
        and inde = expr indInt in
        let _, idT = vide
        and _, indT = inde in
        if (idT == String && indT == Int) then
            (
                VectIntAcc(IdLit(vid),
                           IntExpr(fst inde, snd inde),
                           Na), Na
            )
        else
            failwith "Type incompatibility"
  | Ast.Matrix(s, v, nr, nc) ->
        let head = List.hd v in
        let _, vtype = expr head in
        let nrv, nrt = expr nr in
        let ncv, nct = expr nc in
        let helper e = fst (expr e) in
        if (nrt != Int || nct != Int) then
          failwith "nrow and ncol must be integers"
        else
          (
            Matrix(IdLit(s), (List.map helper v), helper nr , helper nc, vtype), vtype
          )       
  | MatrixIntAcc(vid, indInt1, indInt2) ->
        let vide = expr (Ast.Id(vid))
        and inde1 = expr indInt1
        and inde2 = expr indInt2 in
        let _, idT = vide
        and _, indT1 = inde1
        and _, indT2 = inde2
        in
        if (idT == String && indT1 == Int && indT2 == Int) then
            (
                MatrixIntAcc(IdLit(vid),
                           IntExpr(fst inde1, snd inde2),
                           IntExpr(fst inde2, snd inde2),
                           Na), Na
            )
        else
            failwith "Type incompatibility"

  | Ast.Na -> Na(Na), Na
  | Ast.None -> Na(Na), Na
  | Ast.Assign(id, e) -> 
    		let e1 = expr e in
    		Assign(Id(id), fst e1, snd e1), snd e1
	| Ast.FuncCall(id, el) -> 		
    		(*iterate over list of expressions and pull out the expression_detail from each one*)
    		let helper e = fst (expr e) in
    		FuncCall(Id(id), (List.map helper el), Na), Na
	| Ast.Add( e1, e2) ->
		let e1 = expr e1
		and e2 = expr e2 in

		let _, t1 = e1
		and _, t2 = e2 in

		if ((t1 = t2) && (t1 = Int) && (t2 = Int)) then
			(
				Add((fst e1), (fst e2), Int), Int
			)
		else
			failwith "Type incompatibility"
  | Ast.Sub( e1, e2 ) ->
          let e1 = expr e1
          and e2 = expr e2 in

          let _, t1 = e1
          and _, t2 = e2 in

 		if ((t1 = t2) && (t1 = Int) && (t2 = Int)) then
              (
              Sub((fst e1),(fst e2), Int), Int
              )
          else
              failwith "Type incompatability"
  | Ast.Mult( e1, e2 ) ->
          let e1 = expr e1
          and e2 = expr e2 in

          let _, t1 = e1
          and _, t2 = e2 in
          
	if ((t1 = t2) && (t1 = Int) && (t2 = Int)) then
              (
              Mult((fst e1),(fst e2), Int), Int
              )
          else
              failwith "Type incompatability"

  | Ast.Div( e1, e2 ) ->
          let e1 = expr e1
          and e2 = expr e2 in

          let _, t1 = e1
          and _, t2 = e2 in

      if ((t1 = t2) && (t1 = Int) && (t2 = Int)) then
              (
              Div((fst e1),(fst e2), Int), Int
              )
          else
              failwith "Type incompatability"

  | Ast.Expo( e1, e2 ) ->
          let e1 = expr e1
          and e2 = expr e2 in

          let _, t1 = e1
          and _, t2 = e2 in

	if ((t1 = t2) && (t1 = Int) && (t2 = Int)) then
              (
              Expo((fst e1),(fst e2), Int), Int
              )
          else
              failwith "Type incompatability"

  | Ast.Mod( e1, e2 ) ->
          let e1 = expr e1
          and e2 = expr e2 in

          let _, t1 = e1
          and _, t2 = e2 in

	if ((t1 = t2) && (t1 = Int) && (t2 = Int)) then
              (
              Mod((fst e1),(fst e2), Int), Int
              )
          else
              failwith "Type incompatability"
  | Ast.And( b1, b2) ->
          let b1 = expr b1
          and b2 = expr b2 in

          let _, t1 = b1
          and _, t2 = b2 in

	if ((t1 = t2) && (t1 = Bool) && (t2 = Bool)) then
              (
                 And((fst b1),(fst b2), Bool), Bool
              )
          else
              failwith "Type incompatibility"
  | Ast.Or( b1, b2) ->
          let b1 = expr b1
          and b2 = expr b2 in

          let _, t1 = b1
          and _, t2 = b2 in

	if ((t1 = t2) && (t1 = Bool) && (t2 = Bool)) then
              (
                 Or((fst b1),(fst b2), Bool), Bool
              )
          else
              failwith "Type incompatibility"
  | Ast.Not( b1 ) ->
          let b1 = expr b1 in
          let _, t1 = b1 in
          if t1 = Bool then
              (
                  Not((fst b1), Bool), Bool
              )
          else
              failwith "Type incompatibility"
	| Ast.FAdd( e1, e2) ->
		let e1 = expr e1
		and e2 = expr e2 in

		let _, t1 = e1
		and _, t2 = e2 in

		if ((t1 = t2) && (t1 = Float) && (t2 = Float)) then
			(
				FAdd((fst e1), (fst e2), Float), Float
			)
		else
			failwith "Type incompatibility"
	| Ast.FSub( e1, e2) ->
		let e1 = expr e1
		and e2 = expr e2 in

		let _, t1 = e1
		and _, t2 = e2 in

		if ((t1 = t2) && (t1 = Float) && (t2 = Float)) then
			(
				FSub((fst e1), (fst e2), Float), Float
			)
		else
			failwith "Type incompatibility"
	| Ast.FMult( e1, e2) ->
		let e1 = expr e1
		and e2 = expr e2 in

		let _, t1 = e1
		and _, t2 = e2 in

		if ((t1 = t2) && (t1 = Float) && (t2 = Float)) then
			(
				FMult((fst e1), (fst e2), Float), Float
			)
		else
			failwith "Type incompatibility"
	| Ast.FDiv( e1, e2) ->
		let e1 = expr e1
		and e2 = expr e2 in

		let _, t1 = e1
		and _, t2 = e2 in

		if ((t1 = t2) && (t1 = Float) && (t2 = Float)) then
			(
				FDiv((fst e1), (fst e2), Float), Float
			)
		else
			failwith "Type incompatibility"

let rec stmt = function
	| Ast.Expr( e ) -> 
        let r = expr e in
	      Sstmt(Sexpr( (fst r), (snd r) ), (snd r))
  | Ast.Block( sl ) -> 
          let l = List.map stmt sl in
          Sblock(l, Na)
  | Ast.If(e, s1, s2) -> 
          let r = expr e in
          Sif(Sexpr( (fst r), (snd r) ), 
              stmt s1, stmt s2, Na)
  | Ast.For(e, ie1, ie2, sl) ->
          let re = expr e
          and rie1 = expr ie1
          and rie2 = expr ie2 in
          Sfor(Sexpr(fst(re),snd(re)), 
               Sexpr(fst(rie1),snd(rie1)), 
               Sexpr(fst(rie2),snd(rie2)), 
               stmt sl,
               Na)

let program program = 
	List.map stmt program
