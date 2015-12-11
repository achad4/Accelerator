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
     | IdLit of string
	 | IntLit of int
     | BoolLit of bool
     | FloatLit of float
     | Vector of string * expr_detail list * t
     | Na of t
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
        Vector(s, (List.map helper vl), vtype), vtype
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
	| Ast.Expr( e ) -> let r = expr e in
	                 Sstmt(Sexpr( (fst r), (snd r) ), (snd r))
    | Ast.Block( sl ) -> let l = List.map stmt sl in
                         Sblock(l, Na)
    | Ast.If(e, s1, s2) -> let r = expr e in
<<<<<<< HEAD
                           Sif(Sexpr( (fst r), (snd r) ), stmt s1, stmt s2, Na)
    | Ast.For(id, e1, e2, s) ->  let r1 = expr e1 in
                             let r2 = expr e2 in
                            let r3 = expr id in
                            Sfor(Sexpr( (fst r3), (snd r3) ), Sexpr( (fst r1), (snd r1) ), Sexpr( (fst r2), (snd r2) ), stmt s, Na)
=======
                           Sif(Sexpr( (fst r), (snd r) ), 
                           stmt s1, stmt s2, Na)
>>>>>>> 156577cdd1d2791c26bb11d31dedc1e06261b420

let program program = 
	List.map stmt program
