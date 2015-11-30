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
	 | Add of expr_detail * expr_detail
     | Sub of expr_detail * expr_detail
     | Mult of expr_detail * expr_detail
     | Div of expr_detail * expr_detail
     | Expo of expr_detail * expr_detail
     | Mod of expr_detail * expr_detail
     | FAdd of expr_detail * expr_detail
     | FSub of expr_detail * expr_detail
     | FMult of expr_detail * expr_detail
     | FDiv of expr_detail * expr_detail
	 | FuncCall of id * expr_detail list
	 | Assign of id * expr_detail
     | And of expr_detail * expr_detail
     | Or of expr_detail * expr_detail
     | Not of expr_detail

type detail = 
	| ExprDet of expr_detail

type expression = 
	| Sexpr of expr_detail * t
	| Sadd of expression * expression
    | Ssub of expression * expression
    | Smult of expression * expression
    | Sdiv of expression * expression
    | Sexpo of expression * expression
    | Smod of expression * expression
    | SFAdd of expression * expression
    | SFSub of expression * expression
    | SFMult of expression * expression
    | SFDiv of expression * expression
	| SfuncCall of expression list * t
	| Sassign of expression * t
    | Sand of expression * expression
    | Sor of expression * expression
    | Snot of expression

type stmt_detail = 
	Expr of expression

type statement = 
	Sstmt of stmt_detail * t

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
	| Ast.Assign(id, e) -> 
		let e1 = expr e in
		Assign(Id(id), fst e1), snd e1
	| Ast.FuncCall(id, el) -> 		
		(*iterate over list of expressions and pull out the expression_detail from each one*)
		let helper e = fst (expr e) in
		FuncCall(Id(id), (List.map helper el)), Na
	| Ast.Add( e1, e2) ->
		let e1 = expr e1
		and e2 = expr e2 in

		let _, t1 = e1
		and _, t2 = e2 in

		if t1 = t2 then
			(
				Add((fst e1), (fst e2)), Int
			)
		else
			failwith "Type incompatibility"
    | Ast.Sub( e1, e2 ) ->
            let e1 = expr e1
            and e2 = expr e2 in

            let _, t1 = e1
            and _, t2 = e2 in

            if t1 = t2 then
                (
                Sub((fst e1),(fst e2)), Int
                )
            else
                failwith "Type incompatability"
    | Ast.Mult( e1, e2 ) ->
            let e1 = expr e1
            and e2 = expr e2 in

            let _, t1 = e1
            and _, t2 = e2 in

            if t1 = t2 then
                (
                Mult((fst e1),(fst e2)), Int
                )
            else
                failwith "Type incompatability"

    | Ast.Div( e1, e2 ) ->
            let e1 = expr e1
            and e2 = expr e2 in

            let _, t1 = e1
            and _, t2 = e2 in

            if t1 = t2 then
                (
                Div((fst e1),(fst e2)), Int
                )
            else
                failwith "Type incompatability"

    | Ast.Expo( e1, e2 ) ->
            let e1 = expr e1
            and e2 = expr e2 in

            let _, t1 = e1
            and _, t2 = e2 in

            if t1 = t2 then
                (
                Expo((fst e1),(fst e2)), Int
                )
            else
                failwith "Type incompatability"

    | Ast.Mod( e1, e2 ) ->
            let e1 = expr e1
            and e2 = expr e2 in

            let _, t1 = e1
            and _, t2 = e2 in

            if t1 = t2 then
                (
                Mod((fst e1),(fst e2)), Int
                )
            else
                failwith "Type incompatability"
    | Ast.And( b1, b2) ->
            let b1 = expr b1
            and b2 = expr b2 in

            let _, t1 = b1
            and _, t2 = b2 in

            if t1 = t2 then
                (
                   And((fst b1),(fst b2)), Bool
                )
            else
                failwith "Type incompatibility"
    | Ast.Or( b1, b2) ->
            let b1 = expr b1
            and b2 = expr b2 in

            let _, t1 = b1
            and _, t2 = b2 in

            if t1 = t2 then
                (
                   Or((fst b1),(fst b2)), Bool
                )
            else
                failwith "Type incompatibility"
    | Ast.Not( b1 ) ->
            let b1 = expr b1 in
            let _, t1 = b1 in
            if t1 = Bool then
                (
                    Not(fst b1), Bool
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
				FAdd((fst e1), (fst e2)), Float
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
				FSub((fst e1), (fst e2)), Float
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
				FMult((fst e1), (fst e2)), Float
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
				FDiv((fst e1), (fst e2)), Float
			)
		else
			failwith "Type incompatibility"

let stmt = function
	Ast.Expr( e ) ->
	 (* print_endline (Ast.string_of_expression e); *)
	 let r = expr e in
	 Sexpr( (fst r), (snd r) )

let program program = 
	List.map stmt program
