type op = 
	| Add
    | Sub
    | Mult
    | Div
    | Expo
    | Mod
    | Assign
    | And
    | Or
    | Not

type t = 
    | String
	| Int
    | Bool
	| Na

type id = 
 	| Id of string

type expr_detail = 
     | IdLit of string
	 | IntLit of int
     | BoolLit of bool
	 | Add of expr_detail * expr_detail * t
     | Sub of expr_detail * expr_detail  * t
     | Mult of expr_detail * expr_detail * t
     | Div of expr_detail * expr_detail * t
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
    | Sdiv of expression * expression * t
    | Sexpo of expression * expression * t
    | Smod of expression * expression * t
	| SfuncCall of expression list * t
	| Sassign of expression * t
    | Sand of expression * expression * t
    | Sor of expression * expression * t
    | Snot of expression * t

type statement = 
	Sstmt of expression * t

let string_of_type = function
    | String -> "string"
	| Int -> "int"
    | Bool -> "bool"
    | Na -> "Na"

let string_of_id = function
	Id(s) -> s 

let rec expr = function
    | Ast.Id ( s ) -> IdLit(s), String
	| Ast.IntLit( c ) -> IntLit(c), Int
    | Ast.BoolLit(b) -> BoolLit(b), Bool
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

		if t1 = t2 then
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

            if t1 = t2 then
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

            if t1 = t2 then
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

            if t1 = t2 then
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

            if t1 = t2 then
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

            if t1 = t2 then
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

            if t1 = t2 then
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

            if t1 = t2 then
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

let stmt = function
	Ast.Expr( e ) ->
	 (* print_endline (Ast.string_of_expression e); *)
	 let r = expr e in
	 Sexpr( (fst r), (snd r) ), (snd r)

let program program = 
	List.map stmt program
