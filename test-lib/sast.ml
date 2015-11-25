type op = 
	Add
    | Sub
    | Mult
    | Div
    | Expo
    | Mod
    | Assign
    | And
    | Or

type t = 
	Int
    | Bool
	| Na


type id = 
 	Id of string

type expr_detail = 
	 IntLit of int
     | BoolLit of bool
	 | Add of expr_detail * expr_detail
     | Sub of expr_detail * expr_detail
     | Mult of expr_detail * expr_detail
     | Div of expr_detail * expr_detail
     | Expo of expr_detail * expr_detail
     | Mod of expr_detail * expr_detail
	 | FuncCall of id * expr_detail list
	 | Assign of id * expr_detail
     | And of expr_detail * expr_detail
     | Or of expr_detail * expr_detail

type detail = 
	ExprDet of expr_detail

type expression = 
	| Sexpr of expr_detail * t
	| Sadd of expression * expression
    | Ssub of expression * expression
    | Smult of expression * expression
    | Sdiv of expression * expression
    | Sexpo of expression * expression
    | Smod of expression * expression
	| SfuncCall of expression list * t
	| Sassign of expression * t
    | Sand of expression * expression
    | Sor of expression * expression

type stmt_detail = 
	Expr of expression

type statement = 
	Sstmt of stmt_detail * t

let string_of_type = function
	  Int -> "int"
    | Bool -> "bool"
    | Na -> "Na"

let string_of_id = function
	Id(s) -> s 

let rec expr = function
	Ast.IntLit( c ) -> (* print_int c; *) (* print_endline (string_of_type Int); *) IntLit(c), Int
    | Ast.BoolLit(b) -> BoolLit(b), Bool
	| Ast.Assign(id, e) -> 
		let e1 = expr e in
		Assign(Id(id), fst e1), snd e1
	| Ast.FuncCall(id, el) -> 		
		(*iterate over list of expressions and pull out the expression_detail from each one*)
		let helper e = 
			fst (expr e) in

		FuncCall(Id(id), (List.map helper el)), Na
	| Ast.Add( e1, e2) ->
		let e1 = expr e1
		and e2 = expr e2 in

		let _, t1 = e1
		and _, t2 = e2 in

(* 		require_integer e1 "Left operand must be an integer";
		require_integer e2 "Left operand must be an integer" *)
		if t1 = t2 then
			( (*print_endline (string_of_op Add); *)
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

let stmt = function
	Ast.Expr( e ) ->
	 (* print_endline (Ast.string_of_expression e); *)
	 let r = expr e in
	 Sexpr( (fst r), (snd r) )

let program program = 
	List.map stmt program
