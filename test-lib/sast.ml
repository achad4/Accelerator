
type op = Add

type t = 
	Int

type op_detail =
	IntBinop of op

type operator =
	Operator of op_detail * t

type expr_detail = 
	 IntLit of int

type detail = 
	ExprDet of expr_detail
	| OpDet of op_detail

type expression = 
	| Sexpr of detail * t
	| Sbinop of expression * operator * expression
	 
type stmt_detail = 
	Expr of expression

type statement = 
	Sstmt of stmt_detail * t


let string_of_type = function
	Int -> " int"

let string_of_op = function
	Add -> " +"

let rec expr = function
	Ast.IntLit( c ) -> (* print_int c; *) (* print_endline (string_of_type Int); *) ExprDet( IntLit(c) ), Int
	| Ast.Binop( e1, op, e2) ->
		let e1 = expr e1
		and e2 = expr e2 in

		let _, t1 = e1
		and _, t2 = e2 in

(* 		require_integer e1 "Left operand must be an integer";
		require_integer e2 "Left operand must be an integer" *)
		if t1 = t2 then
			( (*print_endline (string_of_op Add); *)
			OpDet( IntBinop(Add) ), Int )
		else
			failwith "Type incompatibility";;

let stmt = function
	Ast.Expr( e ) ->
	 (* print_endline (Ast.string_of_expression e); *)
	 let r = expr e in
	 Sexpr( (fst r), (snd r) )

let program program = 
	List.map stmt program
