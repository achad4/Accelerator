
type op = Add

type t = 
	Int

type expr_detail = 
	 IntLit of int
	 | IntBinop of op

type expression = 
	| Sexpr of expr_detail * t
	| Sbinop of expression * op * expression
	 
type stmt_detail = 
	Expr of expression

type statement = 
	Sstmt of stmt_detail * t


let string_of_type = function
	Int -> " Int"

let string_of_op = function
	Add -> " Add"

let rec expr = function
	Ast.IntLit( c ) -> print_int c; print_endline (string_of_type Int); IntLit( c ), Int
	| Ast.Binop( e1, op, e2) ->
		let e1 = expr e1
		and e2 = expr e2 in

		let _, t1 = e1
		and _, t2 = e2 in

(* 		require_integer e1 "Left operand must be an integer";
		require_integer e2 "Left operand must be an integer" *)
		if t1 = t2 then
			( print_endline (string_of_op Add);
			IntBinop(Add), Int)
		else
			failwith "Type incompatibility";;

let stmt = function
	Ast.Expr( e ) ->
	 print_endline (Ast.string_of_expression e);
	 let r = expr e in
	 Sexpr( (fst r), (snd r) )

let program program = 
	List.map stmt program
