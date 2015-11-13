

type expr_detail = 
	 IntLit of int

type t = 
	Int

type expression = 
	expr_detail * t

type stmt_detail = 
	Expr of expression

type statement = 
	stmt_detail * t


let string_of_type = function
	Int -> " Int"

let rec expr = function
	Ast.IntLit( c ) -> print_int c; print_endline (string_of_type Int); IntLit( c ), Int

let rec stmt = function
	Ast.Expr( e ) -> Expr( expr e )

let program program = 
	List.map stmt program






