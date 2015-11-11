open Ast

let rec eval = function
	| IntLit n -> n
    | DualOp (e1, Add, e2) -> (eval e1) + (eval e2)

