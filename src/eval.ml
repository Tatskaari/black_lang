module P = Parser

type var =
| Var of string

type value =
| VInt of int
| VBool of bool

module VarCmp = struct
  type t = var
  let compare v1 v2 =
  	match (v1, v2) with
  	| (Var (v1), Var (v2)) -> String.compare v1 v2  
end

module Varmap = Map.Make(VarCmp)

type env = value Varmap.t


let parse_file filename = 
	let file = Files.read_file filename in
	let tokens = Lex.lex file [] 1 in
	P.parse tokens 

let rec pow a b =
	match b with
	| 0 -> 1
	| 1 -> a
	| b -> a * pow a (b-1)

let rec eval_expr expr env =
	let apply_int_op lhs operator rhs =
		match operator with
		| Lex.Add -> VInt(lhs + rhs)
		| Lex.Sub -> VInt(lhs - rhs)
		| Lex.Mul -> VInt(lhs * rhs)
		| Lex.Div -> VInt(lhs / rhs)
		| Lex.Pow -> VInt(pow lhs rhs)
		| Lex.Equals -> VBool(lhs = rhs)
		| Lex.LThan -> VBool(lhs < rhs)
		| Lex.LEThan -> VBool(lhs <= rhs)
		| Lex.GThan -> VBool(lhs > rhs)
		| Lex.GEThan -> VBool(lhs >= rhs)
		| _ -> failwith "Type error: Operation not supported on ints"
	in
	let apply_bool_op lhs operator rhs = 
		match operator with
		| _ -> failwith "Type error: Operation not supported on bools"
	in
	let apply_op lhs operator rhs =
		match (lhs, operator, rhs) with
		| (VInt(lhs), operator, VInt(rhs)) -> apply_int_op lhs operator rhs
		| (VBool(lhs), operator, VBool(rhs)) -> apply_bool_op lhs operator rhs
		| _ -> failwith "Type error: Operand types are not the same"
	in
	let rec get_value expr =
		match expr with
		| P.Op (_) as operation -> eval_expr operation env
		| P.Int(value) -> VInt(value)
		| P.Negative(expr) -> 
			let value = 
				match get_value expr with
				| VInt(v) -> v
				| _ -> failwith "Type error: Operation only supported on ints"
			in
			VInt(-value)
		| P.Ident(ident) -> Varmap.find (Var ident) env 
	in
	match expr with
	| P.Op(lhs, operator, rhs) -> 
		let lhs = get_value lhs in
		let rhs = get_value rhs in
		apply_op lhs operator rhs
	| expr ->
		get_value expr

let rec eval_stmts (stmts:P.stmt list) (env:env) =
	let rec eval_loop condition body env = 
		match eval_expr condition env with
		| VBool(v) -> 
			if v then
				let env = eval_stmts body env in
				eval_loop condition body env
			else
				env
		| _ -> 
			failwith "Type Error: While loop conditions should resolve to bools"
	in
	match stmts with
	| [] -> env
	| P.ScopeBlock(inner_stmts)::stmts ->
	(* TODO: Merge these envs correctly *)
		let env = eval_stmts inner_stmts env in
		eval_stmts stmts env
	| P.Assign(ident, expr)::stmts ->
		let value = eval_expr expr env in
		let env = Varmap.add (Var ident) value env in
		eval_stmts stmts env
	| P.While(condition, body)::stmts ->
		(* TODO merge the envs correctly *)
		let env = eval_loop condition body env in
		eval_stmts stmts env
	| P.If (condition, body)::stmts ->
		let env =
			match eval_expr condition env with
			| VBool(v) -> 
				if v then eval_stmts body env 
				else env
			| _ -> failwith "Type Error: If conditions should resolve to bools"
		in
		eval_stmts stmts env
	| P.IfThenElse (condition, true_body, false_body)::stmts ->
		let env =
			match eval_expr condition env with
			| VBool(v) -> 
				if v then eval_stmts true_body env 
				else eval_stmts false_body env
			| _ -> failwith "Type Error: If conditions should resolve to bools"
		in
		eval_stmts stmts env
	| P.Output (expr)::stmts ->
		begin
			match eval_expr expr env with
			| VInt(i) -> print_endline (string_of_int i)
			| VBool(b) -> print_endline (string_of_bool b);
		end;
		eval_stmts stmts env
	| P.Input(ident)::stmts ->
		let value = int_of_string (read_line()) in
		let env = Varmap.add (Var ident) (VInt value) env in
		eval_stmts stmts env



let eval filename = 
	let ast = parse_file filename in
	eval_stmts [ast] Varmap.empty
