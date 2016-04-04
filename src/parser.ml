open Util

type expr = 
| Int of int
| Ident of string
| Op of expr * Lex.operator * expr

type stmt = 
| Assign of string * expr
| While of expr * (stmt list)
| If of expr * (stmt list)
| IfThenElse of expr * (stmt list) * (stmt list)
| ScopeBlock of stmt list

let get_precidence operator = 
	match operator with
	| Lex.Mul -> 11
	| Lex.Div -> 11
	| Lex.Add -> 10
	| Lex.Sub -> 10
	| _ -> 0

let parse_expr tokens end_token =
	(* Converts a token*int to an expr *)
	let get_value token_loc = 
		match token_loc with
		| (Lex.Int(v), _) -> Int(v)
		| (Lex.Ident(v), _) -> Ident(v)
		| (_, line) -> raise (BadInput ("Unexpected token at line " ^ (string_of_int line)))
	in
	(* 	Takes a left hand side of an operation, the operation and the rest of the tokens and will
		return an operation.
	 *)
	let rec parse_op lhs operator tokens =
		match tokens with
		| (Lex.StmtEnd, _)::tokens -> 
			raise (BadInput "Statement end in operator")
		| (Lex.LParen, _)::token::tokens ->
			(* Parse the right hand side of the parenthesis with the first token as the left hand side*)
			let (rhs, tokens) = parse_rhs (get_value token) tokens in
			let op = Op (lhs, operator, rhs) in
			(op, tokens)
		| token::(Lex.Operator(next_operator), line)::tokens -> 
			if (get_precidence next_operator) > (get_precidence operator) then
				(* If the next op has higher precidence, parse that first and use the result as the RHS *)
				let (rhs, tokens) = parse_op (get_value token) next_operator tokens in
				let op = Op (lhs, operator, rhs) in
				(op, tokens)
			else
				let op = Op (lhs, operator, (get_value token)) in
				(op, (Lex.Operator(next_operator), line)::tokens)
		| token::tokens -> 
			let op = Op (lhs, operator, (get_value token)) in
			(op, tokens)
		| [] -> raise (BadInput "Unexpected end of file")

	and parse_rhs lhs tokens =
		match tokens with
		| (Lex.Operator(operator), _)::tokens -> 
			let (lhs, tokens) = parse_op lhs operator tokens in
			parse_rhs lhs tokens
		| (token, _)::tokens when token = end_token-> 
			(lhs, tokens)
		| (Lex.RParen, _)::tokens -> 
			(lhs, tokens)
		| token::tokens ->
			parse_rhs (get_value token) tokens
		| [] -> raise (BadInput "Unexpected end of file")
	in
	match tokens with
	| [] -> raise (BadInput "Unexpected end of file")
	| (Lex.StmtEnd, _)::tokens -> 
		raise (BadInput "Statement end in operator")
	| (Lex.LParen, _)::token::tokens ->
		begin
			match parse_rhs (get_value token) tokens with
			| (expr, (token, _)::tokens) when token = end_token -> (expr, tokens)
			| expr_tokens -> expr_tokens
		end
	| token::tokens ->
		parse_rhs (get_value token) tokens


let rec parse_stmt tokens stmts = 
	match tokens with
	| [] -> (stmts, [])
	| (Lex.RBrace, _)::tokens ->
		(stmts, tokens)
	| (Lex.LBrace, _)::tokens ->
		let (inner_stms, tokens) = parse_stmt tokens [] in
		let scope_block = ScopeBlock inner_stms in
		parse_stmt tokens (stmts@[scope_block])
	| (Lex.Ident(ident), _)::(Lex.Keyword Lex.Assign, _)::tokens -> 
		let (expr, tokens) = parse_expr tokens Lex.StmtEnd in
		let assign = Assign (ident, expr) in
		parse_stmt tokens (stmts@[assign])
	| (Lex.Keyword While, _)::tokens ->
		let (condition, tokens) = parse_expr tokens Lex.RParen in
		begin
			match tokens with
			| (Lex.LBrace, _)::tokens -> 
				let (inner_stms, tokens) = parse_stmt tokens [] in
				let stmt = While (condition, inner_stms) in
				parse_stmt tokens (stmts@[stmt])
			| (_, line)::tokens -> raise (BadInput ("Unexpected token at line " ^ (string_of_int line)))
		end
	|  (_, line)::_ -> raise(BadInput ("Unexpected token on line " ^ (string_of_int line)))

