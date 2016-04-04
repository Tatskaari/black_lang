open Util

type expr = 
| Int of int
| Ident of string
| Negative of expr
| Op of expr * Lex.operator * expr

type stmt = 
| Assign of string * expr
| While of expr * (stmt list)
| Output of expr
| Input of string
| If of expr * (stmt list)
| IfThenElse of expr * (stmt list) * (stmt list)
| ScopeBlock of stmt list

let raise_unexpected_input line =
	raise (BadInput ("Unexpected token at line " ^ (string_of_int line)))

let raise_enexpected_eof () = 
	raise (BadInput "Unexpected end of file")


let get_precidence operator = 
	match operator with
	| Lex.Mul -> 11
	| Lex.Div -> 11
	| Lex.Pow -> 11
	| Lex.Add -> 10
	| Lex.Sub -> 10
	| _ -> 0

let parse_expr tokens end_token =
	(* Converts a token*int to an expr *)
	let rec get_value tokens = 
		match tokens with
		| (Lex.Int(v), _)::tokens -> (Int(v), tokens)
		| (Lex.Ident(v), _)::tokens -> (Ident(v), tokens)
		| (Lex.Operator operator, line)::tokens ->
			begin
				match operator with
				| Lex.Add -> get_value tokens
				| Lex.Sub ->
					let (value, tokens) = get_value tokens in
					(Negative(value), tokens)
				| _ -> raise_unexpected_input line
			end
		| (_, line)::_ -> raise_unexpected_input line
		| [] -> raise_enexpected_eof()
	in
	(* 	Takes a left hand side of an operation, the operation and the rest of the tokens and will
		return an operation.
	 *)
	let rec parse_op lhs operator tokens =
		match tokens with
		| (Lex.StmtEnd, line)::tokens -> 
			raise_unexpected_input line
		| (Lex.LParen, _)::tokens ->
			(* Parse the right hand side of the parenthesis with the first token as the left hand side*)
			let (paren_lhs, tokens) = get_value tokens in
			let (rhs, tokens) = parse_rhs paren_lhs tokens in
			let op = Op (lhs, operator, rhs) in
			(op, tokens)
		| token::(Lex.Operator(next_operator), line)::tokens -> 
			let (next_value, _) = get_value [token] in
			if (get_precidence next_operator) > (get_precidence operator) then
				(* If the next op has higher precidence, parse that first and use the result as the RHS *)
				let (rhs, tokens) = parse_op next_value next_operator tokens in
				let op = Op (lhs, operator, rhs) in
				(op, tokens)
			else
				let op = Op (lhs, operator, next_value) in
				(op, (Lex.Operator(next_operator), line)::tokens)
		| [] -> 
			raise_enexpected_eof()
		| tokens -> 
			let (rhs, tokens) = get_value tokens in
			let op = Op (lhs, operator, rhs) in
			(op, tokens)
	and parse_rhs lhs tokens =
		match tokens with
		| (Lex.Operator(operator), _)::tokens -> 
			let (lhs, tokens) = parse_op lhs operator tokens in
			parse_rhs lhs tokens
		| (token, _)::_ when token = end_token -> 
			(lhs, tokens)
		| (Lex.RParen, _)::tokens -> 
			(lhs, tokens)
		| [] -> 
			raise_enexpected_eof()
		| tokens ->
			let (lhs, tokens) = get_value tokens in
			parse_rhs lhs tokens
	in
	let result = match tokens with
	| [] -> raise_enexpected_eof()
	| (Lex.StmtEnd, line)::tokens -> 
		raise_unexpected_input line
	| (Lex.LParen, _)::tokens ->
		let (lhs, tokens) = get_value tokens in
		parse_rhs lhs tokens
	| tokens ->
		let (lhs, tokens) = get_value tokens in
		parse_rhs lhs tokens
	in
	(* Check that after parsing the expressoin, the next token is the end token *)
	begin
		match result with
		| (expr, (token, _)::tokens) when token = end_token -> (expr, tokens)
		| (_, (_,line)::_) -> raise_unexpected_input line
		| _ -> raise (BadInput "Error parsing expression")
	end

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
	| (Lex.Keyword Lex.While, _)::tokens ->
		let (condition, tokens) = parse_expr tokens Lex.RParen in
		begin
			match tokens with
			| (Lex.LBrace, _)::tokens -> 
				let (inner_stms, tokens) = parse_stmt tokens [] in
				let stmt = While (condition, inner_stms) in
				parse_stmt tokens (stmts@[stmt])
			| (_, line)::tokens -> raise_unexpected_input line
			| [] -> raise_enexpected_eof()
		end
	| (Lex.Keyword Lex.If, _)::tokens ->
		let (condition, tokens) = parse_expr tokens Lex.RParen in
		begin
			match tokens with
			| (Lex.LBrace, _)::tokens -> 
				let (true_stms, tokens) = parse_stmt tokens [] in
				let (stmt,tokens) = 
					match tokens with
					| (Lex.Keyword Lex.Else, _)::tokens -> 
						let tokens = 
							match tokens with
							| (Lex.LBrace, _)::tokens -> tokens
							| (Lex.Keyword Lex.If, _)::_ -> tokens
							| (_, line)::_ -> raise_unexpected_input line
							| [] -> raise_enexpected_eof()
						in
						let (false_stmts, tokens) = parse_stmt tokens [] in
						let stmt = IfThenElse (condition, true_stms, false_stmts) in
						(stmt, tokens)
					| _ -> 
						(If (condition, true_stms), tokens)
				in
				parse_stmt tokens (stmts@[stmt])
			| (_, line)::_ -> raise_unexpected_input line
			| _ -> raise (BadInput "Error parsing if statement") 
		end
	| (Lex.Keyword Lex.Output, _)::tokens ->
		let (expr, tokens) = parse_expr tokens Lex.StmtEnd in
		parse_stmt tokens (stmts@[Output expr])
	| (Lex.Keyword Lex.Input, _)::(Lex.Ident(ident), _)::(Lex.StmtEnd, _)::tokens ->
		parse_stmt tokens (stmts@[Input ident])
	|  (_, line)::_ -> raise_unexpected_input line

let parse tokens = 
	match parse_stmt tokens [] with
	| ([ast], []) -> ast
	| (_, (_, line)::_) -> raise_unexpected_input line
	| (_::_, _) -> raise (BadInput "Syntax Error: No program root") 
	| ([], []) -> raise(BadInput "No program to parse")

