open Util

type expr = 
| Int of int
| Ident of string
| Op of expr * Lex.operator * expr

type stmt = 
| Assignment of expr * expr
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

let parse_expr tokens =
	let get_value token_loc = 
		match token_loc with
		| (Lex.Int(v), _) -> Int(v)
		| (Lex.Ident(v), _) -> Ident(v)
		| (_, line) -> raise (BadInput ("Unexpected token at line " ^ (string_of_int line)))
	in
	let rec parse_op lhs operator tokens =
		match tokens with
		| (Lex.StmtEnd, _)::tokens -> 
			raise (BadInput "Statement end in operator")
		| (Lex.LParen, _)::token::tokens ->
			let (rhs, tokens) = parse_rhs (get_value token) tokens in
			let op = Op (lhs, operator, rhs) in
			(op, tokens)
		| token::(Lex.Operator(next_operator), line)::tokens -> 
			if (get_precidence next_operator) > (get_precidence operator) then
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
		| (Lex.StmtEnd, _)::tokens -> 
			(lhs, tokens)
		| (Lex.RParen, _)::tokens -> 
			(lhs, tokens)
		| token::tokens ->
			let lhs = get_value token in
			parse_rhs lhs tokens
		| [] -> raise (BadInput "Unexpected end of file")
	in
	match tokens with
	| (Lex.StmtEnd, _)::tokens -> 
		raise (BadInput "Statement end in operator")
	| [] -> raise (BadInput "Unexpected end of file")
	| token::tokens ->
		parse_rhs (get_value token) tokens