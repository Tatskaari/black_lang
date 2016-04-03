open Printf

exception BadInput of string

type keyword = 
  While
| If 
| Then 
| Assign 

type operator = 
  Minus
| Add
| LThan
| GThan
| Mul
| Div

type token =
	LBrace
|	RBrace
|	LParen
|	RParen
|	Assign
| 	Keyword of keyword
|   Operator of operator 
|	Ident of string
|	Int of int

let keyword_map = [("while", While); ("if", If); ("then", Then); (":=", Assign)]
let operator_map = [("-", Minus); ("+", Add); ("<", LThan); (">", GThan); ("*", Div)]
let token_map = [("{", LBrace); ("}", RBrace); ("(", LParen); (")", RParen); (":=", Assign)]

(* token with a line numer *)
type token_line = token * int

(* Returns the number of characters and the token this predicate is able to match. 0 means no match *)
type token_pred = string -> (token option*int)

let rec string_to_keyword str keyword_map =
	match keyword_map with
	| (key_str, key)::keyword_map -> 
		if (String.compare key_str str) = 0 then
			key
		else
			string_to_keyword str keyword_map
	| _ -> 
		raise (BadInput ("Not a keyword: " ^ str))


let reg_ex_pred regex_str str  =
	let regex = Str.regexp regex_str in
	if Str.string_match regex str 0 then
		let matched_string = Str.matched_string str in
		let char_count = String.length matched_string in
		(Some matched_string, char_count)
	else
		(None, 0)

let get_token_weighting token =
	match token with
	| Some Ident(_) -> 1
	| Some Keyword(_) -> 2
	| _ -> 0

let match_ident str =
	match (reg_ex_pred "[a-zA-Z]+" str) with
	| (Some match_str, len) ->
		(Some (Ident match_str), len)
	| (None, len) -> (None, len)

let match_map map str =
	let rec match_map map =
		match map with
		| (key_str, map_entry)::map -> 
			begin
				match (reg_ex_pred key_str str) with
				| (Some match_str, len) ->
					(Some map_entry, len)
				| (None, len) -> 
					match_map map
			end
		| [] -> 
			(None, 0)
	in
	match_map map

 let match_token = match_map token_map

 let match_keyword str = 
 	match match_map keyword_map str with
 	| (Some keyword, len) -> (Some (Keyword keyword), len)
 	| (None, len) -> (None, len)

 let match_operator str = 
 	match match_map operator_map str with
 	| (Some op, len) -> (Some (Operator op), len)
 	| (None, len) -> (None, len)

let match_int str =
	match (reg_ex_pred "[0-9]+" str) with
	| (Some match_str, len) ->
		(Some (Int (int_of_string match_str)), len)
	| (None, len) -> (None, len)

let (token_preds:token_pred list) = [match_token; match_keyword; match_ident; match_int; match_operator]

let rec find_longest_match str (longest_match: token option * int) token_preds line =
	match token_preds with
	| pred::token_preds -> 
		let (match_token, match_len) as new_match = pred (str) in
		let (longest_match_token, longest_match_len) = longest_match in
		let longest_match = 
			if longest_match_len < match_len then
				 new_match
			else if longest_match_len = match_len then
				if (get_token_weighting longest_match_token) < (get_token_weighting match_token) then
					new_match
				else
					longest_match
			else
				longest_match
		in
		find_longest_match str longest_match token_preds line
	| [] -> 
		begin 
			match longest_match with
			| (Some token, len) -> (token, len)
			| (None, _) -> raise (BadInput ("Unexpected input on line " ^ (string_of_int line)))
		end

let rec trim_program program line_number =
	if program = "" then (program, line_number) else
	let rec trim pos line_number =
		if pos >= (String.length program) then (pos, line_number) else
		match program.[pos] with
			| ' ' | '\r' | '\t' -> trim (pos + 1) line_number
			| '\n' -> trim (pos + 1) (line_number + 1)
			| _ -> (pos, line_number)
	in
	let (pos, line_number) = trim 0 line_number in
	let len = String.length program in
	let program = String.sub program pos (len - pos) in
	(program, line_number)


let rec lex (program : string) (token_lines : token_line list) (line : int): token_line list = 
	let (program, line) = trim_program program line in
	if program = "" then
		token_lines
	else
		let (token, len) = find_longest_match program (None, 0) token_preds line in
		let total_len = String.length program in
		let program = String.sub program len (total_len-len) in
		lex program (token_lines@[(token, line)]) line


