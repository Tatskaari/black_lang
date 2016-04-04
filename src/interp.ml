let interp filename =
	let file = Files.read_file filename in
	let tokens = Lex.lex file [] 1 in
	Parser.parse tokens 
