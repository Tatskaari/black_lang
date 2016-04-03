open Printf


let read_file file_name =
	let rec read_file in_chan file =
		let line = 
			try
				Some (input_line in_chan) 
			with
			| End_of_file -> 
					close_in_noerr in_chan;
					None
			| e ->       
					close_in_noerr in_chan;           
					raise e 
		in

		match line with
		| Some line ->
			read_file in_chan (file ^ line ^ "\n")
		| None ->
			file
	in
	let in_chan = open_in file_name in
	read_file in_chan ""

