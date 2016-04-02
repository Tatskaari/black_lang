open Printf


let read_file file_name =
	let rec read_file in_chan file =
		let line = 
			try
				input_line in_chan 
			with
			| End_of_file -> 
					close_in_noerr in_chan;
					""
			| e ->       
					close_in_noerr in_chan;           
					raise e 
		in

		if line = "" then file else
		read_file in_chan (file ^ line ^ "\n")
	in
	let in_chan = open_in file_name in
	read_file in_chan ""

