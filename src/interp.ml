open Printf;;

let filename = 
	try
		Sys.argv.(1) 
	with
	| Invalid_argument(_) -> failwith "Usage \"black <filename>.bl\"";;

Eval.eval filename;; 