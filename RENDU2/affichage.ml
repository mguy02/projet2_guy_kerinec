open Expr

let rec affiche_clause c = match c with
	[] -> ()
	|t::[] -> print_int t
	|t::q -> (print_string ("Or("); 
			print_int t;
			print_string ",";
			affiche_clause q;
			print_string ")";)

let rec affiche_expr e = match e with
	[] -> ()
	|c::[] -> affiche_clause c
	|c::q -> (print_string"And(";
			affiche_clause c;
			print_string ",";
			affiche_expr q;
			print_string ")";)

let print_list l =
	let string_of_list l = 
		let rec aux l = match l with 
			[] -> ""
			|t::q -> 
				let sep = ref "," in
				if q = [] then sep := "";
				t^(!sep)^(aux q)
		in
		"["^(aux l)^"]"
	in
	print_string (string_of_list l)



