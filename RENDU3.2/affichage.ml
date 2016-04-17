open Types

(* affiche une clause *)
let rec affiche_clause (c:clause) : unit = match c with
	[] -> ()
	|t::[] -> print_int t; print_string " 0";
	|t::q ->
	(
			print_int t;
			print_string " ";
			affiche_clause q
	)

(* affiche une expression *)
let rec affiche_expr (e:expr) : unit =
  let aff_aux s a b = 
      begin
    print_string "(";
	affiche_expr a;
	print_string s;
	affiche_expr b;
	print_string ")"
      end
  in
  let aff_aux_1 s a = 
      begin
	print_string s;
	affiche_expr a;
      end
  in
  match e with
  | Var k -> print_int k
  | And(e1,e2) -> aff_aux " /\\ " e1 e2
  | Or(e1,e2) -> aff_aux " \\/ " e1 e2
  | Xor(e1,e2) -> aff_aux " X " e1 e2
  | Impl(e1,e2) -> aff_aux " => " e1 e2
  | Equ(e1,e2) -> aff_aux " <=> " e1 e2
  | Not(e1) -> aff_aux_1 " ~ " e1

(* affiche une liste de type quelconque *)
let print_list l =
	let string_of_list l = 
		let rec aux l = match l with 
			[] -> ""
			|t::q -> 
				let sep = ref "," in
				if q = [] then sep := "";
				(string_of_int t)^(!sep)^(aux q)
		in
		"["^(aux l)^"]"
	in
	print_string (string_of_list l)

(* affiche une FNC *)
let print_fnc (fnc:fnc) : unit =
	print_string "[";	
	
	let print_list l =
		let string_of_list l = 
			let rec aux l = match l with 
				[] -> ""
				|t::q -> 
					let sep = ref "\\/" in
					if q = [] then sep := "";
					(string_of_int t)^(!sep)^(aux q)
			in
			"("^(aux l)^")"
		in
		print_string (string_of_list l)
	in

	
	let rec auxFNC fnc = match fnc with
		[] -> ()
		|(clause,n)::[] -> print_int n;
					print_string "@";
					print_list clause;
		|(clause,n)::q ->
					print_int n;
					print_string "@";
					 print_list clause;
					print_string "/\\";
					auxFNC q;
	in
	
	auxFNC fnc;
	print_string "]"

let print_clause l =
	let string_of_list l = 
		let rec aux l = match l with 
			[] -> ""
			|t::q -> 
				let sep = ref "\\/" in
				if q = [] then sep := "";
				(string_of_int t)^(!sep)^(aux q)
		in
		"("^(aux l)^")"
	in
	print_string (string_of_list l)

(* affiche une liste de paris ou une affectation *)
let print_paris l =
	let string_of_list l = 
		let rec aux l = match l with 
			[] -> ""
			|(t,b)::q -> 
				let sep = ref "," in
				if q = [] then sep := "";
				let affec = ref "true" in
				if b = false then affec := "false";
				"("^(string_of_int t)^","^(!affec)^")"^(!sep)^(aux q)
		in
		"["^(aux l)^"]"
	in
	print_string (string_of_list l)
	
let print_valuation (v:affectations) : unit =
		let rec aux l = match l with 
			[] -> ()
			|(t,b,n,l_deduit)::q -> 
				if b = false then (print_string(" -"); print_int(t); aux q;)
				else (print_string(" "); print_int(t); aux q;);
		in
		aux v;
		print_string(" 0\n");
		


