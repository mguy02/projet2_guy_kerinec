open Types
open Affichage
open Utilitaire

let aff_paris t b = print_endline ("On met "^soi(t)^" a "^sob(b))

let print_color l =
	let string_of_list l = 
		let rec aux l = match l with 
			[] -> ""
			|(t,b,n,c)::q -> 
				let sep = ref "," in
				if q = [] then sep := "";
				"("^(string_of_int t)^","^sob(b)^","^(soi n)^","^c^")"^(!sep)^(aux q)
		in
		"["^(aux l)^"]"
	in
	print_string (string_of_list l)


let ecrit_uip_pot l fichier =
	let string_of_list l = 
		let rec aux l = match l with 
			[] -> ""
			|(t,e)::q -> 
				let sep = ref "," in
				if q = [] then sep := "";
				"("^(string_of_int t)^","^(soi e)^")"^(!sep)^(aux q)
		in
		"Uip potentiel : ["^(aux l)^"]\n"
	in
	ecrire fichier (string_of_list l)



let ecrit_newclause fichier l =
	let string_of_list l = 
		let rec aux l = match l with 
			[] -> ""
			|t::q -> 
				let sep = ref "," in
				if q = [] then sep := "";
				(string_of_int t)^(!sep)^(aux q)
		in
		"Clause a apprendre : ["^(aux l)^"]\n"
	in
	ecrire fichier (string_of_list l)

let ecrire_paris fichier l =
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
	ecrire fichier (string_of_list l)


let debug_valuation (v:affectations) : unit =
		let rec aux l = match l with 
			[] -> ()
			|(t,b,n,l_deduit)::q -> 
				print_string "(";
				if b = false then (print_string(" -"); print_int(t);)
				else (print_string(" "); print_int(t););
				print_string ",";
				print_int n;
				print_string ",";
				print_list l_deduit;
				print_string ")";
				aux q;
		in
		aux v;
		

