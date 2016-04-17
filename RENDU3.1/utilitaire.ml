let string_of_bool b =
	if b = true then "true"
	else "false"

(* raccourcis *)
let soi = string_of_int
let ios = int_of_string
let sob = string_of_bool

(* renvoie vrai si x est dans l *)
let rec appartient x l = match l with
	[] -> false
	|t::q -> if t=x then true else appartient x q


(* ajoute sans doublon *)
let rec ajoute x l = match l with
	[] -> [x]
	|t::q -> if t=x then l else t::(ajoute x q)

(* supprime x de l *)
let rec supprimer x l = match l with
	[] -> []
	|t::q -> if t=x then q else t::(supprimer x q)

(* renvoie le dernier element de l *)
let rec take_last l = match l with
	[] -> print_string "Ca n'etait pas censé arriver...(take_last)"; failwith ""
	|t::[] -> t
	|t::q -> take_last q

(* ajout par insertion croissante *)
let rec ajoute_inser x l = match l with
	[] -> [x]
	|t::q -> if t < x then t::(ajoute_inser x q)
		else x::l

(* ecrit dans un fichier un string s *)
let ecrire (fichier:out_channel) (s:string) : unit = 
	let n = String.length s in
	output fichier s 0 n

(* type var_prop = litteral*bool*int*(int list)
Renvoie le litt d'une valuation *)
let litt_val valu = 
	let (rep,_,_,_) = valu in rep

(* Renvoie le booleen d'une valuation *)
let bool_val valu = 
	let (_,rep,_,_) = valu in rep

(* Renvoie le niveau d'une valuation *)
let niv_val valu = 
	let (_,_,rep,_) = valu in rep

(* Renvoie la liste de deduction d'une valuation *)
let ldeduit_val valu = 
	let (_,_,_,rep) = valu in rep
	
(* Renvoie la liste l avec ses i premiers elements*)
(* Renvoie l si i > taille de la liste *)

let troncate l i = 
	if (i < 0) then (print_string "Erreur troncate: i<0"; failwith "Erreur troncate : i<0");
	
	let rec aux l i c = match l with
	[] -> []
	|t::q -> if (c = i) then [] else t::(aux q i (c+1))
	in
	
	aux l i 0







