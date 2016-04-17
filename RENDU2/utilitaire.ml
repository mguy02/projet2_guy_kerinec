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

