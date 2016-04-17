open Types
open Dpll_common
open Affichage
open Utilitaire



(* renvoie le litteral le plus fréquent de la plus petite clause NON UNITAIRE *)
let mostFreqLitt (fnc:fnc) : int =
	
	let p l = List.length l >= 2 in
	
	let better_fnc = List.filter p fnc in
	
	let tailleMin = ref (List.length (List.hd better_fnc)) in
	let occurences = ref [] in
	
	
	let rec ajout x l= match l with
	[]->(x, 1)::[]
	|(t,n)::q-> if t=x then (t, n+1)::q
				else (t,n)::(ajout x q)
	in
		
	let rec parcours santa= match santa with
	[]-> ()
	|t::q-> occurences:=ajout t !occurences; parcours q;
	in
	
	let rec aux fnc = match fnc with
	[]->()
	|santa::clauses -> 
		let n=List.length santa in 
		if n< !tailleMin then (tailleMin:=n; occurences:=[]; parcours santa; aux clauses)
		else if !tailleMin=n then (parcours santa; aux clauses)
		else aux clauses
	
	in
	aux better_fnc;	
	
	let rec getMaxLitt l solution max_occ = match l with
	[] -> solution
	|(litt,occ)::q -> if occ > max_occ then getMaxLitt q litt occ else getMaxLitt q solution max_occ
	in
	
	getMaxLitt !occurences 0 0
