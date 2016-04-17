open Types
open Dpll_common
open Affichage
open Utilitaire

let print_list l =
	let string_of_list l = 
		let rec aux l = match l with 
			[] -> ""
			|(a,b)::q -> 
				let sep = ref "," in
				if q = [] then sep := "";
				(string_of_int a)^"_"^(string_of_int b)^(!sep)^(aux q)
		in
		"["^(aux l)^"]"
	in
	print_string (string_of_list l)

let print_list2 l =
	let string_of_list l = 
		let rec aux l = match l with 
			[] -> ""
			|a::q -> 
				let sep = ref "," in
				if q = [] then sep := "";
				(string_of_int a)^(!sep)^(aux q)
		in
		"["^(aux l)^"]"
	in
	print_string (string_of_list l)



(* NEW !!! *)

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
		(*print_string "--\n";
		print_int !tailleMin;
		print_newline();
		print_list !occurences;
		print_newline();
		print_string "On regarde la clause ";
		print_list2 santa;
		print_newline();*)
		let n=List.length santa in 
		if n< !tailleMin then (tailleMin:=n; occurences:=[]; parcours santa; aux clauses)
		else if !tailleMin=n then (parcours santa; aux clauses)
		else aux clauses
	
	in
	aux better_fnc;

	(*print_string "\noccurences :	";
	print_newline();
	print_list !occurences;*)
	
	
	let rec getMaxLitt l solution max_occ = match l with
	[] -> solution
	|(litt,occ)::q -> if occ > max_occ then getMaxLitt q litt occ else getMaxLitt q solution max_occ
	in
	
	
	getMaxLitt !occurences 0 0
		
	

let dpll_moms (x:fnc) start_ind use_tseitin = 
	
	let fnc_depart = pretraitement x in
	
	let valuation = ref [] in
	let liste_var = ref [] in
	let liste_paris = ref [] in
	
	let fnc = ref fnc_depart in	
	let sortie = ref 0 in	(* 0: pas fini
							   1: fini et marché
							   2: backtrack
							   3: fini et pas marché *)
	liste_var := getVarList fnc_depart;
	
	
	print_string "fnc : ";
	print_fnc !fnc;
	print_newline();
	
	while (!sortie = 0 || !sortie = 2) do
		print_string "-------\n";
		
		let litt = ref 0 in
		let affec = ref true in
		
		let essai = ref PARI in
		
		(* si c'est une "boucle normale" *)
		if !sortie = 0 then
		(
			(* on verifie s'il y a une clause unitaire *)
			let unitaire = trouve_clause_unitaire !fnc in
			(*let unitaire = None in*)
			
			
			if unitaire <> None then
			(
				print_string ("CLAUSE UNITAIRE TROUVEE : ");
				
				match unitaire with
				Some (t,b) ->
					(
						print_int t;
						print_newline();
				
						litt := abs t;	(* doit etre > 0 *)
						affec := b;
						essai := DEDUCTION;
					)
				|None -> ()
			)
			else	(* s'il n'y a pas de clause unitaire*)
			(	
				(* on verifie s'il y a un litteral pur *)
				let pure = trouve_litteral_pur !fnc in
				
				if pure <> None then
				(
					print_string ("LITTERAL PUR TROUVE : ");
					
					match pure with
					Some (t,b) ->
						(
							print_int t;
							print_newline();
							
							litt := abs t;	(* doit etre > 0 *)
							affec := b;
							essai := DEDUCTION;
						)
					|None -> ()
				)
				(* s'il y a ni litteral pur ni clause unitaire, on fait un pari ! *)
				else
				(
					(* on met la premiere variable à vrai *)
					
					litt := mostFreqLitt !fnc;
					affec := true;
				)
			)
			
			(*print_string "On met ";
			print_int !litt;
			print_string " a vrai.\n";*)
		)	(* fin sortie=0 cas d'une boucle normale *)
		
		(* sinon si c'est un backtrack *)
		else if !sortie = 2 then
		(
			(*print_string "-BACKTRACK TIME !- \n";*)
			
			let break = ref false in
		
			while (!break = false && !liste_paris <> []) do		
				(* On recupere le dernier pari *)
				let (a,b) = take_last !liste_paris in
				litt := a;
				affec := b;
			
				(*print_string "On prend ";
				print_int !litt;
				print_newline();*)
				
				
				liste_paris := supprimer (!litt,!affec) !liste_paris;
				
				valuation := supprimer (!litt,!affec) !valuation;
				
				
				
				
				
				
				(* Si b = true, on passe b a false et on enleve la valuation *)
				(* on n'oublie pas non plus de retablir la fnc *)
				if b = true then
				(
					(* on retabli la fnc *)
				(*print_string "On retabli la fnc, qui vaut maintenant : ";*)
				fnc := retabli_fnc fnc_depart !liste_paris;
				liste_var := getVarList !fnc;
				(*List.iter (print_list) !fnc;
				print_newline();*)
					
				
					affec := false;
					(*print_string "On met ";
					print_int !litt;
					print_string " a faux.\n";*)
					
					break := true;
					(*print_string "Les nouveaux paris sont : ";
					print_paris !liste_paris;
					print_newline();*)
					
					sortie := 0;
				);
			
				liste_var := ajoute_inser !litt !liste_var;

			done;
			(*print_string "...on sort du while break...\n";*)
			(* si on a tout enlevé, aie aie aie *)
			if !break = false && !liste_paris = [] then
			(
				print_string "On a perdu la guerre\n";
				sortie := 3;
			)
		);
		
		if !sortie <> 3 then
		(
			valuation := ajoute (!litt,!affec) !valuation;
		
			(* on nettoie *)
			(*print_string "On nettoie avec ";*)
			
			if !affec = false then
			(
				(*print_string "-";
				print_int !litt;
				print_string ".\n";*)
				fnc := nettoyage (-1 * !litt) !fnc;
				
			)
			else
			(
				(*print_int !litt;
				print_string ".\n";*)
				fnc := nettoyage !litt !fnc;
			);			
			

			print_string "La nouvelle fnc devient : ";
			print_fnc !fnc;
			print_newline();
		
			(* on retire la variable de la liste des variables *)
			(*print_string "La liste_var etait : ";
			print_list !liste_var;
			print_newline();
			print_string "On retire la variable.\n";*)
			
			(* OLD : liste_var := List.tl !liste_var; *)
			liste_var := supprimer !litt !liste_var;
			
			print_string "La nouvelle liste_var est : ";
			Affichage.print_list !liste_var;
			print_newline();
		
			(* et on la rajoute à la liste des paris *)
			(*print_string "On ajoute la variable aux paris.\n";*)
			
			if !essai = PARI then
				liste_paris := ajoute (!litt,!affec) !liste_paris;
			
			
			print_string "Les nouveaux paris sont : ";
			print_paris !liste_paris;
			print_newline();
			
			if !fnc = [] then
			(
				print_string "------\n";
				print_string "on a gagné !\n";
				sortie := 1;
				
				if !liste_var <> [] then
				(
					let rec ajouteFin l = match l with
					[] -> ()
					|t::q -> valuation := ajoute (t,true) !valuation;
						ajouteFin q
					in
					ajouteFin !liste_var;
				);
				
				if use_tseitin = true then
				(
					valuation := post_traitement !valuation start_ind;
				);
				
				print_string "Valuation finale : ";
				print_paris !valuation;
				print_newline();
			)
			else if aUneClauseVide !fnc then
			(
				print_string "on a perdu la bataille, mais pas la guerre\n";
				sortie := 2;
			)
		)
	done;
