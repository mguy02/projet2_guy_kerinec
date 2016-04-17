open Types
open Dpll_common
open Affichage
open Utilitaire

let dpll_normal (x:fnc) start_ind use_tseitin= 
	
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
	
	
	(* TESTS *)
	(*
	let test_base =[[1;-2;3];[-1;-2;3];[-1;-3]] in
	fnc := test_base;
	liste_var := [1;2;3];
	*)
	
	print_string "fnc : ";
	List.iter (print_list) !fnc;
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
					litt := List.hd !liste_var;
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
			List.iter (print_list) !fnc;
			print_newline();
		
			(* on retire la variable de la liste des variables *)
			(*print_string "La liste_var etait : ";
			print_list !liste_var;
			print_newline();
			print_string "On retire la variable.\n";*)
			
			(* OLD : liste_var := List.tl !liste_var; *)
			liste_var := supprimer !litt !liste_var;
			
			print_string "La nouvelle liste_var est : ";
			print_list !liste_var;
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
	done
	
	(*print_string "sortie du while\n";*)

