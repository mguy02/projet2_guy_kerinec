open Types
open Affichage
open Utilitaire
open Dpll_common
open Dpll_moms



let dpll (x:fnc) (h:heuristique) (start_ind:int) (use_tseitin:bool) (clinterac:bool) = 
	
	let fnc_depart = pretraitement x in
	
	let interac = ref clinterac in
	
	
	let valuation = ref [] in
	let liste_var = ref [] in
	let liste_paris = ref [] in
	
	let fnc = ref fnc_depart in	
	let sortie = ref 0 in	(* 0: pas fini
							   1: fini et marché
							   2: backtrack
							   3: fini et pas marché *)
	liste_var := getVarList fnc_depart;
	
		
	while (!sortie = 0 || !sortie = 2) do
		
		let litt = ref 0 in
		let affec = ref true in
		
		let essai = ref PARI in
		
		(* si c'est une "boucle normale" *)
		if !sortie = 0 then
		(
			(* on verifie s'il y a une clause unitaire *)
			let unitaire = trouve_clause_unitaire !fnc in
			
			
			if unitaire <> None then
			(
				
				match unitaire with
				Some (t,b) ->
					(			
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
					match pure with
					Some (t,b) ->
						(			
							litt := abs t;	(* doit etre > 0 *)
							affec := b;
							essai := DEDUCTION;
						)
					|None -> ()
				)
				(* s'il y a ni litteral pur ni clause unitaire, on fait un pari ! *)
				else
				(

					match h with
					NORMAL -> litt := List.hd !liste_var
					|MOMS -> litt := mostFreqLitt !fnc
					;
										
					affec := true;					
					
				)
			)
			
		)	(* fin sortie=0 cas d'une boucle normale *)
		
		(* sinon si c'est un backtrack *)
		else if !sortie = 2 then
		(	
			(* On stop si c'est le mode interactif *)
			if !interac = true then
			(
				
				let char = ref "ocaml" in
				
				while !char <> "c" && !char <> "t" && !char <> "g" do
					print_endline "Conflit trouve ! Que faire ?";
					char := input_line stdin;	
				done;
				
				(* c : continuer jusqu'au prochain conflit *)
				if !char = "c" then
					()
				(* t : on va jusqu'au bout sans s'arreter *)
				else if !char = "t" then
				(
					interac := false
				)
				(* g : engendrer un fichier au format dot/graphviz decrivant le graphe des conflits *)
				else
				(
					print_endline "Pas encore codé !"
				)
			);
		
		
			let break = ref false in
		
			while (!break = false && !liste_paris <> []) do		
				(* On recupere le dernier pari *)
				let (a,b) = take_last !liste_paris in
				litt := a;
				affec := b;		
				
				liste_paris := supprimer (!litt,!affec) !liste_paris;
				
				valuation := supprimer (!litt,!affec) !valuation;
				
				(* Si b = true, on passe b a false et on enleve la valuation *)
				(* on n'oublie pas non plus de retablir la fnc *)
				if b = true then
				(
					(* on retabli la fnc *)
					
					fnc := retabli_fnc fnc_depart !liste_paris;
					liste_var := getVarList !fnc;
				
					affec := false;
					
					break := true;
					
					sortie := 0;
				);
				liste_var := ajoute_inser !litt !liste_var;

			done;

			(* si on a tout enlevé, aie aie aie *)
			if !break = false && !liste_paris = [] then
			(
				print_string "NON SATISFIABLE\n";
				sortie := 3;
			)
		);
		
		if !sortie <> 3 then
		(
			valuation := ajoute (!litt,!affec) !valuation;
		
			(* on nettoie *)
			
			if !affec = false then
			(
				fnc := nettoyage (-1 * !litt) !fnc;	
			)
			else
			(
				fnc := nettoyage !litt !fnc;
			);			
		
			(* on retire la variable de la liste des variables *)
			liste_var := supprimer !litt !liste_var;
			
		
			(* et on la rajoute à la liste des paris, si c'en est un*)
			if !essai = PARI then
				liste_paris := ajoute (!litt,!affec) !liste_paris;
			
			
			if !fnc = [] then
			(
				print_string "SATISFIABLE\n";
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
				(* Raté ! On backtrack ! *)
				sortie := 2;
			)
		)
	done

