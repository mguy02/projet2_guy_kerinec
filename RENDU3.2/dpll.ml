open Types
open Affichage
open Utilitaire
open Dpll_common
open Dpll_moms
open Dpll_clause_learning
open Debug

let dpll (x:fnc) (h:heuristique) (start_ind:int) (use_tseitin:bool) (cl:bool) (interactif:bool) = 
	
	let fnc_depart = ref (pretraitement x) in (*UPDATE : devenu un ref pour le clause learning *)
	
	let interac = ref interactif in
	
	let valuation = ref [] in
	let liste_var = ref [] in
	let liste_paris = ref [] in
	
	let fnc = ref (!fnc_depart) in	
	
	let ancienne_fnc = ref [] in
	
	let sortie = ref 0 in	(* 0: pas fini
							   1: fini et marché
							   2: backtrack
							   3: fini et pas marché *)
	liste_var := getVarList (!fnc_depart);
	
	let niveauCourant = ref 0 in
	let numClause = ref 0 in
		
	while (!sortie = 0 || !sortie = 2) do
		
		let litt = ref 0 in
		let affec = ref true in
		
		let essai = ref PARI in
		
		(* si c'est une "boucle normale" *)
		if !sortie = 0 then
		(
			(* on verifie s'il y a une clause unitaire *)
			let unitaire, n = trouve_clause_unitaire !fnc in
			
			if unitaire <> None then
			(
				match unitaire with
				Some (t,b) ->
					(			
						litt := abs t;	(* doit etre > 0 *)
						affec := b;
						essai := DEDUCTION;
						numClause := n;
					)
				|None -> ()
			)
			else	(* s'il n'y a pas de clause unitaire*)
			(	
				let pure = ref None in
			
				if (cl = false) then
				(
					(* on verifie s'il y a un litteral pur si pas de clause learning *)
					pure := trouve_litteral_pur !fnc;
				);
				
				if !pure <> None then
				(
					match !pure with
					Some (t,b) ->
						(			
							litt := abs t;	(* doit etre > 0 *)
							affec := b;
							essai := DEDUCTION;
							numClause := 0;
						)
					|None -> ()
				
				)
				else (* s'il y a ni litteral pur ni clause unitaire, on fait un pari ! *)
				(
					
					(
					match h with
					
					NORMAL -> litt := List.hd !liste_var
					|MOMS -> litt := mostFreqLitt !fnc
					);
					affec := true;	
					niveauCourant := !niveauCourant + 1;

				);
			)
			
		)	(* fin sortie=0 cas d'une boucle normale *)
		
		(* sinon si c'est un backtrack *)
		else if !sortie = 2 then
		(	
			
			(* On stop si c'est le mode interactif *)
			if !interac = true then
			(
				print_endline "Conflit trouve !";				
				
				let char = ref "ocaml" in
				
				while !char <> "c" && !char <> "t" && !char <> "g" do
					print_endline "Que faire ?";
					char := input_line stdin;	
				done;
				
				(* c : continuer jusqu'au prochain conflit *)
				if !char = "c" then
				(
					let clauseAApprendre, niveauBacktrack = clause_learning !valuation !ancienne_fnc !fnc_depart false in
					liste_paris := troncate !liste_paris (niveauBacktrack);
					fnc_depart := (clauseAApprendre,List.length !fnc_depart)::(!fnc_depart);
					affiche_clause clauseAApprendre;
					print_newline();
					print_int niveauBacktrack;
					print_newline();
				)
				(* t : on va jusqu'au bout sans s'arreter *)
				else if !char = "t" then
				(
					interac := false
				)
				(* g : engendrer un fichier au format dot/graphviz decrivant le graphe des conflits *)
				else
				(
					let clauseAApprendre, niveauBacktrack = clause_learning !valuation !ancienne_fnc !fnc_depart true in
					liste_paris := troncate !liste_paris (niveauBacktrack);
					fnc_depart := (clauseAApprendre,List.length !fnc_depart)::(!fnc_depart);
					
					affiche_clause clauseAApprendre;
					print_newline();
					print_int niveauBacktrack;
					print_newline();
				);
			) 
			
			else if (cl = true) then
			(
				let clauseAApprendre, niveauBacktrack = clause_learning !valuation !ancienne_fnc !fnc_depart false in
				liste_paris := troncate !liste_paris (niveauBacktrack);
				fnc_depart := (clauseAApprendre,List.length !fnc_depart)::(!fnc_depart);

			);
			
			let break = ref false in
		
			while (!break = false && !liste_paris <> []) do		
				
				(* On recupere le dernier pari *)
				let (a,b) = take_last !liste_paris in
				litt := a;
				affec := b;		
			
				
				liste_paris := supprimer (!litt,!affec) !liste_paris;
				
				valuation := retabli_valuation !litt !valuation;
				
				(* Si b = true, on passe b a false et on enleve la valuation *)
				(* on n'oublie pas non plus de retablir la fnc *)
				if b = true then
				(
					(* on retabli la fnc *)

					fnc := retabli_fnc !fnc_depart !valuation;
					
					liste_var := getVarList !fnc;
				
					affec := false;
					
					break := true;
					
					sortie := 0;
				);
				if (!break = false) then
					liste_var := ajoute_inser !litt !liste_var;

			done;

			(* si on a tout enlevé, aie aie aie *)
			if !break = false && !liste_paris = [] then
			(
				print_string "UNSATISFIABLE\n";
				sortie := 3;
			);
			
		);
		
		if !sortie <> 3 then
		(
			ancienne_fnc := !fnc;
			
			let l_deduit = ref [] in
			
			if (!essai <> PARI && !numClause <> 0) then (* si deduction de clause unitaire *)
			(

				l_deduit := getDeduction (abs(!litt))  !numClause !fnc_depart !valuation
			);
			
			valuation := ajoute (!litt,!affec, !niveauCourant, !l_deduit) !valuation;
		
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
				print_string "SATISFIABLE ";
				sortie := 1;
				
				if !liste_var <> [] then
				(
					let rec ajouteFin l = match l with
					[] -> ()
					|t::q -> valuation := ajoute (t,true, 0,[]) !valuation;
						ajouteFin q
					in
					ajouteFin !liste_var;
				);
				
				if use_tseitin = true then
				(
					valuation := post_traitement !valuation start_ind;
				);
				print_valuation !valuation;
				print_newline();
			)
			else if aUneClauseVide !fnc then
			(
				(* Raté ! On backtrack ! *)
				sortie := 2;
			)
		)
	done

