open Types
open Utilitaire
open Affichage
open Debug

(*let colorie_conflit sommet fichier etage =
	let bloc1 = "\""^soi(sommet)^"\ntrue@"^soi(etage)^"\"" in
	let bloc2 = "\""^soi(sommet)^"\nfalse@"^soi(etage)^"\"" in


	ecrire fichier (bloc1^"[shape=circle, style=filled, fillcolor=red];\n");
	ecrire fichier (bloc2^"[shape=circle, style=filled, fillcolor=red];\n")
*)

let rec ajoute_coloration (litt, bLitt, nivLitt, colLitt) l = match l with
	[] -> (litt,bLitt, nivLitt, colLitt)::[]
	|(t, b,n, c)::q -> 
		if (litt = t && b = bLitt) then l else (t,b,n,c)::(ajoute_coloration (litt,bLitt, nivLitt, colLitt) q)

let rec colorie liste_coloration fichier = match liste_coloration with
	[] -> ()
	|(t,b,n,c)::q -> 
		(
			let bloc1 = "\""^soi(t)^"\n"^sob(b)^"@"^soi(n)^"\"" in
			ecrire fichier (bloc1^"[shape=circle, style=filled, fillcolor="^c^"];\n");
			colorie q fichier;
		)

let liaison val_u val_v niv_u niv_v niv_courant fichier liste_coloration = 
	let (u,bu) = val_u in
	let (v,bv) = val_v in

	print_endline ("On fait une liaison de "^soi(u)^" vers "^sob(bv)^soi(v)^" ! Liste coloration :");
	(*print_color liste_coloration;
	print_newline();*)
	
	
	let bloc1 = soi(u)^"\n"^sob(bu)^"@"^soi(niv_u) in
	let bloc2 = soi(v)^"\n"^sob(bv)^"@"^soi(niv_v) in
	let s = "\""^bloc1^"\" -> \""^bloc2^"\";\n" in
	ecrire fichier s;
	
	let col_u = ref "none" in
	let col_v = ref "none" in
	
	if (niv_u = niv_courant) then (col_u := "blue"; print_endline (soi(u)^" en bleu !"));
	if (niv_v = niv_courant) then (col_v := "blue"; print_endline (soi(v)^" en bleu !"));
	
	if (!col_u <> "none") then
	(
		let lr = ajoute_coloration (u,bu,niv_u, !col_u) liste_coloration in
		if (!col_v <> "none") then
		(
			ajoute_coloration (v,bv,niv_v,!col_v) lr
		)
		else
		(
			lr
		)
		
	)
	else if (!col_v <> "none") then
	(
		ajoute_coloration (v,bv,niv_v,!col_v) liste_coloration
	)
	else
	(
		liste_coloration
	)

(*let liaisons_deductions liste_paris valuation listeNiveaux*)

let generate_conflict_graph (liste_paris:affectations) (valuation:affectations) (ancienne_fnc:fnc) (fnc_depart:fnc): unit =
	(* On part de l'étage 1 et on va jusqu'a l'étage envoyé en parametre *)
	
	let etage_max = ref 0 in
	let liste_coloration = ref [] in
	
	let creerListeNiveaux () =
		let rec parcoursValuations attendu aff currLevel = match aff with
		[] -> [], []
		|(t,b)::q -> 
			if (t = attendu) then
			(
				(t,currLevel+1)::[], q
			)
			else
			(
				let x,y = parcoursValuations attendu q currLevel in
				(t,currLevel)::x, y
			)
		in
		
		let rec parcoursParis l aff currLevel = match l with
		[] -> let x,y = parcoursValuations 0 aff currLevel in x
		|(t,b)::q -> 
			let aAjouter, suiteParis = parcoursValuations t aff currLevel in
			aAjouter@(parcoursParis suiteParis aff (currLevel+1))
		in
		
		
		
		match liste_paris with
		[] -> []
		|t::q -> parcoursParis q valuation 1
	in

	let niveaux = creerListeNiveaux () in

	let rec trouve_valu litt l = match l with
	[] -> 	print_endline ("pas de valuation pour "^soi(litt)); failwith "Pas d'affectation pour ce litteral"
	|(t,b)::q -> 
		if (abs(litt) = t) then (t,b) else trouve_valu litt q
	in
	
	let rec trouve_valu_conflit litt l = match l with
	[] -> 	print_endline ("pas de valuation pour "^soi(litt)); failwith "Pas d'affectation pour ce litteral"
	|(t,b)::q -> 
		if (abs(litt) = t && litt > 0) then (t,b) else if (abs(litt) = t && litt < 0) then (t,not(b)) else trouve_valu_conflit litt q
	in
	
	let rec getNiveau litt l = match l with
	[] -> print_endline ("Pas de niveaux pour "^soi(litt)); failwith "";
	|(t,niv_t)::q -> if abs(litt) = t then niv_t else getNiveau litt q
	in
	

	
	let rec creer_dep littConflit valuLittConflit niv_littConflit clause fichier = match clause with
	[] -> ()
	|t::q -> if (t <> littConflit) then 
		(
			let niv_t = getNiveau t niveaux in
			let valuT = trouve_valu t valuation in
			(*let (t,b) = valuT in*)
			(*if (b = true) then print_endline ("On a trouve la valuation true pour "^soi(t))
			else print_endline ("On a trouve la valuation false pour "^soi(t));*)
			liste_coloration := liaison valuT valuLittConflit niv_t niv_littConflit niv_littConflit fichier	!liste_coloration
		);
		creer_dep littConflit valuLittConflit niv_littConflit q fichier
	in

	let rec parcours_fnc numClause littConflit fnc fichier = match fnc with
	[] -> ()
	|(clause,n)::q ->
		if (n <> numClause) then parcours_fnc numClause littConflit q fichier (*si c'est pas la clause qu'on cherche, on saute *)
		else 
			(
				print_endline ("appel de creer_dep"^soi(littConflit));
				print_string "clause : ";
				print_clause clause;
				creer_dep littConflit (trouve_valu_conflit littConflit valuation) !etage_max clause fichier
			)

	
	in
	
	let rec parcours_ancienne_fnc fnc fichier = match fnc with
	[] -> ()
	|(clause,n)::q -> 
		(
			match clause with
			t::[] -> parcours_fnc n t fnc_depart fichier; parcours_ancienne_fnc q fichier
			|_ -> parcours_ancienne_fnc q fichier;
		)
		
	in
	
	(*On ouvre le fichier*)
	try
	(
		let fichier = open_out "output.dot" in
		
		(*On écrit le header *)
		ecrire fichier "digraph G {\n";
		ecrire fichier " size = \"4,4\";\n";

		let (conflit,_) = take_last valuation in
		etage_max := (getNiveau (abs(conflit)) niveaux) -1;
		
		(*colorie_conflit (abs(conflit)) fichier !etage_max; *)
 		
		
		parcours_ancienne_fnc ancienne_fnc fichier;
		
		colorie !liste_coloration fichier;
		
		ecrire fichier "}";
		
		close_out fichier;
		
		print_string "valuation\n";
		print_valuation valuation;

	)	
	with _ -> failwith "Erreur dans l'ouverture du fichier pour generer un Graphivz."
	

