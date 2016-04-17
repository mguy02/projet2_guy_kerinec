open Types
open Utilitaire
open Affichage
open Debug


let trouve_valu litt valuation = 
	let rec aux litt l = match l with
	[] -> 	print_endline ("pas de valuation pour "^soi(litt)); failwith "Pas d'affectation pour ce litteral"
	|(t,b,n,ldeduit)::q -> 
		if (abs(litt) = t) then (t,b,n,ldeduit) else aux litt q
	in
	aux litt valuation


let rec ajoute_coloration (litt, bLitt, nivLitt, colLitt) l (force:bool) = match l with
	[] -> (litt,bLitt, nivLitt, colLitt)::[]
	|(t, b,n, c)::q -> 
		if (litt = t && b = bLitt && force=false) then l 
		else if (litt = t && b = bLitt && force = true) then (t,b,n,colLitt)::q
		else (t,b,n,c)::(ajoute_coloration (litt,bLitt, nivLitt, colLitt) q force)

let rec colorie liste_coloration fichier = match liste_coloration with
	[] -> ()
	|(t,b,n,c)::q -> 
		(
			let bloc1 = "\""^soi(t)^"\n"^sob(b)^"@"^soi(n)^"\"" in
			ecrire fichier (bloc1^"[shape=circle, style=filled, fillcolor="^c^"];\n");
			colorie q fichier;
		)

(* Trouve l'uip et construit la clause a apprendre. Au passage, colorie le graphe *)
let construit_uip liste_coloration (valuation:affectations) (parentsConflit:int list) (generate_graph:bool) fichier : var_prop * clause * int =
	
	let bonneVal = List.rev valuation in 	(* on part du bas *)
	
	let creerListeVarEtage valuation =
		let rec aux valu etageVoulu = match valu with
		[] -> []
		|(t,b,n,ldeduit)::q -> if n = etageVoulu then 
		t::(aux q etageVoulu) else []
		in
		
		match valuation with
		[] -> print_endline " valuation vide creerListeVarEtage dans construit_uip"; failwith ""
		|(t,b,n,ldeduit)::q ->aux valuation n
	in
	
	let liste_var = creerListeVarEtage bonneVal in
	let nouvelle_clause = ref [] in

	let uip_potentiel = ref [] in (* liste de couples (a,b) avec a un litteral uip potentiel et b la liste de ses ENFANTS DIRECTS *)
	let niveau = ref 0 in
	

	let rec appartient uip luip = match luip with
	[] -> None
	|(t,enfant)::q -> if t = uip then Some (t,enfant) else appartient uip q
	in


	(* nettoie la liste uip potentiel en enlevant t et tous ses enfants uip pot *)
	let nettoie_uip t =
		(* enleve u dans uip_potentiel *)
		let rec nettoie_uip_seul (u:litteral) (luip:(litteral*litteral) list) (enfantTrouve:litteral): ((litteral*litteral) list)*litteral = match luip with
		[] -> [],enfantTrouve
		|(t,enfant)::q -> 
			if u = t then 
			(
				q,enfant
			)
			else 
			(
				let newlist, enf = (nettoie_uip_seul u q enfantTrouve) in
				(t,enfant)::newlist, enf
			);
		in
		
		let aNettoyer = ref t in
		let continue = ref true in
		
		
		while (!continue = true) do
					
			let luip_final, enfant = nettoie_uip_seul !aNettoyer !uip_potentiel 0 in
			let enfantPot = appartient enfant luip_final in
			uip_potentiel := luip_final;
		
			match enfantPot with
			None -> continue := false;
			|Some (t, enfant) -> aNettoyer := t;
		
		done;
		
	in

	(* ajoute les elements de ldeduit aux uip pot *)
	let rec ajoute_uip_pot littActuel ldeduit = match ldeduit with
	[] -> ()
	|t::q ->
	( 
		if (Utilitaire.appartient t liste_var = true) then
		(
		let estDejaPot = appartient t !uip_potentiel in
		
		match estDejaPot with
		None -> uip_potentiel := (t, littActuel)::(!uip_potentiel);
		|Some(t,enfant) -> nettoie_uip enfant; nettoie_uip littActuel;
		;
		);
		
		ajoute_uip_pot littActuel q;
	)
	in

	

	let ajoute_si_pas_du_niveau_courant ldeduit =
		
		let rec aux l = match l with
		[] -> ()
		|litt::q -> let val_t = trouve_valu litt valuation in
			let (t,bt,niv_t,_) = val_t in
			if (niv_t <> !niveau) then
			(
				if(bt = true) then
				(
					nouvelle_clause := ajoute ((-1*t),niv_t) !nouvelle_clause;
				)
				else
				(
					nouvelle_clause := ajoute (t,niv_t) !nouvelle_clause;
				);
				
			);
				aux q
		in
		aux ldeduit
	in
	
	let enleve_les_papas l_deduit =
		let rec enlevePapa u l = match l with
		[] -> []
		|(t, niv_t)::q -> if abs(t) = u then q else (t,niv_t)::(enlevePapa u q)
		in
		
		let rec auxPapa l = match l with
		[] -> ()
		|t::q -> nouvelle_clause := enlevePapa t !nouvelle_clause; auxPapa q;
		in
		
		auxPapa l_deduit; 	
	in
	
	let rec colorie_fin_en_bleu (l:affectations) : unit = match l with
	[] -> ()
	|(t,b,n,_)::q -> if (n = !niveau) then (
		liste_coloration := ajoute_coloration (t,b,n,"cornflowerblue") !liste_coloration false; colorie_fin_en_bleu q;
		)
	in

	let rec remonteVal l littActuel ldeduit =
		
		if (List.length !uip_potentiel <> 1) then
		(
		
			ajoute_uip_pot littActuel ldeduit;
		
			match l with
			[] -> ()
			|(t,bt,n,ldeduit_t)::q -> if (n <> !niveau) then ()
				else
				(
					if(generate_graph = true) then
					(
						liste_coloration := ajoute_coloration (t,bt,!niveau,"purple") !liste_coloration false
					);
					ajoute_si_pas_du_niveau_courant ldeduit_t;
					remonteVal q t ldeduit_t
				)
		)
		else if (generate_graph = true) then
		(
			colorie_fin_en_bleu l
		)
	in



	match bonneVal with
	[] -> print_endline "Erreur construit_uip"; failwith "";
	|(startLitt, _,niv,_)::q -> niveau := niv;
		if(generate_graph = true) then
		(
			liste_coloration := ajoute_coloration (startLitt,true,!niveau,"purple") !liste_coloration false;
			liste_coloration := ajoute_coloration (startLitt,false,!niveau,"purple") !liste_coloration false;
		);
		remonteVal q startLitt parentsConflit
	;
	let uip = (fst (List.hd !uip_potentiel)) in

	let val_uip = trouve_valu uip bonneVal in
	
	(*on enleve ses papas *)
	enleve_les_papas (ldeduit_val val_uip);
	
	(* on calcule le niveau du backtrack *)
	let niveauBacktrack = List.fold_left max 0 (List.map snd !nouvelle_clause) in
	
	(*et on ajoute l'uip *)
	if (bool_val val_uip = true) then
	(
		nouvelle_clause := (!nouvelle_clause)@[(-1*uip,0)];
	)
	else
	(
		nouvelle_clause := (!nouvelle_clause)@[(uip,0)];
	);
	
	val_uip, (List.map fst !nouvelle_clause), niveauBacktrack
	
	

(* u -> v *)
let liaison_direct val_u val_v fichier =
	let (u,bu, niv_u, ldeduit_u) = val_u in
	let (v,bv, niv_v, l_deduit_v) = val_v in
	
	let bloc1 = soi(u)^"\n"^sob(bu)^"@"^soi(niv_u) in
	let bloc2 = soi(v)^"\n"^sob(bv)^"@"^soi(niv_v) in
	let s = "\""^bloc1^"\" -> \""^bloc2^"\";\n" in
	ecrire fichier s

let liaison_dep valuation fichier = 
	let (conflit,_,nmin,_) = take_last valuation in
	
	(* Pour chaque valuation, on fait une liaison vers celui dont le litteral en question depend *)
	
	
	(* fait une fleche de chaque element de lDep vers litt(!= conflit) *)
	let rec auxDep val_u lDep = match lDep with
	[] -> ()
	|t::q -> 
		let (u,bu,niv_u) = val_u in (* on n'a pas envoyé la liste de deduction dans val_u car inutile *)
		
		let (litt_t,bt,niv_t,ldeduit_t) = trouve_valu t valuation in
		
		liaison_direct (litt_t,bt,niv_t,ldeduit_t) (u,bu,niv_u,[]) fichier;
		
		auxDep val_u q
	in
	
	let rec auxVal l = match l with
	[] -> ()
	|(t,b,n,ldeduit)::q -> 
		if (n = nmin && abs(t) <> abs(conflit)) then 
		(	
			auxDep (t,b,n) ldeduit
		);		
		auxVal q
	in

	auxVal valuation
	
	

let clause_learning (valuation:affectations) (ancienne_fnc:fnc) (fnc_depart:fnc) (generate_graph:bool) : clause*int =
	(* On part de l'étage 1 et on va jusqu'a l'étage envoyé en parametre *)
	
	let liste_coloration = ref [] in
	let parentsConflit = ref [] in
	
	let trouve_valu litt =
		trouve_valu litt valuation
	in
	
	let trouve_valu_conflit litt = 
		let rec aux litt l = match l with
		[] -> 	print_endline ("pas de valuation pour "^soi(litt)); failwith "Pas d'affectation pour ce litteral"
		|(t,b,n,ldeduit)::q -> 
			if (abs(litt) = t && litt > 0) then (t,b,n,ldeduit) else if (abs(litt) = t && litt < 0) then (t,not(b),n,ldeduit) else aux litt q
		in
		aux litt valuation
	in
	
	let rec creer_dep littConflit valuLittConflit clause fichier = match clause with
	[] -> ()
	|t::q -> if (t <> littConflit) then 
		(
			let valuT = trouve_valu t in
			
			if (generate_graph = true) then
			(
				liaison_direct valuT valuLittConflit fichier
			);
			
			
			parentsConflit := Utilitaire.ajoute (abs(t)) !parentsConflit;
		);
		creer_dep littConflit valuLittConflit q fichier
	in

	let rec parcours_fnc numClause littConflit fnc fichier = match fnc with
	[] -> ()
	|(clause,n)::q ->
		if (n <> numClause) then parcours_fnc numClause littConflit q fichier (*si c'est pas la clause qu'on cherche, on saute *)
		else 
			(
				creer_dep littConflit (trouve_valu_conflit littConflit) clause fichier
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
		let fichier = ref stdout in
		
		if (generate_graph = true) then
		(
			fichier := open_out "output.dot";
		
			(*On écrit le header *)
			ecrire !fichier "digraph G {\n";
			ecrire !fichier " size = \"4,4\";\n";		
		
			parcours_ancienne_fnc ancienne_fnc !fichier;
			liaison_dep valuation !fichier ; (* ON SUPPOSE ICI QU'IL N'Y A QU'UN LITTERAL QUI FAIT CONFLIT ! *)
		
		
			let val_uip, clauseAApprendre, niveauBacktrack = construit_uip liste_coloration valuation !parentsConflit generate_graph !fichier in

			let (uip, b_uip, n_uip,_) = val_uip in
		
			liste_coloration := ajoute_coloration (uip,b_uip,n_uip, "yellow") !liste_coloration true;
		
			colorie !liste_coloration !fichier;
		
			ecrire !fichier "}";
		
			close_out !fichier;
		
			print_string "valuation\n";
			debug_valuation valuation;
			flush stdout;
			print_string "oye\n";
		
		
			clauseAApprendre, niveauBacktrack
		)
		else
		( 				
			parcours_ancienne_fnc ancienne_fnc !fichier;		
		
			let _, clauseAApprendre, niveauBacktrack = construit_uip liste_coloration valuation !parentsConflit generate_graph !fichier in

			clauseAApprendre, niveauBacktrack
		)
	)	
	with _ -> failwith "Erreur dans l'ouverture du fichier pour generer un Graphivz."
	

