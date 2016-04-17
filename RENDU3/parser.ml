open Str
open Types

let ios = int_of_string

(* Lit le fichier ligne par ligne, et crée l'expression de sortie *)
let parseFile fichier : fnc =

	let headerVu = ref false in
	let indiceMax = ref 0 in
	let nbClauses = ref 0 in
	
	let nbClausesLues = ref 0 in
	let indiceMaxTrouve = ref 0 in
	
	(* En entrée, une liste de litteraux sous forme d'entiers entre guillemets (donc l:string list) *)
	(* Test si c'est sous la bonne forme et renvoie un type clause *)
	let rec check_clause l = 
		let rec aux l r indMax = match l with
			[] -> r, indMax
			|t::[] -> failwith "erreur check_clause"
			|u::v::[] -> if v = "0" && u <> "0"
				then let litt = (ios u) in (litt::r),(max (abs (ios u)) indMax)
				else failwith "erreur check_clause"
			|t::q ->	if t = "0" then failwith "erreur check_clause"
				else 
					(
						let litt = (ios t) in
						aux q (litt::r) (max (abs (ios t)) indMax)
					)
		in
		aux l [] 0
	in
	(* fin check_clause *)
	
	
	(* Prend en entrée une ligne, et renvoie la clause correspondante *)
	let getClause ligne =
		let r = Str.regexp "[' ']+" in
		let tab_litt = Str.split r ligne in
		let clause, indMax = check_clause tab_litt in		
		clause,indMax
	in
	(* fin getClause *)
	
	let rec aux fichier sortie =
		try
		(
			let ligne = input_line fichier in	(* on recupere la prochaine ligne *)
			(* si c'est un commentaire *)
			if (ligne.[0] = 'c') then 
			(
				aux fichier sortie 		(* on saute les commentaires *)
			)
			(* si c'est le header *)
			else if (ligne.[0] = 'p' && ligne.[1] = ' ') then
			(
				
				if (!headerVu = true) then	(* si on a deja vu un header on arrete tout *)
				(	
					print_string "Error: Il ne doit y avoir qu'un header !";
					failwith "Error: Il ne doit y avoir qu'un header !"
				)
				else
				(	
					let r = Str.regexp "[' ']+" in
					let tab = Str.split r ligne in

				
					match tab with
					p::cnf::v::c::[] ->
						(
							let r = Str.regexp "[1-9][0-9]*" in
					
							let test_cnf = (cnf = "cnf") in
							let test_v = (Str.string_match r v 0) in
							let test_c = (Str.string_match r c 0) in
					
							if (not (test_cnf && test_v && test_c)) then
							(
								print_string "Error : header pas au bon format";
								failwith "Error : header pas au bon format"
							)
							else
							(

								indiceMax := ios v;
								nbClauses := ios c;

								headerVu := true;
								aux fichier sortie;
							)
						
						)
					|_ -> (print_string "Error : header pas au bon format (p cnf V C)";
						failwith "Error: header pas au bon format")
				
				)			
			)

			(* si c'est une clause *)
			else if (Str.string_match (Str.regexp "-? ?[1-9][0-9]*") ligne 0) then	(*completer regex *) 
			(
				(* si il y a une clause AVANT le header *)
				if (!headerVu = false) then
				(
					print_string "Error: input file pas dans le bon format";
					failwith "Error: input file pas dans le bon format."
				)
				else
				(
					nbClausesLues := !nbClausesLues +1;
					
					
					let clause, indMax = getClause ligne in	(* on recupere la clause *)
					
					indiceMaxTrouve := max !indiceMaxTrouve indMax;
					

					aux fichier (clause::sortie);
				)
				
			)
			(* sinon, pas du bon format ! *)
			else
			(
				print_string "Error: input file pas dans le bon format\n";
				failwith "Error: input file pas dans le bon format"
			)
		)
		with End_of_file ->
			(
				if !nbClausesLues <> !nbClauses then
				(
					print_string "/!\\ Le fichier comporte ";
					print_int !nbClausesLues;
					print_string " clauses, alors que ";
					print_int !nbClauses;
					print_string " etaient annoncees. /!\\\n";
				);
				if (!indiceMaxTrouve <> !indiceMax) then
				(
					print_string "/!\\ Le fichier comporte un indice max de ";
					print_int !indiceMaxTrouve;
					print_string ", alors que ";
					print_int !indiceMax;
					print_string " etait cense etre la limite. /!\\\n";
				);
				 sortie
			)
			|_ -> print_string "Erreur inconnue\n"; []
			
	in
	(* fin aux *)
	
	aux fichier []
(* fin parseFile *)

