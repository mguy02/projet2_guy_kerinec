open Types
open Affichage
open Utilitaire

(* elimine les tautologies *)
(* Regarde dans chaque clause s'il y a une tautologie *)
let rec tautology_elimination (fnc:fnc) : fnc =
	
	let rec isTautology clause = match clause with
		[] -> false
		|t::q -> if (appartient (-1*t) q) then true else isTautology q
	in
	
	match fnc with
		[] -> []
		|clause::q -> if (isTautology clause) then tautology_elimination q
			else clause::(tautology_elimination q)


let nettoyage (var:int) (fnc:fnc) : fnc = 
	
	(*print_string "APPEL DE NETTOYAGE avec la FNC : ";
	List.iter (print_list) fnc;
	print_newline();*)
	
	let rec auxLitt l lr estSeul = match l with
	[] -> lr
	|t::q -> if abs t = abs var then
		(
			if t = var then auxLitt [] [] estSeul
			else 
			(
				(* Soit -t est seul et donc on l'enleve *)
				if estSeul && q = [] then
				(
					(* 0 signifie clause vide *)
					auxLitt [] [0] true
				)
				(* Soit -t n'est pas seul et donc on l'enleve *)
				else
				(
					auxLitt q lr false
				)
			)
		)
		else
		(
			auxLitt q (t::lr) false
		)
	in
	
	let rec auxClause l = match l with
	[] -> []
	|clause::q -> 
			(auxLitt clause [] true)::(auxClause q)
	in
	
	(* enleve les clauses vides *)
	let rec repassage l = match l with
		[] -> []
		|clause::q -> if clause = [] then repassage q
			else if clause = [0] then []::(repassage q)
			else clause::(repassage q)
	in
	
	let retour = repassage (auxClause fnc) in
	
	(*print_string "VALEUR DE RETOUR : ";
	List.iter (print_list) retour;*)
	retour


let rec aUneClauseVide (fnc:fnc) : bool = match fnc with
	[] -> false
	|clause::q -> if clause = [] then true else
		aUneClauseVide q


let rec retabli_fnc (fnc:fnc) l : fnc = match l with
	[] -> fnc;
	|(t,b)::q -> (
		let temp = ref 0 in
		if b then temp := t
		else temp := -t;
	
		let next_fnc = nettoyage !temp fnc in
	retabli_fnc next_fnc q
	)


let getVarList (fnc:fnc) : int list = 
	
	let listeVar =  ref [] in
	
	let rec insertion x l = match l with
		[] -> [x]
		|t::q -> if x = t then l
			else if x > t then t::(insertion x q)
			else x::l
	in 
	
	let rec auxClause clause = match clause with
		[] -> ()
		|litt::suite -> listeVar := insertion (abs litt) !listeVar;
			auxClause suite;
	in
	
	let rec auxFNC (fnc:fnc) = match fnc with
		[] -> ()
		|clause::q -> auxClause clause; auxFNC q;
	in
	
	auxFNC fnc;

	!listeVar
	
(* TODO : VOIR SI x = [] *)

let trouve_litteral_pur (fnc:fnc) : var_prop option = 

	let liste_vus = ref [] in
	let pur_potentiels = ref [] in
	
	let rec auxLitt clause = match clause with
		[] -> ()
		|t::q -> 
			begin
				
				if appartient (-1*t) !pur_potentiels then
				(
					pur_potentiels := supprimer (-1*t) !pur_potentiels;
					liste_vus := ajoute t !liste_vus;
				)
				else if appartient t !liste_vus = false then
				(
					pur_potentiels := ajoute t !pur_potentiels;
					liste_vus := ajoute t !liste_vus;
				);
				(*print_list !liste_vus;
				print_newline();
				print_list !pur_potentiels;
				print_string("\n-----\n");*)
				auxLitt q
			end
	in
	
	let rec auxClause fnc = match fnc with
		[] -> ()
		|clause::q -> auxLitt clause;
			auxClause q
	in
	
	auxClause fnc;
	
	(*print_list !liste_vus;
	print_newline();
	print_list !pur_potentiels;
	print_string("\n-----\n");*)
	
	match !pur_potentiels with
	[] -> None
	|t::q -> Some (t, t>0)


let trouve_clause_unitaire (fnc:fnc) : var_prop option = 
	let rec aux acc clauses = match acc with
		None -> (
			match clauses with
			|[] -> None
			|clause::resteClauses -> ( match clause with
				|t::[] -> aux (Some (t, t>0)) resteClauses
				|_ -> aux None resteClauses
				)
		)
		|_ -> acc
	in
	aux None fnc




(* verifie juste s'il y a des clauses tautologiques *)
let pretraitement (fnc:fnc) : fnc =
	tautology_elimination fnc

let post_traitement (valuation:affectations) (start_ind:int) : affectations =
	let rec aux v = match v with
	[] -> []
	|(t,b)::q -> if (abs t) >= start_ind then aux q else (t,b)::(aux q)
	in
	aux valuation

