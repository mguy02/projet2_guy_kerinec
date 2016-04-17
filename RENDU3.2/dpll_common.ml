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
		|(clause,n)::q -> if (isTautology clause) then tautology_elimination q
			else (clause,n)::(tautology_elimination q)



let nettoyage (var:int) (fnc:fnc) : fnc = 
	
	
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
	|(clause,n)::q -> 
			((auxLitt clause [] true),n)::(auxClause q)
	in
	
	(* enleve les clauses vides *)
	let rec repassage l = match l with
		[] -> []
		|(clause,n)::q -> if clause = [] then repassage q
			else if clause = [0] then ([],n)::(repassage q)
			else (clause,n)::(repassage q)
	in
	
	let retour = repassage (auxClause fnc) in
	
	retour


let rec aUneClauseVide (fnc:fnc) : bool = match fnc with
	[] -> false
	|(clause,n)::q -> if clause = [] then true else
		aUneClauseVide q

(* retabli les valuations jusqu'a valu exclus *)
let rec retabli_valuation (litt:litteral) (valuation:affectations) : affectations = match valuation with
	[] -> []
	|(t,b,n,l_deduit)::q -> 
		if (litt = t) then [] else (t,b,n,l_deduit)::(retabli_valuation litt q)


let rec retabli_fnc (fnc:fnc) (l:affectations) : fnc = match l with
	[] -> fnc;
	|(t,b,n,l_deduit)::q -> (
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
		|(clause,n)::q -> auxClause clause; auxFNC q;
	in
	
	auxFNC fnc;

	!listeVar
	

let trouve_litteral_pur (fnc:fnc) : (litteral*bool) option = 

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
				auxLitt q
			end
	in
	
	let rec auxClause fnc = match fnc with
		[] -> ()
		|(clause,n)::q -> auxLitt clause;
			auxClause q
	in
	
	auxClause fnc;
	
	match !pur_potentiels with
	[] -> None
	|t::q -> Some (t, t>0)


let trouve_clause_unitaire (fnc:fnc) : ((litteral*bool) option)*int = 
	let rec aux acc clauses = match acc with
		(None,_) -> (
			match clauses with
			|[] -> (None,0)
			|(clause,n)::resteClauses -> ( match clause with
				|t::[] ->
					let rep = Some (t,t>0) in 
					aux (rep, n) resteClauses
				|_ -> aux (None,0) resteClauses
				)
		)
		|_ -> acc
	in
	aux (None,0) fnc


let getDeduction (litt:litteral) (numClause:int) (fnc:fnc) (valuation:affectations) : litteral list =
	
	let rec est_affecte aChercher valuation = match valuation with
	[] -> false
	|(t,b,n,l_deduit)::q -> if (aChercher = t) then true else est_affecte aChercher q
	in
	
	let rec parcours_clause clause lr = match clause with
	[] -> lr
	|t::q -> if (t = litt) then parcours_clause q lr(* on saute le litt *)
		else
		(
			if (est_affecte (abs(t)) valuation) then (parcours_clause q (abs(t)::lr))
			else ( parcours_clause q lr)	(* on ajoute pas *)
		)
	in
	
	let rec parcours_fnc numClause fnc = match fnc with
	[] -> []
	|(clause,n)::q ->
		if (n <> numClause) then parcours_fnc numClause q (*si c'est pas la clause qu'on cherche, on saute *)
		else 
			(
				parcours_clause clause []
			)
	in
	
	parcours_fnc numClause fnc
	
	
	
	



(* verifie juste s'il y a des clauses tautologiques *)
let pretraitement (fnc:fnc) : fnc =
	tautology_elimination fnc

let post_traitement (valuation:affectations) (start_ind:int) : affectations =
	let rec aux v = match v with
	[] -> []
	|(t,b,n,l_deduit)::q -> if (abs t) >= start_ind then aux q else (t,b,n,l_deduit)::(aux q)
	in
	aux valuation

