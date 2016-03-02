open Expr

type var_prop = litteral*bool
type affectations = var_prop list



let print_list l =
	let string_of_list l = 
		let rec aux l = match l with 
			[] -> ""
			|t::q -> 
				let sep = ref "," in
				if q = [] then sep := "";
				(string_of_int t)^(!sep)^(aux q)
		in
		"["^(aux l)^"]"
	in
	print_string (string_of_list l)



let rec trouve_affectation (t:int) (a:affectations) : bool option =
	match a with
		[] -> None
		|(var, b)::q -> if var = t then Some b
						else trouve_affectation t q



(* renvoie vrai si AU MOINS UNE variable de la clause a une affectation *)
let rec clause_affectee (clause:clause) (a:affectations) : bool =
	(*print_string "APPEL DE CLAUSE AFFECTEE avec la clause ";
	print_list clause;
	print_newline();*)
	match clause with
		|[] -> false
		|var::q -> if Some (var > 0) = trouve_affectation var a then true
				else clause_affectee q a


(* renvoie vrai si toutes les clauses ont une affectation *)
let fnc_satisfaite (fnc:fnc) (a:affectations) : bool = 
	(*print_string "APPEL DE FNC SATISFAITE avec la FNC ";
	List.iter (print_list) fnc;
	print_newline(); *)
	let rec aux formule = match formule with
		|[] -> true
		|[[]] -> true
		|clause::q -> 
			(
				if clause = [] then true else
				(
					if (clause_affectee clause a) then (aux q)
					else false
				)
			)
	in
	aux fnc

(* Renvoie la liste des entiers representants les variables *)
let get_litt (fnc:fnc) : int list = 
	
	let rec appartient (x:int) (l:int list) = match l with
		[] -> false
		|t::q -> if x = t then true else appartient x q
	in
	
	let rec aux (lr:int list) (l:fnc) = match l with 
		[] -> lr
		|clause::q -> 
			let rec aux_clause lr2 l2 = match l2 with
			[] -> lr2
			|t::q -> if not (appartient t lr) && not (appartient t lr2) then aux_clause (t::lr2) q else aux_clause lr q
			in
			aux ((aux_clause [] clause)@lr) q
	in
	aux [] fnc



let trouve_litteral_pur (fnc:fnc) : var_prop option = 
	
	let match_var_prop (litt:var_prop) (liste_litt:var_prop list) : bool option =
		let rec aux acc l = match acc with
			None -> (
				match l with
				|[] -> None
				|t::q -> let (var1,b1) = litt in
						 let (var2,b2) = t in
					if var1 = var2 then aux (Some (b1=b2)) q
					else aux None q
			)
			|_ -> acc
		in
		aux None liste_litt	
	in
	
	
	
	let pure_lits = List.fold_left (fun pure_lits nxt_clause ->
                     (List.fold_left
			(fun lits nxt_lit ->
			  match (match_var_prop (nxt_lit,nxt_lit>0) pure_lits) with
			 	None -> (nxt_lit,nxt_lit>0)::lits
			  | Some false -> let sym = nxt_lit in List.filter (fun (sym', _) -> not (sym = sym')) lits
			  | _ -> lits)
			pure_lits nxt_clause)) [] fnc in
	
	match pure_lits with
	[] -> None
	| t::_ -> Some t
	

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

(* Nettoie la fnc des instances de 'var' et les clauses satisfaites *)
(*let nettoyage (fnc) (var:int) : fnc =
	
	List.fold_left
    (fun new_fnc clause ->
      let new_clause =
         (List.fold_left (fun new_clause lit ->
	   if (lit = var) then new_clause else lit::new_clause) [] clause) in
    match new_clause with [] -> new_fnc | _ -> new_clause::new_fnc) [] fnc
*)
let nettoyage (fnc:fnc) (var:int) : fnc = 
	
	(*print_string "APPEL DE NETTOYAGE avec la FNC : ";
	List.iter (print_list) fnc;
	print_newline();*)
	
	let rec auxLitt l lr = match l with
	[] -> lr
	|t::q -> if abs t = abs var then
		(
			if t = var then auxLitt [] []
			else auxLitt q lr
		)
		else
		(
			auxLitt q (t::lr)
		)
	in
	
	let rec auxClause l = match l with
	[] -> []
	|clause::q -> (auxLitt clause [])::(auxClause q)
	in
	
	let retour = auxClause fnc in
	
	(*print_string "VALEUR DE RETOUR : ";
	List.iter (print_list) retour;*)
	retour

let rec affiche_affect (a:(litteral*bool) list) : unit =  match a with
	[] -> ()
	|(u,booleen)::q -> 
		let b = if booleen = true then "vrai" else "faux" in
		print_string ("("^(string_of_int u)^","^b^")\n");
		affiche_affect q

let dpll (fnc:fnc) : bool*affectations =
	
	let rec dpll_sat (fnc:fnc) (list_var:int list) (a:affectations) : bool*affectations =
	
		(*print_string "\nNouvel appel de DPLL_SAT\n";
		print_list list_var;
		print_string "\n";
		affiche_affect a;*)
	
		if (fnc_satisfaite fnc a = true) then (true, a)
		else
		(
			let unitaire = trouve_clause_unitaire fnc in
			let pure = trouve_litteral_pur fnc in
			
			(* si il y a une clause unitaire *)
			if unitaire <> None then
			(
				let Some (t,b) = unitaire in 
				(*print_string "\nunitaire trouvé : ";
    				print_int t;*)
				
				let next_fnc = nettoyage fnc t in
				let asg1 = (t, b) in
				let next_list_var = List.filter (fun var' -> not (t = var')) list_var in
				dpll_sat next_fnc next_list_var (asg1::a)
			)
			else if pure <> None then
			(
				let Some (t,b) = pure in
				(*print_string "\npure trouvé : ";
   				print_int t;*)
				let next_fnc = nettoyage fnc t in
				let asg1 = (t, b) in
				let next_list_var = List.filter (fun var' -> not (t = var')) list_var in
				dpll_sat next_fnc next_list_var (asg1::a);
			)
			else
			(
				match list_var with
				[] -> (false,a)
				|t::q ->(
					let next_fnc = nettoyage fnc t in
    				let asg1 = (t, true) in
    				(*print_string "\non test ";
    				print_int t;
    				print_string " a true";*)
    				let (b1,a1) = dpll_sat next_fnc q (asg1::a) in
    				if b1 then (b1,a1)
    				else
    				(
    					let asg2 = (t, false) in
						(*print_string "\non test ";
						print_int t;
						print_string " a false";*)
    					dpll_sat next_fnc q (asg2::a)
    				);
				)
			)
			
		)
	in
	dpll_sat fnc (get_litt fnc) []
		
