open Parser
open Parser_tseitin
open Affichage
open Types
open Dpll
open Tseitin


let use_tseitin = ref false
let heuristique = ref NORMAL
let interac = ref false
let cl = ref false

let activateMOMS () : unit =
	heuristique := MOMS

;;

let activateClInterac () : unit =
	interac := true;
	cl := true

;;

let opt_tseitin =("-tseitin", Arg.Set use_tseitin, "Transformee de Tseitin") in
let opt_moms = ("-moms", Arg.Unit activateMOMS, "Heuristique MOMS") in
let opt_clinterac = ("-cl-interac", Arg.Unit activateClInterac, "Apprentissage de clause, mode interactif") in
let opt_cl = ("-cl", Arg.Set cl, "Apprentissage de clause") in

let options = [opt_tseitin; opt_moms;opt_clinterac;opt_cl] in
let usage_msg = "Options disponibles" in
Arg.parse options print_endline usage_msg
;;



let parse_tseitin fichier = Parser_tseitin.main Lexer_tseitin.token (Lexing.from_channel fichier)

(* la fonction que l'on lance ci-dessous *)
let calc (fichier) =
	if (!use_tseitin = false) then
	(
    	let result = parseFile (fichier) in	(* renvoie la fnc lue *)


		match result with
		[] -> print_string "SATISFIABLE 0\n"
		|[([],0)] -> print_string "UNSATISFIABLE\n"
		|_ -> dpll result !heuristique 0 false !cl !interac;
		
	)
	else if (!use_tseitin = true) then
	(
		let result = parse_tseitin fichier in	(* utilise le parser tseitin (.for) *)
		let expr = pretraitement_tseitin result in	(* prétraitement relatif aux parsing des .for *)
		
		let start_ind = (get_ind_max expr) + 1 in
		
		
		let fnc = tseitin expr start_ind in		(* on crée la fnc *)
		
		dpll fnc !heuristique start_ind true !cl !interac

	)

;;

let _ = 
	let fichier = ref stdin in
	let numberArg = Array.length (Sys.argv) in
	
	if (numberArg = 1) then
		(
			print_string "Pas d'argument en entree: nom du fichier manquant.";
			print_newline ();
			failwith "Erreur";
		)
	else
		(
			try
				fichier := open_in Sys.argv.(numberArg-1);
			with _ -> (failwith "Erreur dans l'ouverture du fichier. Verifiez l'ordre des arguments : ./resol -option1 -option2 fichier_test")
		);
		

	calc(!fichier);
	
	
	if(numberArg != 1) then
		close_in !fichier;
