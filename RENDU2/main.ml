open Parser
open Parser_tseitin
open Affichage
open Types
open Dpll
open Tseitin


let parse_tseitin fichier = Parser_tseitin.main Lexer_tseitin.token (Lexing.from_channel fichier)

(* la fonction que l'on lance ci-dessous *)
let calc (fichier, heuristique, use_tseitin) =
	if (use_tseitin = false) then
	(
      let result = parseFile (fichier) in	(* renvoie la fnc lue *)
      (* Expr.affiche_expr result; print_newline (); flush stdout *)
		
		print_string "\n>> DPLL <<\n";
    
    	dpll result heuristique 0 false;
	)
	else if (use_tseitin = true) then
	(
		let result = parse_tseitin fichier in	(* utilise le parser tseitin (.for) *)
		let expr = pretraitement_tseitin result in	(* prétraitement relatif aux parsing des .for *)
		
		let start_ind = (get_ind_max expr) + 1 in
		
		print_int start_ind;
		print_newline();
		
		print_string "TSEITIN\n";
		
		print_string "expr : ";
		affiche_expr result;
		print_newline();
		print_string "expr pretraitée tseitin: ";
		affiche_expr expr;
		print_newline();
		print_string "tseitin : ";
		
		let fnc = tseitin expr start_ind in		(* on crée la fnc *)
		print_fnc fnc;
		print_newline();

		print_string "\n>> DPLL <<\n";
		
		dpll fnc heuristique start_ind true

	)
;;

let _ = 
	let fichier = ref stdin in
	let numberArg = Array.length (Sys.argv) in
	let heuristique = ref NORMAL in
	let tseitin = ref false in
	
	
	if (numberArg = 1) then
		(
			print_string "Pas d'argument en entree: nom du fichier manquant.";
			print_newline ();
			failwith "Erreur";
		)
	else
		(
			
			(* si il y a 3 arguments, on fait tseitin en ouvrant un .for *)
			if (numberArg = 3) then
				(
				
					if (Sys.argv.(2) = "-tseitin") then
					(
						tseitin := true;
					)
					else if (Sys.argv.(2) = "-moms") then
					(
						heuristique := MOMS
					)
					else
					(
						failwith "ERROR OPTION"
					);
					
				
				)
			else if (numberArg = 4) then
				(
					let opt1 = Sys.argv.(2) in
					let opt2 = Sys.argv.(3) in
					
					tseitin := (opt1="-tseitin" || opt2="-tseitin");
					
					if (opt1 = "-moms" || opt2 ="-moms") then
					(
						heuristique := MOMS
					);
					
					
				);
			
			try
				fichier := open_in Sys.argv.(1);
			with _ -> (failwith "Erreur dans l'ouverture du fichier")
		);
		

	calc(!fichier, !heuristique, !tseitin);
	
	
	if(numberArg != 1) then
		close_in !fichier;
