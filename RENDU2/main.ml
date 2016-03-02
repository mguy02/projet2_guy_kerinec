open Parser
open Affichage
open Expr
open Dpll

let compile e =
  begin
    print_string "\n>> DPLL <<\n";

    let a,b = dpll e in
    
    if a then print_string "SATISFIABLE\n" else print_string "NON SATISFIABLE\n";
    

  end



let test fichier =
	parseFile fichier



(* la fonction que l'on lance ci-dessous *)
let calc (fichier) =

      let result = test(fichier) in
      (* Expr.affiche_expr result; print_newline (); flush stdout *)
	compile result;
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
			if (numberArg > 2) then
				(
					print_string "Plus d'un argument a ete detecte, le surplus a ete ignore.";
					print_newline();
				);
			try
				fichier := open_in Sys.argv.(1);
			with _ -> (failwith "Erreur dans l'ouverture du fichier")
		);
	
	calc(!fichier);
	if(numberArg != 1) then
		close_in !fichier;
