open Types
open Affichage
open Utilitaire

open Dpll_normal
open Dpll_moms

let dpll (fnc:fnc) (h:heuristique) (start_ind:int) (use_tseitin:bool) =
	if h = NORMAL then
		dpll_normal fnc start_ind use_tseitin
	else if h = MOMS then
		dpll_moms fnc start_ind use_tseitin
	else
		failwith "Erreur dpll : heuristique n'existe pas"


