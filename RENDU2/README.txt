- Rendu 1 solveur SAT -
Kerinec Emma & Guy Martin

Ce qui a été fait:
	* lecture du fichier de test et récupération de la formule FNC (dans parser.ml, a l'aide de la bibliotheque Str, permettant de facilement séparer suivant les espaces)
	* Implémentation de DPLL.


Nous avons choisi les structures de données suivantes :
 * un litteral est un int
 * une clause est une liste de litteraux
 * une FNC est une liste de clauses
 * une affectation des variables est une liste de couple (entier, booleen)

Tout a été codé ensemble.

Le solveur semble être correct pour un petit nombre de clauses (une vingtaine fonctionne) mais pour un plus grand nombre on obtient un stack overflow. Nous n'avons pas encore analysé exactement pourquoi.

Fonctions d'affichage:
	Nous avons fait une fonction "print_list" qui va afficher une liste de litteraux. Il suffit de faire print_list clause pour l'afficher.
	Si on veut afficher une FNC, il suffit de faire List.iter (print_list) fnc.
	
	Il y a également affiche_clause pour afficher une clause en notation infixe, de même pour les expressions. Ceci a été fait pour débugger durant la création du "parser".
