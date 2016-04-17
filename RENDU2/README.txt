- Rendu 2 solveur SAT -
Kerinec Emma & Guy Martin
23/03/16

Nous avons travaillé ce rendu 2 en quatre parties:
	1) Correction des bugs du rendu 1, à savoir
		- DPLL était fait en récursif. Il a été repris de zéro et codé en impératif.
		- Vérifier pourquoi certains fichiers de tests n'étaient pas correctement lus. Il s'est avéré que cela était dû au fait que
		  les fichiers de tests avaient été créés sous Windows (et le programme s'executait sous Linux). Les fichiers ont donc été
		  "refaits" sous Ubuntu.
		- Le programme compile désormais sans warning. :)
	2) Prendre le parser/lexer (lex&yacc) du TP1 pour l'adapter au projet, et l'appliquer uniquement dans le cas de tseitin.
	3) Heuristique MOMS
	4) Organisation

Tout a été fait ensemble.

2) Détails vis à vis de Tseitin
	Nous avons choisi pour Tseitin de garder les variables existantes, et de créer d'autres variables avec un numéro plus grand que toutes celles existantes. Ainsi, il devient facile de les supprimer à la fin. La fonction get_ind_max (dans tseitin.ml) renvoie ce numéro.
	Aussi, un prétraitement particulier est fait pour Tseitin. Il permet de transformer l'expression avec uniquement des Or() et des And(), pour une application plus facile de Tseitin.

3) Détails sur l'heuristique MOMS
	Le contenu de la fonction dpll_moms est quasiment un copié/collé de la fonction dpll de base. En effet, seul la façon de choisir le prochain pari change. Cela se passe dans la fonction mostFreqLitt, qui renvoie le litteral le plus fréquent de la plus petite clause.
	Note sur la complexité de cette fonction: il y a probablement mieux, nous envisageons de réfléchir à cela de façon plus approfondie par la suite.

4) Explication de l'organisation
	Nous avons mieux séparé en fichiers l'ensemble du code. L'ensemble des définitions des types se trouve dans 'types.ml', toutes les fonctions d'affichages sont uniquement dans 'affichage.ml'. Nous avons aussi regroupé diverses fonctions utiles dans 'utilitaire.ml'. Ces fichiers sont commentés et le type des paramètres apparait.
	Par rapport à DPLL, il y a un fichier dpll.ml qui s'occupe de lancer dpll avec l'heuristique choisie. Nous avons mis dans un fichier 'dpll_common.ml' toutes les fonctions communes à l'algorithme DPLL. Cela permet de faire un fichier relatif à chaque heuristique. Ainsi 'dpll_normal.ml' représente l'algorithme sans heuristique particuliere, et 'dpll_moms' avec l'heuristique MOMS.

Concernant l'algorithme DPLL, il fait les déductions clause unitaire et litteral pur. Un prétraitement, éliminant les tautologies, est aussi fait.

Liste des options possibles: '-tseitin' et '-moms'.
	Il est possible d'utiliser les deux quelque soit l'ordre entre elles.

Liste de ce qui est nouveau (non exhaustif):
	- DPLL en impératif
	- Heuristique MOMS
	- Utilisation de fichiers .for
	- Tseitin ainsi que lex&yacc
	- Meilleure organisation

Ce qui est prévu pour la suite:
	Outre ce qui a été dit plus haut, nous envisageons de faire un générateur de fichier .for et de .cnf pour tester sur des plus grands exemples. Aussi, nous ferons des scripts pour tester directement plusieurs fichiers, tout en comparant à minisat.


*** OLD ***
- Rendu 1 solveur SAT -
Kerinec Emma & Guy Martin
19/02/16

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
