- Rendu 3 solveur SAT -
Kerinec Emma & Guy Martin
14/04/16

Nous avions pour ce rendu à ne traiter que la section 2.1 et 2.2.

Nous avons encore une fois séparé en plusieurs parties, et séparé le travail:
	1) Prise en compte des remarques vis à vis du rendu 2, à savoir
		- Gérer le format de sortie correctement (par exemple UNSATIFIABLE au lieu de NON SATISFIABLE)
		- Gérer les cas particuliers dans le parsing (clause vide et formule vide)
		- Le parser fait "a la main" (pour les fichiers .cnf) est plus maniable: il peut y avoir autant d'espace que voulu, et quelques bugs anecdotiques ont été corrigés.
	
	Tout ceci a été traité par Emma.

	2) Gestion des options
		Nous avons utilisé le module Arg pour gérer les options. La liste des options disponibles s'affiche donc dans le fichier main.ml. Nous avons rajouté l'option -cl-interac (ainsi que -cl).
	
	Ceci a été fait par Martin.	
	
	3) Graphe des conflits
		Afin de construire le graphe de conflit, nous avons besoin des numéros de clause (pour retrouver la clause initial). Or, utilisant des listes, cela se perd. Ainsi nous avons donc rajouté un numéro associé à chaque clause.
		
		a) Changement de types 
			Comme dit ci-dessus, on a désormais le type fnc qui devient un (clause*int) list.
			Nous avons aussi changé le type de var_prop qui était jusqu'à présent un litteral*bool (litteral et sa valuation). Désormais, une var_prop est de type litteral*bool*int*(int list) (litteral muni de sa valuation, le niveau auquel il a été instancié, et la liste de ses parents)
			Cela permet une plus grande souplesse lors de la création du graphe. En effet, avant ce changement, nous étions obligé de refaire de nombreuses fois des parcours de listes.
		
		Cela a été fait ensemble.
		
		b) Idée pour construire le graphe
			Nous avons besoin de l'ancienne FNC (celle juste avant le conflit). Ainsi nous retenons à chaque étape de DPLL l'ancienne FNC.
			La construction se fait en deux étapes: premierement, on parcours l'ancienne fnc pour trouver le conflit. Ensuite, on va créer une liaison entre leurs parents (litteraux qui les impliquent) et eux. 
	
Divers:
	- Nouvelles fonctions utilitaire (dans utilitaire.ml):
		* 'ecrire (fichier:out_channel) (s:string) : unit' pour ecrire facilement dans un fichier.
		* 'litt_val valu'
		  'bool_val valu' 
	  	  'niv_val valu'
		  'ldeduit_val valu'
		  Ces fonctions renvoient respectivement le litteral, le booleen, le niveau, et la liste des parents d'une valuation. Cela évite de à chaque fois définir d'autres variables inutilement.
		* 'troncate l i'
		Cette fonction tronque la liste l à ses i premiers elements. Renvoie la liste l si i > taille de la liste.
		Cette fonction est utilisée pour l'apprentissage de clauses.



*** OLD ***
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
	/!\ Conformément au rendu 1, les options se font avant le fichier de test à charger.
		Exemple: ./resol -moms -tseitin tests/test1.cnf

Liste de ce qui est nouveau (non exhaustif):
	- DPLL en impératif
	- Heuristique MOMS
	- Utilisation de fichiers .for
	- Tseitin ainsi que lex&yacc
	- Meilleure organisation
	- Il est désormais possible de mettre un espace au début d'une clause d'un fichier .cnf

Ce qui est prévu pour la suite:
	Outre ce qui a été dit plus haut, nous envisageons de faire un générateur de fichier .for et de .cnf pour tester sur des plus grands exemples. Aussi, nous ferons des scripts pour tester directement plusieurs fichiers, tout en comparant à minisat.



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
