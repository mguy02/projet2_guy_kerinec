type litteral = int
type clause = litteral list
type fnc = (clause*int) list	(* UPDATE : clause list devient clause*int list *)

type var_prop = litteral*bool*int*(int list)	(*UPDATE : litteral*bool devient litteral*bool*int*(int list) litt*valuation*niveau*(liste de qui litt est deduit) *)
type affectations = var_prop list



type heuristique = NORMAL | MOMS

type essai = PARI | DEDUCTION	(* permet de differentier quel type d'essai on fait durant un tour de boucle de DPLL *)


(* expressions quelconques, utilisées pour tseytin *)
type expr =
    Var of int
  | And of expr*expr
  | Or of expr*expr
  | Xor of expr*expr
  | Impl of expr*expr
  | Equ of expr*expr
  | Not of expr


