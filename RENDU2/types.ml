type litteral = int
type clause = litteral list
type fnc = clause list

type var_prop = litteral*bool
type affectations = var_prop list



type heuristique = NORMAL | MOMS

type essai = PARI | DEDUCTION


(* expressions quelconques, utilisées pour tseytin *)
type expr =
    Var of int
  | And of expr*expr
  | Or of expr*expr
  | Xor of expr*expr
  | Impl of expr*expr
  | Equ of expr*expr
  | Not of expr



