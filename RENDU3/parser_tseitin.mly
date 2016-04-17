%{
(* --- préambule: ici du code Caml --- *)

open Types   

%}
/* description des lexèmes */

%token <int> VAR       /* le lexème VAR a un attribut entier */
%token ET OU OUEX IMPL EQUIV NON
%token LPAREN RPAREN
%token EOL             /* retour à la ligne */

%left IMPL
%left ET  /* associativité gauche: a/\b/\c, c'est (a/\b)/\c */
%left OU  /* associativité gauche: a\/b\/c, c'est (a\/b)\/c */
%left OUEX

%left EQUIV
%left NON

/* le plus a la fin est le plus prioritaire */

%start main             /* "start" signale le point d'entrée: c'est ici main */
%type <Types.expr> main     /* on _doit_ donner le type du point d'entrée */

%%
    /* --- début des règles de grammaire --- */
                            /* à droite, les valeurs associées */


main:                       /* le point d'entrée (cf. + haut, "start") */
    expr EOL                { $1 }  /* on veut reconnaître un "expr" */
;
expr:			    /* règles de grammaire pour les expressions */
  | VAR                     { Var $1 }
  | LPAREN expr RPAREN      { $2 } /* on récupère le deuxième élément */
  | expr ET expr          { And($1,$3) }
  | expr OU expr         { Or($1,$3) }
  | expr OUEX expr         { Xor($1,$3) }
  | expr IMPL expr         { Impl($1,$3) }
  | expr EQUIV expr         { Equ($1,$3) }
  | NON expr         	{ Not($2) }
;

