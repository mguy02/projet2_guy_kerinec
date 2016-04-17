%{
(* --- pr�ambule: ici du code Caml --- *)

open Types   

%}
/* description des lex�mes */

%token <int> VAR       /* le lex�me VAR a un attribut entier */
%token ET OU OUEX IMPL EQUIV NON
%token LPAREN RPAREN
%token EOL             /* retour � la ligne */

%left IMPL
%left ET  /* associativit� gauche: a/\b/\c, c'est (a/\b)/\c */
%left OU  /* associativit� gauche: a\/b\/c, c'est (a\/b)\/c */
%left OUEX

%left EQUIV
%left NON

/* le plus a la fin est le plus prioritaire */

%start main             /* "start" signale le point d'entr�e: c'est ici main */
%type <Types.expr> main     /* on _doit_ donner le type du point d'entr�e */

%%
    /* --- d�but des r�gles de grammaire --- */
                            /* � droite, les valeurs associ�es */


main:                       /* le point d'entr�e (cf. + haut, "start") */
    expr EOL                { $1 }  /* on veut reconna�tre un "expr" */
;
expr:			    /* r�gles de grammaire pour les expressions */
  | VAR                     { Var $1 }
  | LPAREN expr RPAREN      { $2 } /* on r�cup�re le deuxi�me �l�ment */
  | expr ET expr          { And($1,$3) }
  | expr OU expr         { Or($1,$3) }
  | expr OUEX expr         { Xor($1,$3) }
  | expr IMPL expr         { Impl($1,$3) }
  | expr EQUIV expr         { Equ($1,$3) }
  | NON expr         	{ Not($2) }
;

