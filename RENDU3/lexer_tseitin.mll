{
open Parser_tseitin;;        (* le type "token" est défini dans parser.mli *)
exception Eof;;
}

rule token = parse    (* la "fonction" aussi s'appelle token .. *)
  | [' ' '\t']     { token lexbuf }    (* on saute les blancs *)
 	     	   	           (* token: appel récursif *)
                                   (* lexbuf: argument implicite
                                      associé au tampon où sont
                                      lus les caractères *)
  | '0'				{EOL}
  | '\n'            { ET }
  | "/\\"             { ET }
  | "\\/"             { OU }
  | 'X'				{ OUEX }
  | "=>"			{ IMPL }
  | "<=>"			{ EQUIV }
  | '-'				{ NON}
  | '~'				{ NON}
  | '('             { LPAREN }
  | ')'             { RPAREN }
  | ['1'-'9']['0'-'9']* as s { VAR (int_of_string s) }
  | eof             { raise Eof } 
