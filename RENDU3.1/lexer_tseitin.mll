{
open Parser_tseitin;;        (* le type "token" est d�fini dans parser.mli *)
exception Eof;;
}

rule token = parse    (* la "fonction" aussi s'appelle token .. *)
  | [' ' '\t']     { token lexbuf }    (* on saute les blancs *)
 	     	   	           (* token: appel r�cursif *)
                                   (* lexbuf: argument implicite
                                      associ� au tampon o� sont
                                      lus les caract�res *)
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
