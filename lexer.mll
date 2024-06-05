(* lexer.mll *)

{
  open Parser  (* Importation des tokens définis dans le fichier parser.mly *)
  exception Eof
  exception Error of string
}

let chiffre = ['0'-'9']
let min = ['a'-'z'] 
let maj = ['A'-'Z']
let lettre = (maj|min)
let nombre = chiffre+
let string2 = '\"'[^'\"']*'\"'
let string = [^'\"']*
let relop = "<" (">" | "=" | "") | ">" ("<" | "=" | "") | "="
let espace = [' ' '\t' '\n' '\r']

rule token = parse
  | espace+                           { token lexbuf }
  | '\n'                             { print_endline "FIN"; FIN }
  | maj                              { VAR (Lexing.lexeme lexbuf) }
  | nombre                           { NOMBRE (int_of_string (Lexing.lexeme lexbuf))}
  | '"' string as s '"'              { STRING s }
  | relop                            { let r = Lexing.lexeme lexbuf in
                                        match r with 
                                        | ">"  -> SUPER
                                        | "<"  -> INFER
                                        | "="  -> EGAL
                                        | ">=" -> SUPEREGAL 
                                        | "<=" -> INFEREGAL 
                                        | _ -> DIFF }
  | "IMPRIME"                        { IMPRIME }
  | "SI"                             { SI }
  | "ALORS"                          { ALORS }
  | "VAVERS"                         { VAVERS }
  | "ENTREE"                         { ENTREE }
  | "FIN"                            { FIN }
  | "REM"                            { REM }
  | "NL"                             { NL }
  | "+"                              { PLUS }
  | "-"                              { MOINS }
  | "*"                              { MULT }
  | "/"                              { DIVISE }
  | ","                              { VIRGULE }
  | "("                              { LPAREN }
  | ")"                              { RPAREN }
  | eof                              { EOF }
  | _                                { failwith "caractère non reconnu" }
