(* main.ml *)

let _ =
  (* Ouvrir le fichier spécifié en argument de la ligne de commande *)
  let fd = open_in Sys.argv.(1) in
  
  (* Créer un buffer de lexing à partir du canal de fichier *)
  let lexbuf = Lexing.from_channel fd in
  
  (* Analyser le programme en utilisant le parser et le lexer *)
  let programme = Parser.programme Lexer.token lexbuf in
  
  (* Fermer le fichier après l'analyse *)
  close_in fd;
  
  (* Interpréter le programme *)
  Interpreter.interpret programme
