open Ast

(* Initialisation des variables (26 lettres de l'alphabet) *)
let init_vars = List.init 26 (fun _ -> 0)

(* Convertir un caractère majuscule en index (A -> 0, B -> 1, ...) *)
let int_of_caps (c : char) : int =
  Char.code c - Char.code 'A'

(* Mettre à jour la valeur d'une variable dans la liste *)
let set_var (c : char) (v : int) (vars : int list) : int list =
  let i = int_of_caps c in
  List.mapi (fun j x -> if i = j then v else x) vars

(* Interpréter un facteur *)
let rec interpret_factor vars = function
  | Variable v -> List.nth vars (int_of_caps (String.get v 0))
  | Nombre n -> n
  | Expression exp -> interpret_expression vars exp

(* Interpréter un terme *)
and interpret_term vars = function
  | Facteur f -> interpret_factor vars f
  | Mult (f1, f2) -> interpret_factor vars f1 * interpret_factor vars f2
  | Div (f1, f2) -> interpret_factor vars f1 / interpret_factor vars f2

(* Interpréter une expression *)
and interpret_expression vars = function
  | Term t -> interpret_term vars t
  | Plus (t1, t2) -> interpret_term vars t1 + interpret_term vars t2
  | Moins (t1, t2) -> interpret_term vars t1 - interpret_term vars t2

(* Comparer deux expressions avec un opérateur de comparaison *)
let compare_expression exp1 relop exp2 =
  match relop with
  | Egal -> exp1 = exp2
  | Super -> exp1 > exp2
  | SuperEgal -> exp1 >= exp2
  | Infer -> exp1 < exp2
  | InferEgal -> exp1 <= exp2
  | Diff -> exp1 <> exp2

(* Interpréter une liste d'expressions *)
let interpret_expr_list vars list =
  List.map (function
    | Str s -> String.sub s 1 (String.length s - 1)
    | Expr exp -> string_of_int (interpret_expression vars exp)
  ) list |> String.concat ""

(* Lire un entier avec une gestion d'erreur *)
let rec read_int_with_retry () =
  try Some (read_int ())
  with
  | End_of_file -> None
  | Failure _ ->
      print_endline "Entrée invalide, merci d'entrer un entier.";
      read_int_with_retry ()

(* Interpréter une instruction *)
let rec interpret_instruction vars nombre = function
  | Imprime exp ->
      print_string (interpret_expr_list vars exp);
      vars, nombre
  | Si (exp, r, exp', i) ->
      if compare_expression (interpret_expression vars exp) r (interpret_expression vars exp') then
        interpret_instruction vars nombre i
      else vars, nombre
  | Entree var_list ->
      let rec loop vars = function
        | [] -> vars
        | v :: reste ->
            (match read_int_with_retry () with
            | Some value -> loop (set_var (String.get v 0) value vars) reste
            | None -> loop vars reste)
      in
      loop vars var_list, nombre
  | Affectation (v, exp) ->
      set_var (String.get v 0) (interpret_expression vars exp) vars, nombre
  | MultiAffectation affs ->
    let rec apply_affs vars = function
    | [] -> vars
    | (v, exp) :: reste ->
      let new_val = interpret_expression vars exp in
      let new_vars = set_var (String.get v 0) new_val vars
      in apply_affs new_vars reste 
    in apply_affs vars affs, nombre
  | VaVers n -> vars, n
  | Fin -> vars, -1
  | Rem _ -> vars, nombre
  | NL -> print_newline (); vars, nombre

(* Trouver l'indice d'un élément dans une liste *)
let find_indice list x =
  let rec aux list x indice =
    if List.nth list indice = x then indice
    else aux list x (indice + 1)
  in
  aux list x 0

(* Fonction pour filtrer les instructions en gardant seulement la dernière occurrence de chaque numéro de ligne *)
let filtre_doublons instrs =
  let rec aux seen acc = function
    | [] -> acc
    | (nombre, instr) :: reste ->
      if List.exists (fun (n, _) -> n = nombre) seen then
        aux seen acc reste
      else
        aux ((nombre, instr) :: seen) ((nombre, instr) :: acc) reste
  in
  aux [] [] (List.rev instrs)

(* Interpréter un programme *)
let interpret (list_instr : (int * instr) list) =
  (* Vérification si la liste des instructions est vide *)
  if list_instr = [] then
    failwith "Le fichier de test est vide."
  else
    (* Filtrage des instructions pour garder seulement la dernière occurrence de chaque numéro de ligne *)
    let instr_filtre = filtre_doublons list_instr in
    (* Tri des instructions par ordre croissant *)
    let list_instr = List.sort (fun (i, _) (i', _) -> compare i i') instr_filtre in

    let rec aux vars programme nombre =
      let (current_nombre, instr) = List.find (fun (i, _) -> i = nombre) programme in
      let new_vars, new_nombre = interpret_instruction vars current_nombre instr in
      if new_nombre = -1 then ()

      else if new_nombre = current_nombre then
        let new_index = find_indice programme (current_nombre, instr) + 1 in
        if new_index >= List.length programme then ()
        else aux new_vars programme (fst (List.nth programme new_index))

      else
        aux new_vars programme new_nombre
    in
    aux init_vars list_instr (fst (List.hd list_instr))
