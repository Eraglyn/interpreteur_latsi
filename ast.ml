(* ast.ml *)

type str = Str of string

type relop = Egal | Super | SuperEgal | Infer | InferEgal | Diff

type facteur = 
  | Variable of string
  | Nombre of int
  | Expression of expression

and term = 
  | Facteur of facteur
  | Mult of facteur * facteur
  | Div of facteur * facteur

and expression =
  | Term of term
  | Plus of term * term
  | Moins of term * term

and expr_str = 
  | Expr of expression
  | Str of string

type instr =
  | Imprime of expr_str list
  | Si of expression * relop * expression * instr
  | VaVers of int
  | Entree of string list
  | Affectation of string * expression
  | MultiAffectation of (string * expression) list
  | Fin
  | Rem of string
  | NL

type programme = (int * instr) list
