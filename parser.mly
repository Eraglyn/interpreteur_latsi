(* parser.mly *)

%{
  (* Importation du module Ast pour utiliser les types et les constructeurs *)
  open Ast  
%}

%token <int> NOMBRE
%token <string> STRING
%token <string> VAR

(* Mots-clés *)
%token IMPRIME SI ALORS VAVERS ENTREE FIN REM NL

(* Parenthèses et autres symboles *)
%token LPAREN RPAREN VIRGULE

(* Opérateurs arithmétiques *)
%token PLUS 
%token MOINS
%token MULT
%token DIVISE

(* Opérateurs de comparaison *)
%token EGAL
%token SUPER
%token SUPEREGAL
%token INFER
%token INFEREGAL
%token DIFF

(* Fin de fichier *)
%token EOF

(* Point d'entrée de la grammaire *)
%start <programme> programme
%%

(* La grammaire du programme commence ici *)

programme:
  (* Le programme est une séquence de lignes suivies de la fin de fichier *)
  | l=ligne* EOF { l }

ligne:
  (* Une ligne est un couple ligne * instruction *)
  | ln=NOMBRE i=instr { (ln, i) } 

instr:
  (* Impression *)
  | IMPRIME e=expr_list { Imprime(e) }
  (* Conditionnelles *)
  | SI exp1=expression EGAL exp2=expression ALORS i=instr { Si(exp1,Egal,exp2,i) }
  | SI exp1=expression SUPER exp2=expression ALORS i=instr { Si(exp1,Super,exp2,i) }
  | SI exp1=expression SUPEREGAL exp2=expression ALORS i=instr { Si(exp1,SuperEgal,exp2,i) }
  | SI exp1=expression INFER exp2=expression ALORS i=instr { Si(exp1,Infer,exp2,i) }
  | SI exp1=expression INFEREGAL exp2=expression ALORS i=instr { Si(exp1,InferEgal,exp2,i) }
  | SI exp1=expression DIFF exp2=expression ALORS i=instr { Si(exp1,Diff,exp2,i) }
  (* Saut *)
  | VAVERS ln=NOMBRE { VaVers(ln) }
  (* Lecture *)
  | ENTREE vl=var_list { Entree(vl) }
  (* Affectation *)
  | v=VAR EGAL exp=expression { Affectation(v, exp) }
  (* Multi Affectation *)
  | ma=multi_aff { MultiAffectation(ma) }
  (* Fin de programme *)
  | FIN { Fin }
  (* Commentaire *)
  | REM str=STRING { Rem(str) }
  (* Nouvelle Ligne *)
  | NL { NL }

(* Règles pour la mutli affectation *)
multi_aff:
  | v=VAR EGAL exp=expression { [(v, exp)] }
  | v=VAR EGAL exp=expression VIRGULE ma=multi_aff { (v, exp) :: ma }

(* Interprétation des expressions et des chaînes de caractères *)
expr_str:
  | exp=expression { Expr(exp) }
  | str=STRING { Str(str) }

(* Liste d'expressions *)
expr_list:
  | exp=expr_str { [exp] }
  | exp=expr_str VIRGULE tab=expr_list  { exp :: tab }

(* Liste de variables *)
var_list:
  | v=VAR { [v] }
  | v=VAR VIRGULE tab=var_list { v :: tab }

(* Expression arithmétique *)
expression:
  | t=term { Term(t) }
  | t1=term PLUS t2=term { Plus(t1, t2) }
  | t1=term MOINS t2=term { Moins(t1, t2) }

(* Terme arithmétique *)
term:
  | f=facteur { Facteur(f) }
  | f1=facteur MULT f2=facteur { Mult(f1, f2) }
  | f1=facteur DIVISE f2=facteur { Div(f1, f2) }

(* Facteur arithmétique *)
facteur:
  | v=VAR { Variable(v) }
  | nb=NOMBRE { Nombre(nb) }
  | LPAREN exp=expression RPAREN { Expression(exp) }
