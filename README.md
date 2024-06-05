# INTERPRÉTEUR LATSI
Projet de Grammaire et Analyse Syntaxique — Interpréteur LATSI en Ocaml.

## DESCRIPTION
Interpréteur LATSI (LAngage Très Simple d'Instructions), un langage fictif sous forme [EBNF](https://fr.wikipedia.org/wiki/Extended_Backus-Naur_Form). Voir [projet.pdf](./projet.pdf) pour plus d'informations.

## POUR TESTER
### COMPILATION
Pour compiler le projet :
```bash
make
```

Cela va produire un dossier `_build` ainsi qu'un exécutable `latsi` au répertoire courant.

### EXÉCUTION
Pour exécuter `latsi`, il faut de lancer avec un fichier en entrée en suivant la syntaxe suivante :
```bash
./latsi source
```
`source` étant un simple fichier texte.
