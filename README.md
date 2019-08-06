# uKanren_translator

Translator miniKanren (http://minikanren.org/) to Haskell


## Grammar for parsing miniKanren
```
Prog -> Def* Goal
Term -> Ident | '<' Ident : Term* '>'
Def -> :: Ident Ident* = Goal
Goal -> Disj | Fresh | Invoke
Fresh -> '[' Ident+ ':' Goal ']'
Invoke -> '{' Ident Term* '}'
Disj -> Conj ('\/' Conj)*
Conj -> Pat ('/\' Pat)*
Pat -> Term '===' Term | Fin
Fin -> Fresh | Ident | '(' Disj ')'
```
