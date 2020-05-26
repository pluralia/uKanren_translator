[![Build Status](https://travis-ci.org/Pluralia/uKanren_translator.svg?branch=master)](https://travis-ci.org/Pluralia/uKanren_translator)

# uKanren_translator

Translator miniKanren (http://minikanren.org/) to Haskell

-------

# Parsing grammar
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
Fin -> Fresh | '(' Disj ')'
```

