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
Fin -> Fresh | Ident | '(' Disj ')'
```

-------

# Relations classification

## Structure (existing of...)
- fresh-variables
- DNF
- calls of other functions

## Recursion
- no
- self-call
- f calls g, g calls f

## Input arguments
- only input: relation=predicate
- there are output: function

## Output arguments (determinism of the result)
- no output
- 1 output
- many output

