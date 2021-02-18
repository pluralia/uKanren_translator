[![Build Status](https://travis-ci.org/Pluralia/uKanren_translator.svg?branch=master)](https://travis-ci.org/Pluralia/uKanren_translator)

# uKanren_translator

Translator [miniKanren](http://minikanren.org/) to `Haskell`

-------

## A Quick Start

- Install `Haskell` build tool [Stack](https://docs.haskellstack.org/en/stable/README/#quick-start-guide)
- Execute 
```
cd uKanren_translator
stack build
stack exec -- ktrans
```

The program will pull `miniKanren` programs from the 'resources' directory, 
translate them in some directions and put the resulted `Haskell` programs in the 
'test' directory.

### Concept by example

You have a `miniKanren` program `appendo`.
It's a relation, which links 3 lists where the first 2 are concatenation of the
third one.
```
:: appendo x y xy =
  (x === [] /\ xy === y) \/
  ([h t ty:
     x === h % t /\ xy === h % ty /\ {appendo t y ty}])
```
"It's a relation" means you can execute it in different directions, fixing arguments.
For example, if you fix the first two arguments `x` and `y` and execute 
`appendo x y`, you get concatenation of 2 lists.
If you fix the last one and execute `appendo xy`, you get pairs of all lists,
concatenation of which ones gets the list `xy`.

To make translation in not relational language, you need to choose
a direction of translation.
As a result, translated `appendo x y` in `Haskell` looks like such way:
```java
appendoIIO x0 x1 = appendoIIO0 x0 x1 ++ appendoIIO1 x0 x1
appendoIIO0 s0@[] s1 = do
  let s2 = s1
  return $ (s2)
appendoIIO0 _ _ = []
appendoIIO1 s0@(s3 : s4) s1 = do
  (s5) <- appendoIIO s4 s1
  let s2 = (s3 : s5)
  return $ (s2)
appendoIIO1 _ _ = []
```
and `appendo xy`:
```java
appendoOOI x0 = appendoOOI0 x0 ++ appendoOOI1 x0
appendoOOI0 s2@s1 = do
  let s0 = []
  return $ (s0, s1)
appendoOOI0 _ = []
appendoOOI1 s2@(s3 : s5) = do
  (s4, s1) <- appendoOOI s5
  let s0 = (s3 : s4)
  return $ (s0, s1)
appendoOOI1 _ = []
```

### Parsing grammar

Use the next syntax to create a `miniKanren` program:

```
Prog   -> Def* Goal
Term   -> Ident | '<' Ident : Term* '>'
Def    -> :: Ident Ident* = Goal
Goal   -> Disj | Fresh | Invoke
Fresh  -> '[' Ident+ ':' Goal ']'
Invoke -> '{' Ident Term* '}'
Disj   -> Conj ('\/' Conj)*
Conj   -> Pat ('/\' Pat)*
Pat    -> Term '===' Term | Fin
Fin    -> Fresh | '(' Disj ')'
```

---

## [*Internal*] Check boxes
- [ ] Refactor the translator based on bta
- [ ] Add user interface
- [ ] Move test execution in `Spec`
- [ ] Update latex files
