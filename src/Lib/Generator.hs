module Lib.Generator where


class Generator a where
  gen :: [a]

instance (Generator a) => Generator [a] where
  gen = [] : do
    xs <- gen
    x <- gen
    return (x : xs)

instance Generator Int where
  gen = [0..6]

instance Generator Bool where
  gen = [False, True]
