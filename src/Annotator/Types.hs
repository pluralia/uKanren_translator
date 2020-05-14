{-# LANGUAGE FlexibleInstances #-}
module Annotator.Types where

import           Syntax

----------------------------------------------------------------------------------------------------

data AnnDef = AnnDef Name [(S, Word)] [[Conj]]
  deriving (Show, Eq)


data Conj = U (Term (S, Word)) (Term (S, Word))
          | I Name [(S, Word)]
  deriving (Show, Eq)

type A = Word

----------------------------------------------------------------------------------------------------

