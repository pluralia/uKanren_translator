module Annotator.Internal.Lib where

import           Data.List (nub)
import           Syntax

----------------------------------------------------------------------------------------------------

getVarsT :: (Eq a) => Term a -> [a]
getVarsT = nub . go
  where
    go (V v)       = [v]
    go (C _ terms) = go `concatMap` terms

----------------------------------------------------------------------------------------------------

isVar :: Term a -> Bool
isVar (V _) = True
isVar _     = False

----------------------------------------------------------------------------------------------------

