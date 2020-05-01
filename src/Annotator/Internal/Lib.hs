module Annotator.Internal.Lib where

import           Data.List (nub)
import           Syntax

import           Annotator.Internal.Types

----------------------------------------------------------------------------------------------------

fixPoint :: (Eq a) => (a -> a) -> a -> a
fixPoint handler = fixPoint'
  where
    fixPoint' input =
      let input1 = handler input
          input2 = handler input1
       in if input1 == input2 then input1 else fixPoint' input2

----------------------------------------------------------------------------------------------------

getVarsT :: (Eq a) => Term a -> [a]
getVarsT = nub . go
  where
    go (V v)       = [v]
    go (C _ terms) = go `concatMap` terms

getVars :: (Eq a) => G a -> [a]
getVars = nub . go
 where
  go (t1 :=: t2)      = getVarsT t1 ++ getVarsT t2
  go (g1 :/\: g2)     = go g1 ++ go g2
  go (g1 :\/: g2)     = go g1 ++ go g2
  go (Invoke _ terms) = getVarsT `concatMap` terms
  go (Fresh _ g)      = go g
  go (Let _ _)        = error "LET"

----------------------------------------------------------------------------------------------------

isVar :: Term a -> Bool
isVar (V _) = True
isVar _     = False

----------------------------------------------------------------------------------------------------

argsOrder :: [Term (S, Ann)] -> [[G (S, Ann)]] -> ArgsOrder
argsOrder terms goal = ArgsOrder (maxAnn <$> terms) goal (fmap fst . concatMap getVarsT $ terms)

maxAnn :: Term (S, Ann) -> Ann
maxAnn (V (s, ann)) = ann
maxAnn (C _ terms)  = maybe Nothing (Just . foldr max 0) . sequence . fmap maxAnn $ terms

----------------------------------------------------------------------------------------------------

