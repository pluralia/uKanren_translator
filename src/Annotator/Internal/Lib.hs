module Annotator.Internal.Lib where

import           Data.List             (nub)
import           Data.Maybe            (fromMaybe)
import qualified Data.Map.Strict  as M
import qualified Data.Set         as S

import           Syntax

import           Annotator.Internal.Types
import           Annotator.Types

----------------------------------------------------------------------------------------------------

fixPoint :: (Eq a) => (a -> a) -> a -> a
fixPoint handler = fixPoint'
  where
    fixPoint' input =
      let input1 = handler input
          input2 = handler input1
       in if input1 == input2 then input1 else fixPoint' input2

----------------------------------------------------------------------------------------------------

makeStackBeauty :: Stack -> [AnnDef]
makeStackBeauty = concatMap (\(name, aoSet) -> fmap (toAnnDef name) . S.toList $ aoSet) . M.toList
  where
    goalToConj :: G (S, Word) -> Conj
    goalToConj (u1 :=: u2)         = U u1 u2
    goalToConj (Invoke name terms) = I name . fmap (\(V v) -> v) $ terms

    toAnnDef :: Name -> ArgsOrder -> AnnDef
    toAnnDef name (ArgsOrder anns goal vars) =
      let
          fromMb  = fromMaybe (error "makeStackBeauty: UNDEF ANNOTATION")
          args    = zip vars (fromMb <$> anns)
          resGoal = fmap (goalToConj . fmap (fmap fromMb)) <$> goal
       in AnnDef name args resGoal

----------------------------------------------------------------------------------------------------

isVar :: Term a -> Bool
isVar (V _) = True
isVar _     = False

getVarsT :: (Eq a) => Term a -> [a]
getVarsT (V v)       = [v]
getVarsT (C _ terms) = getVarsT `concatMap` terms

getVars :: (Eq a) => G a -> [a]
getVars (t1 :=: t2)      = getVarsT t1 ++ getVarsT t2
getVars (g1 :/\: g2)     = getVars g1 ++ getVars g2
getVars (g1 :\/: g2)     = getVars g1 ++ getVars g2
getVars (Invoke _ terms) = getVarsT `concatMap` terms
getVars (Fresh _ g)      = getVars g
getVars (Let _ _)        = error "LET"

----------------------------------------------------------------------------------------------------

maxAnn :: Term (S, Ann) -> Ann
maxAnn (V (s, ann)) = ann
maxAnn (C _ terms)  = maybe Nothing (Just . foldr max 0) . sequence . fmap maxAnn $ terms

----------------------------------------------------------------------------------------------------

argsOrder :: [Term (S, Ann)] -> [[G (S, Ann)]] -> ArgsOrder
argsOrder terms goal = ArgsOrder (maxAnn <$> terms) goal (fmap fst . concatMap getVarsT $ terms)

----------------------------------------------------------------------------------------------------

