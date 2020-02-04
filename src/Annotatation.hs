module Annotate (
    annotateGoal
  ) where

import MKSyntax

import Data.List

data Ann = In | Out | Undef
  deriving (Show, Eq)

annotateGoal :: ([X], [X]) -> G X -> G (X, Ann)
annotateGoal varsInfo = f . initF varsInfo

initF :: ([X], [X]) -> G X -> G (X, Ann)
initF (inVars, outVars) = fmap go
  where
    go :: X -> (X, Ann)
    go x
      | x `elem` inVars  = (x, In)
      | x `elem` outVars = (x, Out)
      | otherwise        = (x, Undef)

f :: G (X, Ann) -> G (X, Ann)
f (g1 :/\: g2)     = f g1 `meetGoal` f g2
f (g1 :\/: g2)     = f g1 :\/: f g2
f (Fresh name g)   = Fresh name (f g)
f inv@(Invoke _ _) = inv
f (Let _ _)        = error "LET"
f unif@(t1 :=: t2) = termF t1 t2
  where
    termF :: Term (X, Ann) -> Term (X, Ann) -> G (X, Ann)
    termF (V (_, ann)) (C name terms)       = t1 :=: C name (fmap (fmap (meet ann)) <$> terms)
    termF (C name1 terms1) (C name2 terms2)
      | name1 == name2 && length terms1 == length terms2 =
        let (t1', t2') = unzip . fmap ((\(x :=: y) -> (x, y)) . uncurry termF) $ zip terms1 terms2 
         in C name1 t1' :=: C name2 t2'
      | otherwise = error "fail of ctors unification"
    termF (V (_, In))    (V (name, Undef)) = t1 :=: V (name, Out)
    termF (V (_, In))    _                 = unif
    termF (V (_, Out))   (V (_, Out))      = unif
    termF (V (_, Out))   (V (name, Undef)) = t1 :=: V (name, In)
    termF (V (_, Undef)) (V (_, Undef))    = unif
    termF _              _                 = (\(t1' :=: t2') -> t2' :=: t1') $ termF t2 t1

meet :: Ann -> Ann -> Ann
meet In _    = In
meet Out In  = In
meet Out _   = Out
meet Undef x = x

-----------------------------------------------------------------------------------------------------

meetGoal :: G (X, Ann) -> G (X, Ann) -> G (X, Ann)
meetGoal g1 g2 = updAnnots g1 :/\: updAnnots g2
  where
    annotList = [(x1, meet ann1 ann2) | (x1, ann1) <- getVars g1, (x2, ann2) <- getVars g2, x1 == x2]
    
    updAnnots :: G (X, Ann) -> G (X, Ann)
    updAnnots = fmap (\v@(x, _) -> maybe v (\updAnn -> (x, updAnn)) $ lookup x annotList)

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
