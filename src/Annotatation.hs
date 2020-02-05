{-# LANGUAGE TupleSections #-}

module Annotate (
    annotateGoal
  ) where

import MKSyntax

import Data.List (nub)
import Data.Maybe (fromMaybe)


scope :: [Def]
scope = [
          ("reverso",["x","y"],V "x" :=: C "Nil" [] :/\: V "y" :=: C "Nil" [] :\/: Fresh "h" (Fresh "t" (Fresh "rt" (V "x" :=: C "Cons" [V "h",V "t"] :/\: (Invoke "reverso" [V "t",V "rt"] :/\: Invoke "appendo" [V "rt",C "Cons" [V "h",C "Nil" []],V "y"])))))
        , ("appendo",["x","y","xy"],V "x" :=: C "Nil" [] :/\: V "xy" :=: V "y" :\/: Fresh "h" (Fresh "t" (Fresh "ty" (V "x" :=: C "Cons" [V "h",V "t"] :/\: (V "xy" :=: C "Cons" [V "h",V "ty"] :/\: Invoke "appendo" [V "t",V "y",V "ty"])))))
        ]
{-
getGoalBySpec :: X -> Int -> G X
getGoalBySpec funcName argsNum =
    maybe (error "function not in the scope")
          (\(args, body) -> if length args == argsNum
                              then body
                              else error "error args num in function call")
  . lookup funcName
  . fmap (\(name, args, body) -> (name, (args, body))) $ scope

updInvokeArgs :: Name -> [Term (X, Ann)] -> [Term (X, Ann)]
updInvokeArgs name args = annotateGoal () name $ getGoalBySpec name (length args)
  where
    argsNames :: [X]
    argsNames = fmap fst . concatMap getVarsT $ args

    invokeGoalVars :: [(X, Ann)]
    
    inOutOfArgs :: ([X], [X])
    inOutOfArgs = flatInOutList $ getInOut <$> args

    flatInOutList :: [([a], [a])] -> ([a], [a])
    flatInOutList = foldr (\(xIn, xOut) (accIn, accOut) -> (xIn ++ accIn, xOut ++ accOut)) ([], []) 
-}
----------------------------------------------------------------------------------------------------

data Ann = In | Out | Undef
  deriving (Show, Eq)

annotateGoal :: [(X, Ann)] -> Name -> G X -> G (X, Ann)
annotateGoal argsSpec mainName = f . init
  where
    init :: (Functor f) => f X -> f (X, Ann)
    init = fmap (\x -> fromMaybe (x, Undef) $ lookup x argsSpec)
    
    f :: G (X, Ann) -> G (X, Ann)
    f (Let _ _)        = error "LET"
    f (g1 :/\: g2)     = meetGoal mainName (f g1) (f g2)
    f (g1 :\/: g2)     = f g1 :\/: f g2
    f (Fresh name g)   = Fresh name (f g)
    f inv@(Invoke name args)
      | mainName == name, length argsSpec == length args
          = Invoke name . updMainInvokeArgs $ zip (fmap snd argsSpec) args
      | mainName == name, length mainArgs /= length args
          = error "name collision in invoke: some functions with diff number of args"
      | otherwise
          = inv
      where
        updMainInvokeArgs :: [(Ann, Term (X, Ann))] -> [Term (X, Ann)]
        updMainInvokeArgs = fmap (uncurry fromMaybe) . zip args . fmap updMainInvokeArg

        updMainInvokeArg :: (Ann, Term (X, Ann)) -> Maybe (Term (X, Ann))
        updMainInvokeArg (mann, V (x, Undef)) = Just $ V (x, mann)
        updMainInvokeArg (mann, v@(V (_, ann)))
          | mann == ann = Just v
          | otherwise   = Nothing
        updMainInvokeArg (mann, C name terms) =
          fmap (C name) . sequence . fmap updMainInvokeArg . fmap (mann,) $ terms

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

updAnnots :: [(X, Ann)] -> G (X, Ann) -> G (X, Ann)
updAnnots annotList = fmap (\v@(x, _) -> maybe v (\updAnn -> (x, updAnn)) $ lookup x annotList)

meetGoal :: Name -> G (X, Ann) -> G (X, Ann) -> G (X, Ann)
meetGoal name g1 g2 = updAnnots annotList g1 :/\: updAnnots annotList g2
  where
    annotList = [(x1, meet ann1 ann2) | (x1, ann1) <- getVars g1,
                                        (x2, ann2) <- getVars g2, x1 == x2]

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
