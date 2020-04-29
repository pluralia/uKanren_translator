{-# LANGUAGE TupleSections #-}
module Annotator.Internal.Normalization (
    normalizeUnif
  , normalizeInvokes
  ) where


import           Data.Bifunctor        (first, second, bimap)
import qualified Data.Map.Strict  as M
import           Data.List             (intercalate)
import qualified Data.Set         as S

import qualified Eval             as E
import           Syntax

import           Annotator.Internal.Lib
import           Annotator.Types

import           Debug.Trace           (trace)

----------------------------------------------------------------------------------------------------

normalizeUnif :: [[G a]] -> [[G a]]
normalizeUnif = fmap (concatMap go)
  where
    go :: G a -> [G a]
    go (C name1 term1 :=: C name2 term2)
      | name1 == name2 &&
        length term1 == length term2 = go `concatMap` zipWith (:=:) term1 term2
      | otherwise                    = error "normUnification: failed ctor unification"
    go goal                          = [goal]

----------------------------------------------------------------------------------------------------

normalizeInvokes :: Program -> Program
normalizeInvokes (Program scope goal) =
  let
      gamma0          = E.updateDefsInGamma E.env0 scope
      replaceInvokePA = replaceInvoke gamma0 (M.fromList $ (\(Def name _ _) -> (name, 0)) <$> scope)
      (specToName, (updGoal, goalDefs)) = replaceInvokePA M.empty goal
      (x, (replacedScope, scopeDefs))   = 
        foldr
          (\(Def n as goal) (sTN, (acc, defs)) ->
              second (bimap ((: acc) . (Def n as)) (++ defs)) $ replaceInvokePA sTN goal)
          (specToName, ([], []))
          scope
      updScope = removeRepeatingDefs $ goalDefs ++ scopeDefs ++ replacedScope
   in Program updScope updGoal


removeRepeatingDefs :: [Def] -> [Def]
removeRepeatingDefs = mapToDefList . defListToMap
  where
    defListToMap = M.fromList . fmap (\(Def name args goal) -> ((name, args), goal)) 
    mapToDefList = fmap (\((name, args), goal) -> Def name args goal) . M.toList


invokeSpec :: (Ord a, Show a) => [Term a] -> [Term X]
invokeSpec terms = (\(C _ updTerms, _) -> updTerms) $ go (0, M.empty) (C "" terms)
  where
    go :: (Ord a, Show a) => (Int, M.Map a Int) -> Term a -> (Term X, (Int, M.Map a Int))
    go mapInfo@(n, varToName) (V var) =
      let intToVar     = V . show
          retIfNothing = (intToVar n, (succ n, M.insert var n varToName))
       in maybe retIfNothing ((, mapInfo) . intToVar) $ M.lookup var varToName
    go mapInfo (C name terms) =
      let res = foldr (\x (acc, mI) -> (: acc) `first` go mI x) ([], mapInfo) $ terms
       in C name `first` res


replaceInvoke :: E.Gamma -> M.Map Name Int -> M.Map Name Name -> G X -> (M.Map Name Name, (G X, [Def]))
replaceInvoke gamma0 fNameToNum = go
  where
    go :: M.Map Name Name -> G X -> (M.Map Name Name, (G X, [Def]))
    go specToName unif@(_ :=: _)          = (specToName, (unif, []))
    go specToName (goal1 :/\: goal2)      =
      let (specToName1, (updG1, defs1)) = go specToName  goal1
          (specToName2, (updG2, defs2)) = go specToName1 goal2
       in (specToName2, (updG1 :/\: updG2, defs1 ++ defs2))
    go specToName (goal1 :\/: goal2)      =
      let (specToName1, (updG1, defs1)) = go specToName  goal1
          (specToName2, (updG2, defs2)) = go specToName1 goal2
       in (specToName2, (updG1 :\/: updG2, defs1 ++ defs2))
    go specToName (Fresh name goal) = second (first (Fresh name)) $ go specToName goal
    go specToName inv@(Invoke name terms) 
      | all isVar terms = (specToName, (inv, []))
      | otherwise       =
      let (def@(Def newName _ _), (_, updSpecToName)) =
            unfoldInvoke gamma0 (fNameToNum, specToName) (name, invokeSpec terms)
       in (updSpecToName, (Invoke newName (V <$> getVarsT `concatMap` terms), [def]))
    go _          (Let _ _)               = error "extractInvokes: Let"


-- (source func name TO number of additional funcs, func spec TO name of additional func) ->
-- (func name, terms with replaced var names)
unfoldInvoke :: E.Gamma ->
                (M.Map Name Int, M.Map Name Name) ->
                (Name, [Term X]) ->
                (Def, (M.Map Name Int, M.Map Name Name))
unfoldInvoke (defByName, _, _) invInfo@(fNameToNum, specToName) (name, terms) =
  let
      num           = maybe (error "unfoldInvokes: undef invoke") id $ M.lookup name fNameToNum
      newName       = name ++ (show num)

      argsNames     = S.toList . S.fromList $ getVarsT `concatMap` terms

      (Def _ sourceArgs sourceGoal) = defByName name
      unfoldedGoal = renameGX (M.fromList $ zip sourceArgs terms) sourceGoal

      def           = Def newName argsNames unfoldedGoal

      updFNameToNum = M.insert name (succ num) fNameToNum
      spec          = "{" ++ name ++ " " ++ (intercalate " " (show <$> terms)) ++ "}"
      updSpecToName = M.insert spec newName specToName

      retIfNothing  = (def, (updFNameToNum, updSpecToName)) 
   in maybe retIfNothing (error "unfoldInvoke: don't need to unfold") $ M.lookup spec specToName


renameGX :: M.Map X (Term X) -> G X -> G X
renameGX oldToNew = go
  where
    getUpd :: X -> Term X
    getUpd x = maybe (V x) id $ M.lookup x oldToNew

    renameTX :: Term X -> Term X
    renameTX (V x) = getUpd x
    renameTX (C name terms) = C name (renameTX <$> terms)

    go :: G X -> G X
    go (term1 :=: term2)  = renameTX term1 :=: renameTX term2
    go (goal1 :/\: goal2) = go goal1 :/\: go goal2
    go (goal1 :\/: goal2) = go goal1 :\/: go goal2
    go (Fresh name goal) = Fresh name $ go goal
    go (Invoke name terms) = Invoke name $ renameTX <$> terms

----------------------------------------------------------------------------------------------------

