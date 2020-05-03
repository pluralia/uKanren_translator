{-# LANGUAGE TupleSections #-}
module Annotator.Internal.Normalization (
    normalizeUnif
  , normalizeInvokes
  ) where


import           Data.Bifunctor        (first, second, bimap)
import           Data.Foldable         (foldl')
import qualified Data.Map.Strict  as M
import           Data.List             (intercalate, nub)
import qualified Data.Set         as S

import qualified Eval             as E
import           Syntax

import           Annotator.Internal.Lib
import           Annotator.Internal.Types

import           Debug.Trace           (trace)

----------------------------------------------------------------------------------------------------

normalizeUnif :: [[G a]] -> [[G a]]
normalizeUnif = fmap (concatMap go)
  where
    go :: G a -> [G a]
    go (C name1 term1 :=: C name2 term2)
      | name1 == name2 &&
        length term1 == length term2 = go `concatMap` zipWith (:=:) term1 term2
      | otherwise                    = error "normalizeUnif: failed ctor unification"
    go goal                          = [goal]

----------------------------------------------------------------------------------------------------

normalizeInvokes :: Program -> Program
normalizeInvokes = fixPoint normalizeInvokesInternal 


normalizeInvokesInternal :: Program -> Program
normalizeInvokesInternal (Program scope goal) =
  let
      gamma0         = E.updateDefsInGamma E.env0 scope
      replaceInvokeG = replaceInvoke gamma0
      initInvInfo    = (M.fromList . fmap (\(Def name _ _) -> (name, 0)) $ scope, M.empty)
      (invInfo, (updGoal, goalDefs))  = replaceInvokeG initInvInfo goal
      (_, (replacedScope, scopeDefs)) = 
        foldr
          (\(Def n as goal) (sTN, (acc, defs)) ->
              second (bimap ((: acc) . (Def n as)) (++ defs)) $ replaceInvokeG sTN goal)
          (invInfo, ([], []))
          scope
      updScope = removeRepeatingDefs $ goalDefs ++ scopeDefs ++ replacedScope
   in Program updScope updGoal
  where
    removeRepeatingDefs :: [Def] -> [Def]
    removeRepeatingDefs = 
      let
          defListToMap = M.fromList . fmap (\(Def name args goal) -> ((name, args), goal)) 
          mapToDefList = fmap (\((name, args), goal) -> Def name args goal) . M.toList
       in mapToDefList . defListToMap


invokeSpec :: [Term X] -> [Term X]

invokeSpec terms = (\(C _ updTerms, _) -> updTerms) $ go (0, M.empty) (C "" terms)
  where
    go :: (Ord a, Show a) => (Int, M.Map a Int) -> Term a -> (Term X, (Int, M.Map a Int))
    go mapInfo@(n, varToName) (V var) =
      let intToVar     = V . show
          retIfNothing = (intToVar n, (succ n, M.insert var n varToName))
       in maybe retIfNothing ((, mapInfo) . intToVar) $ M.lookup var varToName
    go mapInfo (C name terms) =
      first (C name) .
      foldl' (\(acc, mI) x -> ((acc ++) . (:[])) `first` go mI x) ([], mapInfo) $ terms


makeSpec :: Name -> [Term X] -> Name
makeSpec name terms = "{" ++ name ++ " " ++ (intercalate " " (show <$> terms)) ++ "}"


makeUniqueVars :: [Term X] -> [X]
makeUniqueVars =
  foldl' (\acc x -> if x `elem` acc then acc else acc ++ [x]) [] .
  concatMap getVarsT


replaceInvoke :: E.Gamma ->
                 (M.Map Name Int, M.Map Name Name)->
                 G X ->
                 ((M.Map Name Int, M.Map Name Name), (G X, [Def]))
replaceInvoke gamma0 = go
  where
    go :: (M.Map Name Int, M.Map Name Name) -> G X ->
          ((M.Map Name Int, M.Map Name Name), (G X, [Def]))
    go invInfo unif@(_ :=: _)          = (invInfo, (unif, []))
    go invInfo (goal1 :/\: goal2)      =
      let (invInfo1, (updG1, defs1)) = go invInfo  goal1
          (invInfo2, (updG2, defs2)) = go invInfo1 goal2
       in (invInfo2, (updG1 :/\: updG2, defs1 ++ defs2))
    go invInfo (goal1 :\/: goal2)      =
      let (invInfo1, (updG1, defs1)) = go invInfo  goal1
          (invInfo2, (updG2, defs2)) = go invInfo1 goal2
       in (invInfo2, (updG1 :\/: updG2, defs1 ++ defs2))
    go invInfo (Fresh name goal) = second (first (Fresh name)) $ go invInfo goal
    go invInfo inv@(Invoke name terms)
     | all isVar terms && nub terms == terms = (invInfo, (inv, []))
     | otherwise                             =
      let
          spec        = makeSpec name (invokeSpec terms)
          (defs, updInvInfo@(_, specToName)) = unfoldInvoke gamma0 invInfo (name, invokeSpec terms)
          newName     = maybe (error "replaceInvoke: undef invoke") id $ M.lookup spec specToName
          invokeTerms = fmap V . makeUniqueVars $ terms
       in (updInvInfo, ({-Fresh "xxx" $ V "xxx" :=: V "xxx" :/\: -} Invoke newName invokeTerms, defs))


-- (source func name TO number of additional funcs, func spec TO name of additional func) ->
-- (func name, terms with replaced var names)
unfoldInvoke :: E.Gamma ->
                (M.Map Name Int, M.Map Name Name) ->
                (Name, [Term X]) ->
                ([Def], (M.Map Name Int, M.Map Name Name))
unfoldInvoke (defByName, _, _) invInfo@(fNameToNum, specToName) (name, terms) =
--  trace ("TERMS: " ++ (show terms)) $
  let
      num           = maybe (error "unfoldInvokes: undef invoke") id $ M.lookup name fNameToNum
      newName       = name ++ (show num) ++ "ext"

      argsNames     = makeUniqueVars terms

      (Def _ sourceArgs sourceGoal) = defByName name
      unfoldedGoal                  = renameGX (M.fromList $ zip sourceArgs terms) sourceGoal

      def           = Def newName argsNames unfoldedGoal

      updFNameToNum = M.insert name (succ num) fNameToNum
      spec          = makeSpec name terms
      updSpecToName = M.insert spec newName specToName
      
      retIfNothing  = ([def], (updFNameToNum, updSpecToName))
      retIfJust     = const ([], invInfo)
   in maybe retIfNothing retIfJust $ M.lookup spec specToName

renameGX :: M.Map X (Term X) -> G X -> G X
renameGX oldToNew = go
  where
    getUpd :: X -> Term X
    getUpd x = maybe (V x) id $ M.lookup x oldToNew

    renameTX :: Term X -> Term X
    renameTX (V x)          = getUpd x
    renameTX (C name terms) = C name (renameTX <$> terms)

    go :: G X -> G X
    go (term1 :=: term2)   = renameTX term1 :=: renameTX term2
    go (goal1 :/\: goal2)  = go goal1 :/\: go goal2
    go (goal1 :\/: goal2)  = go goal1 :\/: go goal2
    go (Fresh name goal)   = Fresh name $ go goal
    go (Invoke name terms) = Invoke name $ renameTX <$> terms

----------------------------------------------------------------------------------------------------

