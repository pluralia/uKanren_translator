{-# LANGUAGE TupleSections #-}
module Annotator.Main (
    preTranslate
  ) where


import           Data.Bifunctor        (first, second, bimap)
import           Data.Foldable         (foldl')
import qualified Data.Map.Strict  as M
import           Data.Maybe            (fromMaybe, isJust, catMaybes, isNothing)
import           Data.List             (groupBy, sortBy, nub, intercalate, permutations, intersect, partition)
import qualified Data.Set         as S
import           Data.Tuple            (swap)

import qualified CPD.LocalControl as LC
import qualified Eval             as E
import           Syntax

import           Annotator.Internal.Core
import           Annotator.Internal.Lib
import           Annotator.Internal.Normalization
import           Annotator.Internal.Stack
import           Annotator.Types

import           Debug.Trace           (trace)


----------------------------------------------------------------------------------------------------

--preTranslate :: Program -> [(X, PreAnn)] -> [AnnDef]
preTranslate program = trace ("Program: " ++ (show scope) ++ "\n\nSCOPE:" ++ (show scope) ++ "\n\n") $
  makeStackBeauty .
  filterStack . snd .
  uncurry annotate . initTranslation gamma goal
  where
    (Program scope goal) = normalizeInvokes $ normalizeInvokes program
    gamma                = E.updateDefsInGamma E.env0 scope

----------------------------------------------------------------------------------------------------

goalToConj :: G (S, Word) -> Conj
goalToConj (u1 :=: u2)         = U u1 u2
goalToConj (Invoke name terms) = I name . fmap (\(V v) -> v) $ terms


makeStackBeauty :: Stack -> [AnnDef]
makeStackBeauty = concatMap (\(name, aoSet) -> fmap (go name) . S.toList $ aoSet) . M.toList
  where
    go :: Name -> ArgsOrder -> AnnDef
    go name (ArgsOrder anns goal vars) =
      let
          fromMb  = fromMaybe (error "makeStackBeauty: UNDEF ANNOTATION")
          args    = zip vars (fromMb <$> anns)
          resGoal = fmap (goalToConj . fmap (fmap fromMb)) <$> goal
       in AnnDef name args resGoal

----------------------------------------------------------------------------------------------------

preAnnToAnn :: PreAnn -> Ann
preAnnToAnn In = Just 0

----------------------------------------------------------------------------------------------------

initTranslation ::  E.Gamma -> G X -> [(X, PreAnn)] -> (E.Gamma, [[G (S, Ann)]])
initTranslation gamma goal xPreAnn =
  let (_, iota, _) = gamma
   in {- trace ("Iota before init: " ++ E.showInt iota) $ -}
     let
         (unfreshedGoal, gamma', _) = E.preEval gamma goal
         (_, iota'@(_, xToTs), _)   = gamma'
         normalizedGoal             = LC.normalize unfreshedGoal
         normUnifGoal               = normalizeUnif normalizedGoal
         xAnn                       = second preAnnToAnn <$> xPreAnn
         preAnnotatedGoal           = fmap (initAnnotation xToTs xAnn) <$> normUnifGoal
      in {- trace ("Iota after init: " ++ E.showInt iota') -} (gamma', preAnnotatedGoal)

initAnnotation :: (X -> Ts) -> [(X, Ann)] -> G S -> G (S, Ann)
initAnnotation xToTs xAnn = fmap (\s -> (s, fromMaybe Nothing $ lookup s sAnn))
  where
    sAnn = (\(x, ann) -> fmap (, ann) . getVarsT . xToTs $ x) `concatMap` xAnn

----------------------------------------------------------------------------------------------------
