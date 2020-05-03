{-# LANGUAGE TupleSections #-}
module Annotator.Main (
    preTranslate
  ) where


import           Data.Bifunctor        (second)
import           Data.Foldable         (foldl')
import qualified Data.Map.Strict  as M
import           Data.Maybe            (fromMaybe, isJust, catMaybes, isNothing)
import qualified Data.Set         as S

import qualified Eval             as E
import           Syntax
import qualified Unfold           as U

import           Annotator.Internal.Core
import           Annotator.Internal.Lib
import           Annotator.Internal.Normalization
import           Annotator.Internal.Stack
import           Annotator.Internal.Types
import           Annotator.Types

import           Debug.Trace           (trace)


----------------------------------------------------------------------------------------------------

--preTranslate :: Program -> [X] -> [AnnDef]
preTranslate program inX =
  let
      (Program scope goal)  = {- normalizeInvokes -} program
      gamma                 = trace ("SCOPE: " ++ (show scope)) $ E.updateDefsInGamma E.env0 scope
      (initGamma, initGoal) = initTranslation gamma goal inX
      (_, stack)            = annotate initGamma initGoal
   in trace ("STACK: " ++ show stack) $
      maybe (annotateStackWithGen gamma stack) makeStackBeauty . maybeStack $ stack

----------------------------------------------------------------------------------------------------

annotateStackWithGen :: E.Gamma -> Stack -> [AnnDef]
annotateStackWithGen gamma stack =
  maybe (error "annotateStackWithGen: fail gen") makeStackBeauty . maybeStack $ go
  where
    go :: Stack
    go = foldl' annotateIt stack $ argsOrdersForGen stack

    argsOrdersForGen :: Stack -> [(Name, ArgsOrder)]
    argsOrdersForGen =
      filter (not . argsOrderPred . snd) . 
      concatMap (\(name, aoList) -> (name,) <$> aoList) . 
      fmap (second S.toList) . 
      M.toList
    
    annotateIt :: Stack -> (Name, ArgsOrder) -> Stack
    annotateIt stack (name, ArgsOrder anns goal args) =
      let
          genGoal                   = addGen args goal
          (annotatedGoal, updStack) = annotateInternal name gamma (genGoal, stack)
          updUpdStack               = addToStack updStack name (V <$> zip args anns) annotatedGoal
       in trace ("GOAL: " ++ (show genGoal)) $
          updUpdStack

    addGen :: [S] -> [[G (S, Ann)]] -> [[G (S, Ann)]]
    addGen args = fmap (snd . foldl' (\(gl, res) g -> (++ res) `second` genConj gl g) (args, []))
      where
        genConj :: [S] -> G (S, Ann) -> ([S], [G (S, Ann)])
        genConj genList invoke@(Invoke _ _) = (genList, [invoke])
        genConj genList unif@(u1 :=: u2)    = (unif :) `second` (genConjT genList (C "" [u1, u2]))

        genConjT :: [S] -> Term (S, Ann) -> ([S], [G (S, Ann)])
        genConjT genList (V (s, Nothing))
          | not $ s `elem` genList = (s : genList, [V (s, Just 1) :=: C "gen" []])
        genConjT genList (V _)                       = (genList, [])
        genConjT genList (C _ terms)                 =
          foldl' (\(gl, res) t -> (++ res) `second` genConjT gl t) (genList, []) terms

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

-- trace ("Iota before init: " ++ E.showInt iota)
initTranslation ::  E.Gamma -> G X -> [X] -> (E.Gamma, [[G (S, Ann)]])
initTranslation gamma goal inX =
  let
      (unfreshedGoal, gamma', _) = E.preEval gamma goal
      (_, iota'@(_, xToTs), _)   = gamma'
      inS                        = (getVarsT . xToTs) `concatMap` inX
      normalizedGoal             = U.normalize unfreshedGoal
      normUnifGoal               = normalizeUnif normalizedGoal
      preAnnotatedGoal           = fmap (initAnns inS) <$> normUnifGoal
   in (gamma', preAnnotatedGoal)
  where
    initAnns :: [S] -> G S -> G (S, Ann)
    initAnns inS = fmap (\s -> (s, if s `elem` inS then Just 0 else Nothing))

----------------------------------------------------------------------------------------------------
