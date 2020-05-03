{-# LANGUAGE TupleSections #-}
module Annotator.Internal.Gen (
    annotateStackWithGen
  ) where


import           Data.Bifunctor        (second)
import           Data.Foldable         (foldl')
import qualified Data.Map.Strict  as M
import qualified Data.Set         as S

import qualified Eval             as E
import           Syntax

import           Annotator.Internal.Core
import           Annotator.Internal.Lib
import           Annotator.Internal.Stack
import           Annotator.Internal.Types
import           Annotator.Types

import           Debug.Trace           (trace)

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

