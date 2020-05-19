{-# LANGUAGE TupleSections #-}
module Annotator.Internal.Gen (
    annotateStackWithGen
  ) where


import           Data.Bifunctor        (second)
import           Data.Foldable         (foldl')
import           Data.List             ((\\))
import qualified Data.Map.Strict  as M
import qualified Data.Set         as S

import           Embed                 (Instance (..))
import qualified Eval             as E
import           Syntax

import           Annotator.Internal.Core
import           Annotator.Internal.Lib
import           Annotator.Internal.Stack
import           Annotator.Internal.Types
import           Annotator.Types

import           Debug.Trace           (trace)

----------------------------------------------------------------------------------------------------

annotateStackWithGen :: E.Gamma -> Stack -> Stack
annotateStackWithGen gamma stack =
  go . fixPoint go $ stack
  where
    go :: Stack -> Stack
    go stack = foldl' annotateIt stack $ argsOrdersForGen $ stack

    argsOrdersForGen :: Stack -> [(Name, ArgsOrder)]
    argsOrdersForGen =
      filter (not . argsOrderPred . snd) . 
      concatMap (\(name, aoList) -> (name,) <$> aoList) . 
      fmap (second S.toList) . 
      M.toList
    
    annotateIt :: Stack -> (Name, ArgsOrder) -> Stack
    annotateIt stack (name, ArgsOrder anns goal args) =
      let
          inArgs                    = fmap snd . filter ((== (Just 0)) . fst) $ zip anns args
          genGoal                   = addGen inArgs goal
          (annotatedGoal, updStack) = annotateInternal name gamma (genGoal, stack)
          updUpdStack               = addToStack updStack name (V <$> zip args anns) annotatedGoal
       in trace ("GOAL: " ++ (show genGoal)) $
          trace ("STACK: " ++ (show updUpdStack)) $
          updUpdStack

    addGen :: [S] -> [[G (S, Ann)]] -> [[G (S, Ann)]]
    addGen inArgs = fmap (snd . foldl' (\(gl, res) g -> (++ res) `second` genConj gl g) (inArgs, []))
      where
        genConj :: [S] -> G (S, Ann) -> ([S], [G (S, Ann)])
        genConj genList invoke@(Invoke _ _) = (genList, [invoke])
        genConj genList unif@(u1 :=: u2)    =
          (unif :) `second`
            case (genVars u1, genVars u2) of
              (gv1@(_ : _), gv2@(_ : _)) ->
                case (gv1 \\ genList, gv2 \\ genList) of
                  (l1@(_ : _), l2@(_ : _)) -> conj $ if u1 `isInst` u2 then l2 else l1
                  (_,          _)          -> (genList, [])
              (gv1@(_ : _), [])          -> conj (gv1 \\ genList)
              ([],          gv2@(_ : _)) -> conj (gv2 \\ genList)
              ([],          [])          -> (genList, [])

        conj :: [S] -> ([S], [G (S, Ann)])
        conj vars = (vars, ((:=: C "gen" [])  . V . (, Just 1)) <$> vars)

        genVars :: Term (S, Ann) -> [S]
        genVars (V (s, Nothing)) = [s]
        genVars (V _)            = []
        genVars (C _ terms)      = genVars `concatMap` terms

----------------------------------------------------------------------------------------------------

