{-# LANGUAGE TupleSections #-}
module Annotator.Internal.Gen (
    annotateStackWithGen
  ) where


import           Data.Bifunctor        (second)
import           Data.Foldable         (foldl')
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

annotateStackWithGen :: E.Gamma -> Stack -> [AnnDef]
annotateStackWithGen gamma stack =
  maybe (error "annotateStackWithGen: fail gen") makeStackBeauty . maybeStack $ go
  where
    go :: Stack
    go = foldl' annotateIt stack $ argsOrdersForGen $ stack

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
        mask :: Term (S, Ann) -> Term S
        mask (V _)          = V 0
        mask (C name terms) = C name . fmap mask $ terms

        genConj :: [S] -> G (S, Ann) -> ([S], [G (S, Ann)])
        genConj genList invoke@(Invoke _ _) = (genList, [invoke])
        genConj genList unif@(u1 :=: u2)    =
          let res1@(_, conjs1) = genConjT genList u1
              res2@(_, conjs2) = genConjT genList u2
           in (unif :) `second`
              case (conjs1, conjs2) of
                ((_ : _), (_ : _)) -> if mask u1 `isInst` mask u2 then res2 else res1
                ((_ : _), [])      -> res1
                ([],      (_ : _)) -> res2
                ([],      [])      -> (genList, [])

        genConjT :: [S] -> Term (S, Ann) -> ([S], [G (S, Ann)])
        genConjT genList (V (s, Nothing))
          | not $ s `elem` genList = (s : genList, [V (s, Just 1) :=: C "gen" []])
        genConjT genList (V _)       = (genList, [])
        genConjT genList (C _ terms) =
          foldl' (\(gl, res) t -> (++ res) `second` genConjT gl t) (genList, []) terms

----------------------------------------------------------------------------------------------------

