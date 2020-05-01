module Annotator.Main (
    preTranslate
  ) where


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
      (Program scope goal)  = {- normalizeInvokes $ normalizeInvokes -} program
      gamma                 = E.updateDefsInGamma E.env0 scope
      (initGamma, initGoal) = initTranslation gamma goal inX
      (_, stack)            = annotate initGamma initGoal
   in {- makeStackBeauty . -} maybe (error "FAILED STACK") id . maybeStack $ stack

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
