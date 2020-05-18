{-# LANGUAGE TupleSections #-}
module Annotator.Internal.Core (
    annotate
  , annotateInternal
  ) where


import           Data.Bifunctor        (first, bimap)
import           Data.Foldable         (foldl')
import qualified Data.Map.Strict  as M
import           Data.Maybe            (fromMaybe, isJust, catMaybes, isNothing, fromJust)
import           Data.List             (groupBy, sortOn, permutations, partition)
import qualified Data.Set         as S

import qualified Eval             as E
import           Syntax
import qualified Unfold           as U

import           Annotator.Internal.Normalization
import           Annotator.Internal.Lib
import           Annotator.Internal.Stack
import           Annotator.Internal.Types

import           Debug.Trace           (trace)

----------------------------------------------------------------------------------------------------

annotate :: E.Gamma -> [[G (S, Ann)]] -> ([[G (S, Ann)]], Stack)
annotate gamma = annotateInternal "" gamma . (, M.empty)

----------------------------------------------------------------------------------------------------

annotateInternal :: Name -> E.Gamma -> ([[G (S, Ann)]], Stack) -> ([[G (S, Ann)]], Stack)
annotateInternal mainName gamma@(defByName, (_, xToTs), _) = annotateGoal
  where
    annotateGoal :: ([[G (S, Ann)]], Stack) -> ([[G (S, Ann)]], Stack)
    annotateGoal (disjList, stack) =
      foldr (\x (acc, st) -> (: acc) `first` annotateDisj (x, st)) ([], stack) disjList

    annotateDisj :: ([G (S, Ann)], Stack) -> ([G (S, Ann)], Stack)
    annotateDisj (conjList, stack) = head $ if null res then resDisjStackList else res
      where
        resDisjStackList = fmap (fixPoint annotateDisjInternal . (, stack)) . disjPerm $ conjList
        res              = dropWhile (not . disjStackPred mainName) resDisjStackList
        
        annotateDisjInternal :: ([G (S, Ann)], Stack) -> ([G (S, Ann)], Stack)
        annotateDisjInternal (conjList, stack) = 
          first meetGoals .
          foldl'
            (\(acc, st) x -> (((acc ++) . (:[])) `first` annotateConj (acc `meetGoalForOne` x, st)))
            ([], stack) $ conjList
          
    annotateConj :: (G (S, Ann), Stack) -> (G (S, Ann), Stack)
    annotateConj (unif@(t1 :=: t2), stack) = (t1 `meetTerm` t2, stack)
      where
        meetTerm :: Term (S, Ann) -> Term (S, Ann) -> G (S, Ann)
        meetTerm (V (s, Nothing)) _       = V (s, fmap succ . maxAnn $ t2) :=: t2
        meetTerm (V (s, ann))     _       = t1 :=: replaceUndef (succ <$> ann) t2
        meetTerm (C _ _)          (C _ _) = error "annotateConj: two ctors unification is forbidden"
        meetTerm _                _       = trace "HERE: --------------- " $ let (t2' :=: t1') = t2 `meetTerm` t1 in t1 :=: t2
    
    annotateConj invokeStack@(invoke@(Invoke name terms), stack)
      | any (not . isVar) terms = error "updTermAnnsByGoal: invoke argument is ctor"
--      | isSkippable  = {- trace (unlines ["skippable"])  -} invokeStack
      | checkInStack = {- trace (unlines ["inStack"])    -} (Invoke name selfUpdTerms, stack)
      | otherwise    = {- trace (unlines ["notInStack"]) -} annotateInvoke
      where
{-
        -- if all terms are undefined or annotated -- skip it
        isSkippable :: Bool
        isSkippable = {- trace ("INVOKE: " ++ show invokeStack) $ -}
          (all (isNothing . maxAnn) $ terms) || (all (isJust . maxAnn) terms)
-}
        -- in stack
        checkInStack :: Bool
        checkInStack =
          let
              resetTerms                = fmap (fmap (fmap (const 0))) <$> terms
              stackTerms                = replaceUndef (Just 1) <$> resetTerms
           in maybe False (S.member (argsOrder stackTerms [])) $ M.lookup name stack

        selfUpdTerms :: [Term (S, Ann)]
        selfUpdTerms =
          let
              annList = catMaybes . fmap maxAnn $ terms
              ann = if null annList then Nothing else Just . succ . maximum $ annList
           in replaceUndef ann <$> terms

        -- not in stack
        annotateInvoke :: (G (S, Ann), Stack)
        annotateInvoke =
          let
              (unfreshedGoal, updGamma) = U.oneStepUnfold (fst <$> invoke) gamma

              normalizedGoal            = U.normalize unfreshedGoal
              normUnifGoal              = normalizeUnif normalizedGoal
              preAnnotatedGoal          = initGoalAnns terms normUnifGoal

              resetTerms                = fmap (fmap (fmap (const 0))) <$> terms
              stackTerms                = replaceUndef (Just 1) <$> resetTerms

              stackWithTheGoal          = addToStack stack name stackTerms preAnnotatedGoal
              goalStack                 = (preAnnotatedGoal, stackWithTheGoal)
              
              (annotatedGoal, updStack) = annotateInternal name updGamma goalStack
--              isInvDef                  = disjStackPred name (concat annotatedGoal, updStack) 

              resTerms                  = selfUpdTerms -- if isInvDef then selfUpdTerms else terms


              updUpdStack               = addToStack updStack name stackTerms annotatedGoal
              resStack                  = updUpdStack -- if isInvDef then updUpdStack else updStack
           in (Invoke name resTerms, resStack)

    annotateConj _                  = error "annotateConj: forbidden goal for conj"

----------------------------------------------------------------------------------------------------

initGoalAnns :: [Term (S, Ann)] -> [[G S]] -> [[G (S, Ann)]]
initGoalAnns terms goal =
  let inVars = fmap fst . filter (isJust . snd) . concatMap getVarsT $ terms
   in fmap (fmap (\s -> (s, if s `elem` inVars then Just 0 else Nothing))) <$> goal

----------------------------------------------------------------------------------------------------

disjPerm :: [G a] -> [[G a]]
disjPerm = (\(unifs, invs) -> (unifs ++) <$> permutations invs) . partition isUnif
  where
    isUnif :: G a -> Bool
    isUnif (_ :=: _) = True
    isUnif _         = False

----------------------------------------------------------------------------------------------------

replaceUndef :: Ann -> Term (S, Ann) -> Term (S, Ann)
replaceUndef ann (V (s, Nothing))  = V (s, ann)
replaceUndef ann v@(V (_, oldAnn)) = v
replaceUndef ann (C name terms)    = C name . fmap (replaceUndef ann) $ terms

----------------------------------------------------------------------------------------------------

meet :: Ann -> Ann -> Ann
meet Nothing  x        = x
meet x        Nothing  = x
meet (Just x) (Just y) = Just $ max x y


commonVarAnns :: [G (S, Ann)] -> M.Map S Ann
commonVarAnns = M.fromList
              . fmap (bimap head (foldl' meet Nothing) . unzip)
              . groupBy (\(s1, _) (s2, _) -> s1 == s2)
              . sortOn fst
              . concatMap getVars


meetGoals :: [G (S, Ann)] -> [G (S, Ann)]
meetGoals conjs =
  let sToAnn = commonVarAnns conjs
   in fmap (\(s, ann) -> (s, fromMaybe ann $ M.lookup s sToAnn)) <$> conjs


meetGoalForOne :: [G (S, Ann)] -> G (S, Ann) -> G (S, Ann)
meetGoalForOne acc = head . meetGoals . (: acc)

----------------------------------------------------------------------------------------------------


