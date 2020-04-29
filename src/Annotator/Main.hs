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

import           Annotator.Internal.InvokeNormalization
import           Annotator.Internal.Lib
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

addToStack :: Stack -> Name -> [Term (S, Ann)] -> [[G (S, Ann)]] -> Stack
addToStack stack name terms goal = trace ("addToStack: " ++ name ++ " | " ++ show terms ++ " | " ++ show (argsOrder terms goal)) $
  let updArgsOrderSet = S.insert (argsOrder terms goal) . fromMaybe S.empty $ M.lookup name stack
   in M.insert name updArgsOrderSet stack


filterStack :: Stack -> Stack
filterStack stack = fmap (errorIfUndefStack . S.filter argsOrderPred) stack
  where
    errorIfUndefStack :: S.Set ArgsOrder -> S.Set ArgsOrder
    errorIfUndefStack set
      | S.null set = error $ "UNDEFINED IN STACK\n" ++ show stack
      | otherwise  = set

----------------------------------------------------------------------------------------------------



argsOrderPred :: ArgsOrder -> Bool
argsOrderPred (ArgsOrder anns goal _) =
  all isJust anns &&
  (all (isJust . snd) . concatMap (concatMap getVars) $ goal)

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


normalizeUnif :: [[G a]] -> [[G a]]
normalizeUnif = fmap (concatMap go)
  where
    go :: G a -> [G a]
    go (C name1 term1 :=: C name2 term2)
      | name1 == name2 &&
        length term1 == length term2 = go `concatMap` zipWith (:=:) term1 term2
      | otherwise                    = error "normUnification: failed ctor unification"
    go goal                          = [goal]

initAnnotation :: (X -> Ts) -> [(X, Ann)] -> G S -> G (S, Ann)
initAnnotation xToTs xAnn = fmap (\s -> (s, fromMaybe Nothing $ lookup s sAnn))
  where
    sAnn = (\(x, ann) -> fmap (, ann) . getVarsT . xToTs $ x) `concatMap` xAnn

----------------------------------------------------------------------------------------------------

annotate :: E.Gamma -> [[G (S, Ann)]] -> ([[G (S, Ann)]], Stack)
annotate gamma goal = annotateInternal False "" gamma . (, M.empty) $ goal


fixPoint :: (Eq a) => (a -> a) -> a -> a
fixPoint handler = fixPoint'
  where
    fixPoint' input =
      let input1 = handler input 
          input2 = handler input1
       in if input1 == input2 then input1 else fixPoint' input2


annotateInternal :: Bool -> Name -> E.Gamma -> ([[G (S, Ann)]], Stack) -> ([[G (S, Ann)]], Stack)
annotateInternal isRecCall mainName gamma@(defByName, (_, xToTs), _) = annotateGoal
  where
    annotateGoal :: ([[G (S, Ann)]], Stack) -> ([[G (S, Ann)]], Stack)
    annotateGoal (disjList, stack) =
      foldr (\x (acc, st) -> (: acc) `first` annotateDisj (x, st)) ([], stack) disjList

    annotateDisj :: ([G (S, Ann)], Stack) -> ([G (S, Ann)], Stack)
    annotateDisj (conjList, stack) =
      if null res then (if isRecCall then head resDisjStackList else trace ("FAIL DISJ") $ head resDisjStackList) else head res
      where
        resDisjStackList = fmap (fixPoint annotateDisjInternal . (, stack)) . disjPerm $ conjList
        res              = dropWhile (disjStackPred mainName) resDisjStackList
        
        annotateDisjInternal :: ([G (S, Ann)], Stack) -> ([G (S, Ann)], Stack)
        annotateDisjInternal (conjList, stack) = 
          first meetGoals .
          foldl'
            (\(acc, st) x -> trace (mainName ++ ": CONJ " ++ (show x)) $ ((acc ++) . (:[])) `first` annotateConj (meetGoalForOne acc x, st))
            ([], stack) $ conjList
          
    annotateConj :: (G (S, Ann), Stack) -> (G (S, Ann), Stack)
    annotateConj (unif@(t1 :=: t2), stack) = (meetTerm t1 t2, stack)
      where
        meetTerm :: Term (S, Ann) -> Term (S, Ann) -> G (S, Ann)
        meetTerm (V (s, Nothing)) _       = V (s, fmap succ . maxAnn $ t2) :=: t2
        meetTerm (V (s, ann))     _       = t1 :=: replaceUndef (succ <$> ann) t2
        meetTerm (C _ _)          (C _ _) = error "annotateConj: two ctors unification"
        meetTerm _                _       = let (t2' :=: t1') = meetTerm t2 t1 in t1 :=: t2
    
    annotateConj invokeStack@(invoke@(Invoke name terms), stack)
      | isSkippable  = trace (unlines ["skippable"])  invokeStack
      | checkInStack = trace (unlines ["inStack"])    (Invoke name selfUpdTerms, stack)
      | otherwise    = trace (unlines ["notInStack"]) $ annotateInvoke initInvAnnotation
      where
        -- if all terms are undefined or annotated -- skip it
        isSkippable :: Bool
        isSkippable = trace ("INVOKE: " ++ show invokeStack) $ isNothing (maxAnnList terms) || isJust (maxAnn $ C undefined terms)

        -- in stack
        checkInStack :: Bool
        checkInStack = maybe False (S.member (argsOrder terms undefined)) $ M.lookup name stack

        selfUpdTerms :: [Term (S, Ann)]
        selfUpdTerms = replaceUndef (fmap succ . maxAnnList $ terms) <$> terms

        -- not in stack
        initInvAnnotation :: (E.Gamma, ([[G (S, Ann)]], Stack))
        initInvAnnotation = trace (mainName ++ ": unfoldName") $ 
          let
              (unfreshedGoal, updGamma) = LC.oneStepUnfold (fst <$> invoke) gamma
              normalizedGoal            = LC.normalize unfreshedGoal
              normUnifGoal              = normalizeUnif normalizedGoal
              preAnnotatedGoal          = updGoalAnnsByTerm terms normUnifGoal
              stackWithTheGoal          = addToStack stack name terms preAnnotatedGoal
           in (updGamma, (preAnnotatedGoal, stackWithTheGoal))

        annotateInvoke :: (E.Gamma, ([[G (S, Ann)]], Stack)) -> (G (S, Ann), Stack)
        annotateInvoke (updGamma, goalStack) = trace (mainName ++ ": annotatedUnfolded") $
          let 
              (annotatedGoal, updStack) = annotateInternal True name updGamma goalStack
              isInvDef                  = not $ disjStackPred name (concat annotatedGoal, updStack) 
              updTerms                  = if isInvDef then selfUpdTerms else terms
              stackTerms                = updTermAnnsByGoalForStack annotatedGoal terms
              updUpdStack               = addToStack updStack name stackTerms annotatedGoal
           in (Invoke name updTerms, updUpdStack)

    annotateConj _                  = error "forbidden goal for conj"

----------------------------------------------------------------------------------------------------

disjPerm :: [G a] -> [[G a]]
disjPerm = (\(unifs, invs) -> (unifs ++) <$> permutations invs) . partition isUnif
  where
    isUnif :: G a -> Bool
    isUnif (_ :=: _) = True
    isUnif _         = False


disjStackPred :: Name -> ([G (S, Ann)], Stack) -> Bool
disjStackPred name (list, stD) = not $ isNoUndef list && isAllInvDef
  where
    isNoUndef :: [G (S, Ann)] -> Bool
    isNoUndef = all isJust . concatMap (fmap snd . getVars)

    isAllInvDef :: Bool
    isAllInvDef  = all isDefInv . filter isNotRecInvoke $ list

    isNotRecInvoke :: G a -> Bool
    isNotRecInvoke (Invoke name' _)
      | name == name' = False
      | otherwise     = True
    isNotRecInvoke _               = False

    isDefInv :: G a -> Bool
    isDefInv (Invoke name _) = maybe False (any argsOrderPred) $ M.lookup name stD
    isDefInv _               = error "isDefInv is undefined for not invoke goal"

----------------------------------------------------------------------------------------------------

meet :: Ann -> Ann -> Ann
meet Nothing  x        = x
meet x        Nothing  = x
meet (Just x) (Just y) = Just $ max x y


meetGoals :: [G (S, Ann)] -> [G (S, Ann)]
meetGoals conjs = fmap (\(s, ann) -> (s, fromMaybe ann $ lookup s (commonVarAnns conjs))) <$> conjs


meetGoalForOne :: [G (S, Ann)] -> G (S, Ann) -> G (S, Ann)
meetGoalForOne acc = head . meetGoals . (: acc)

----------------------------------------------------------------------------------------------------

commonVarAnns :: [G (S, Ann)] -> [(S, Ann)]
commonVarAnns = fmap (bimap head (foldl' meet Nothing) . unzip)
              . groupBy (\(s1, _) (s2, _) -> s1 == s2)
              . sortBy (\(s1, _) (s2, _) -> compare s1 s2)
              . concatMap getVars

----------------------------------------------------------------------------------------------------

updTermAnnsByGoalForStack :: [[G (S, Ann)]] -> [Term (S, Ann)] -> [Term (S, Ann)]
updTermAnnsByGoalForStack goal terms
  | any (not . isVar) terms = error "updTermAnnsByGoal: invoke argument is ctor"
  | otherwise               = fmap go $ zip ((\(V (_, ann)) -> ann) <$> terms) updTerms
  where
    sToAnn   = M.fromList . commonVarAnns . concat $ goal
    updTerms = fmap (\(s, _) ->
             maybe (error "updTermAnnByGoal: args was not found") (s,) $ M.lookup s sToAnn) <$> terms

    go :: (Ann, Term (S, Ann)) -> Term (S, Ann)
    go (_, v@(V (_, Nothing))) = v
    go (Nothing, V (s, Just _)) = V (s, Just 1)
    go (Just _, V (s, _)) = V (s, Just 0)


updGoalAnnsByTerm :: [Term (S, Ann)] -> [[G S]] -> [[G (S, Ann)]]
updGoalAnnsByTerm terms goal
  | any (not . isVar) terms = error "updGoalAnnsByTerm: invoke argument is ctor"
  | otherwise               = fmap (fmap (\s -> (s,) . maybe Nothing id $ M.lookup s sToAnn)) <$> goal
  where
    sToAnn = M.fromList . fmap (\(V x) -> x) $ terms

----------------------------------------------------------------------------------------------------

getVars :: (Eq a) => G a -> [a]
getVars = nub . go
 where
  go (t1 :=: t2)      = getVarsT t1 ++ getVarsT t2
  go (g1 :/\: g2)     = go g1 ++ go g2
  go (g1 :\/: g2)     = go g1 ++ go g2
  go (Invoke _ terms) = getVarsT `concatMap` terms
  go (Fresh _ g)      = go g
  go (Let _ _)        = error "LET"

----------------------------------------------------------------------------------------------------

maxAnn :: Term (S, Ann) -> Ann
maxAnn (V (s, ann)) = ann
maxAnn (C _ terms)  = maybe Nothing (Just . foldr max 0) . sequence . fmap maxAnn $ terms


replaceUndef :: Ann -> Term (S, Ann) -> Term (S, Ann)
replaceUndef ann (V (s, Nothing))  = V (s, ann)
replaceUndef ann v@(V (_, oldAnn)) = v
replaceUndef ann (C name terms)    = C name . fmap (replaceUndef ann) $ terms


maxAnnList :: [Term (S, Ann)] -> Ann
maxAnnList terms =
  let annList = catMaybes . fmap maxAnn $ terms
   in if null annList then Nothing else Just . maximum $ annList

----------------------------------------------------------------------------------------------------

argsOrder :: [Term (S, Ann)] -> [[G (S, Ann)]] -> ArgsOrder
argsOrder terms goal = ArgsOrder (maxAnn <$> terms) goal (fmap fst . concatMap getVarsT $ terms)

annToMask :: [Ann] -> [[S]]
annToMask = fmap (fmap fst)
          . groupBy (\(_, ann1) (_, ann2) -> ann1 == ann2)
          . sortBy (\(_, ann1) (_, ann2) -> compare ann1 ann2)
          . zip [0..]

----------------------------------------------------------------------------------------------------

