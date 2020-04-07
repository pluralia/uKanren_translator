{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE FlexibleInstances #-}
module Annotation (
    translate
  , PreAnn (..)
  ) where


import           Data.Bifunctor        (first, second, bimap)
import           Data.Foldable         (foldl')
import qualified Data.Map.Strict  as M
import           Data.Maybe            (fromMaybe, isJust, catMaybes, isNothing)
import           Data.List             (groupBy, sortBy, nub, intercalate, permutations, intersect, partition)
import qualified Data.Set         as S
import           Data.Tuple            (swap)
import           Text.Printf

import qualified CPD.LocalControl as LC
import qualified Eval             as E
import           Syntax

import           Debug.Trace           (trace)


----------------------------------------------------------------------------------------------------

translate :: Program -> [(X, PreAnn)] -> ([[G (S, Ann)]], Stack)
translate (Program scope goal) = 
  second filterStack . 
  uncurry annotate . initTranslation gamma goal
  where
    gamma@(_, iota, _) = E.updateDefsInGamma E.env0 scope

----------------------------------------------------------------------------------------------------

data PreAnn = In
  deriving (Show, Eq)

preAnnToAnn :: PreAnn -> Ann
preAnnToAnn In = Just 0


-- Binding-time annotation:
-- Nothing ~ Undefined
-- Just x ~ x
type Ann = Maybe Word

----------------------------------------------------------------------------------------------------

type Stack = M.Map Name (S.Set ArgsOrder)


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

data ArgsOrder = ArgsOrder [Ann] [[G (S, Ann)]]


instance Eq ArgsOrder where
  (ArgsOrder anns1 _) == (ArgsOrder anns2 _) =
    (checkArgs anns1 anns2 || checkArgs anns2 anns1) && checkAnns (annToMask anns1) (annToMask anns2)
    where
      checkArgs :: [Ann] -> [Ann] -> Bool
      checkArgs ax ay = all (\(x, y) -> x <= y) $ zip ax ay

      checkAnns :: [[S]] -> [[S]] -> Bool
      checkAnns ax ay = not . null $ go ax `intersect` go ay
        where
          go :: [[S]] -> [[S]]
          go []       = []
          go [x]      = permutations x
          go (x : xs) = do
            h <- permutations x
            t <- go xs
            return $ h ++ t

instance Ord ArgsOrder where
  (ArgsOrder anns1 _ ) <= (ArgsOrder anns2 _) = annToMask anns1 <= annToMask anns2


argsOrderPred :: ArgsOrder -> Bool
argsOrderPred (ArgsOrder anns goal) =
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
         xAnn                       = second preAnnToAnn <$> xPreAnn
         preAnnotatedGoal           = fmap (initAnnotation xToTs xAnn) <$> normalizedGoal
      in {- trace ("Iota after init: " ++ E.showInt iota') -} (gamma', preAnnotatedGoal)


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
      | otherwise    = trace (unlines ["notInStack"]) annotateUnfolded $ unfoldName
      where
        -- if all terms are undefined or annotated -- skip it
        isSkippable :: Bool
        isSkippable = trace ("INVOKE: " ++ show invokeStack) $ isNothing (maxAnnList terms) || isJust (maxAnn $ C undefined terms)

        -- in stack
        checkInStack :: Bool
        checkInStack = maybe False (S.member (argsOrder terms undefined)) $ M.lookup name stack

        selfUpdTerms :: [Term (S, Ann)]
        selfUpdTerms = updTermsByAnn (iterate id . fmap succ . maxAnnList $ terms) terms

        -- not in stack
        unfoldName :: ([S], E.Gamma, ([[G (S, Ann)]], Stack))
        unfoldName = trace (mainName ++ ": unfoldName") $ 
          let (Def _ args _)          = defByName name
              (unfreshedGoal, gamma') = LC.oneStepUnfold (fst <$> invoke) gamma
              (_, (_, xToTs), _)      = gamma'
              normalizedGoal          = LC.normalize unfreshedGoal
              xAnn                    = zip args (resetInAnn <$> terms)
              preAnnotatedGoal        = fmap (initAnnotation xToTs xAnn) <$> normalizedGoal
              interestS               = (getVarsT . xToTs) `concatMap` args
              stackWithTheGoal        = addToStack stack name terms preAnnotatedGoal
           in trace (show $ (interestS, (preAnnotatedGoal, stackWithTheGoal))) (interestS, gamma', (preAnnotatedGoal, stackWithTheGoal))

        annotateUnfolded :: ([S], E.Gamma, ([[G (S, Ann)]], Stack)) -> (G (S, Ann), Stack)
        annotateUnfolded (interestS, gamma, goalStack) = trace (mainName ++ ": annotatedUnfolded") $
          let 
              (annotatedGoal, updStack) = annotateInternal True name gamma goalStack
              anns                      = annsByGoal interestS annotatedGoal
              stackTerms                = updTermsByAnn anns terms
              isInvDef                  = not $ disjStackPred name (concat annotatedGoal, updStack) 
              updUpdStack               = addToStack updStack name stackTerms annotatedGoal
              updTerms                  = if isInvDef then selfUpdTerms else terms
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


annsByGoal :: [S] -> [[G (S, Ann)]] -> [Ann]
annsByGoal interestS goal = 
  let varAnns  = commonVarAnns . concat $ goal
      sToAnn   = M.fromList . filter ((`elem` interestS) . fst) $ varAnns
      argsAnns = (\s -> fromMaybe Nothing $ M.lookup s sToAnn) <$> interestS
   in argsAnns

----------------------------------------------------------------------------------------------------

getVarsT :: (Eq a) => Term a -> [a]
getVarsT = nub . go
  where
    go (V v)       = [v]
    go (C _ terms) = go `concatMap` terms


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


resetInAnn :: Term (S, Ann) -> Ann
resetInAnn = maybe Nothing (const $ Just 0) . maxAnn


replaceUndef :: Ann -> Term (S, Ann) -> Term (S, Ann)
replaceUndef ann (V (s, Nothing))  = V (s, ann)
replaceUndef ann v@(V (_, oldAnn)) = v
replaceUndef ann (C name terms)    = C name . fmap (replaceUndef ann) $ terms


updTermsByAnn :: [Ann] -> [Term (S, Ann)] -> [Term (S, Ann)]
updTermsByAnn anns = fmap (uncurry replaceUndef) . zip anns


maxAnnList :: [Term (S, Ann)] -> Ann
maxAnnList terms =
  let annList = catMaybes . fmap maxAnn $ terms
   in if null annList then Nothing else Just . maximum $ annList

----------------------------------------------------------------------------------------------------

argsOrder :: [Term (S, Ann)] -> [[G (S, Ann)]] -> ArgsOrder
argsOrder terms = ArgsOrder (maxAnn <$> terms)

annToMask :: [Ann] -> [[S]]
annToMask = fmap (fmap fst)
          . groupBy (\(_, ann1) (_, ann2) -> ann1 == ann2)
          . sortBy (\(_, ann1) (_, ann2) -> compare ann1 ann2)
          . zip [0..]

----------------------------------------------------------------------------------------------------

instance Show ArgsOrder where
  show (ArgsOrder anns goal) =
       "\n"
    ++ "( " ++ intercalate "   |   " (printAnns anns) ++ " )\n"
    ++ printGoal goal
    ++ "\n"
    where
      printAnns :: [Ann] -> [String]
      printAnns = fmap (\(x, ann) -> show x ++ ": " ++ maybe "undef" show ann) . zip [0..]

      printGoal :: [[G (S, Ann)]] -> String
      printGoal = show . foldl1 (|||) . fmap (foldl1 (&&&))


showVar' :: (S, Ann) -> String
showVar' (s, ann) = "v" ++ show s ++ "." ++ maybe "x" show ann


instance {-# OVERLAPPING #-} Show (Term (S, Ann)) where
  show (V v) = showVar' v
  show (C name []) | isNil name = "[]"
  show (C name [h, t]) | isCons name = printf "(%s : %s)" (show h) (show t)
  show c | isSucc c || isZero c = pretifyNum 0 c show showVar
  show (C name ts) = case ts of
                       [] -> name
                       _  -> printf "C %s [%s]" name (intercalate ", " $ map show ts)


instance {-# OVERLAPPING #-} Show (G (S, Ann)) where
  show (t1 :=:  t2) = printf "%s = %s" (show t1) (show t2)
  show (g1 :/\: g2) = printf "(%s /\\ %s)" (show g1) (show g2)
  show (g1 :\/: g2) = printf "(%s \\/ %s)" (show g1) (show g2)
  show (Fresh name g) =
    let (names, goal) = freshVars [name] g
     in printf "fresh %s (%s)" (unwords $ map show $ reverse names) (show goal)
  show (Invoke name ts) =
    printf "%s %s" name 
                     (unwords $ map (\x -> if ' ' `elem` x then printf "(%s)" x else x) $ map show ts)
  show (Let (Def name args body) g) =
    printf "let %s %s = %s in %s" name (unwords args) (show body) (show g)

----------------------------------------------------------------------------------------------------
