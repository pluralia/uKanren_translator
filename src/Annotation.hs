{-# LANGUAGE TupleSections #-}

module Annotation (
    translate
  , PreAnn (..)
  ) where


import           Data.Bifunctor        (first, second)
import           Data.Foldable         (foldl')
import qualified Data.Map.Strict  as M
import           Data.Maybe            (fromMaybe, isJust, catMaybes, isNothing)
import           Data.List             (groupBy, sortBy, nub)
import qualified Data.Set         as S

import qualified CPD.LocalControl as LC
import qualified Eval             as E
import           Syntax

import           Debug.Trace           (trace)

----------------------------------------------------------------------------------------------------

translate :: Program -> [(X, PreAnn)] -> ([[G (S, Ann)]], Stack)
translate (Program scope goal) = uncurry annotate . initTranslation gamma goal
  where
    gamma = E.updateDefsInGamma E.env0 scope

----------------------------------------------------------------------------------------------------

initTranslation ::  E.Gamma -> G X -> [(X, PreAnn)] -> (E.Gamma, [[G (S, Ann)]])
initTranslation gamma goal xPreAnn =
  let (unfreshesGoal, gamma'@(_, (_, xToTs), _), _) = E.preEval gamma goal
      normalizeGoal                                 = LC.normalize unfreshesGoal
      xAnn                                          = second preAnnToAnn <$> xPreAnn
      firAnnotateGoal                               = fmap (initAnnotate xToTs xAnn) <$> normalizeGoal
   in (gamma', trace (show firAnnotateGoal ++ "\n") firAnnotateGoal)


initAnnotate :: (X -> Ts) -> [(X, Ann)] -> G S -> G (S, Ann)
initAnnotate xToTs xAnn = fmap (\s -> (s, fromMaybe Nothing $ lookup s sAnn))
  where
    sAnn = first ((\(V s) -> s) . xToTs) <$> xAnn

----------------------------------------------------------------------------------------------------

data PreAnn = In
  deriving (Show, Eq)

preAnnToAnn :: PreAnn -> Ann
preAnnToAnn In = Just 0


-- Nothing ~ Undefined
-- Just x ~ x - binding time
type Ann = Maybe Word


data ArgsOrder = ArgsOrder [(Ann, S.Set S)] [[G (S, Ann)]]
  deriving (Show)

instance Eq ArgsOrder where
  (ArgsOrder ao1 _) == (ArgsOrder ao2 _) = cmp (getOrderMasks ao1) (getOrderMasks ao2)
    where
      getOrderMasks :: [(Ann, S.Set S)] -> [[S]]
      getOrderMasks = fmap (S.toList . snd)

      cmp :: [[S]] -> [[S]] -> Bool
      cmp []       []       = True
      cmp x        []       = True
      cmp []       y        = True
      cmp xss@(x : xs) yss@(y : ys) = (x == y) && cmp xs ys ||
                                      go x (concat yss) && cmp xs (rem x yss)  ||
                                      go y (concat xss) && cmp (rem y xss) ys
        where
          go :: [S] -> [S] -> Bool
          go []       _  = True
          go (x : xs) ys = x `elem` ys && go xs ys

          rem :: [S] -> [[S]] -> [[S]]
          rem xs = fmap (filter (`notElem` xs))
  
instance Ord ArgsOrder where
  (ArgsOrder argsOrder1 _) <= (ArgsOrder argsOrder2 _) = fmap snd argsOrder1 <= fmap snd argsOrder2

type Stack = M.Map Name (S.Set ArgsOrder)

----------------------------------------------------------------------------------------------------

maxAnn :: Term (S, Ann) -> Ann
maxAnn (V (s, ann)) = ann
maxAnn (C _ terms)  = maybe Nothing handlingAnnList . sequence . fmap maxAnn $ terms
  where
    handlingAnnList :: [Word] -> Maybe Word
    handlingAnnList list = if null list then Nothing else Just . maximum $ list


maxAnnList :: [Term (S, Ann)] -> Ann
maxAnnList terms =
  let annList = catMaybes . fmap maxAnn $ terms
   in if null annList
        then Nothing
        else Just . maximum $ annList


replaceUndef :: Ann -> Term (S, Ann) -> Term (S, Ann)
replaceUndef ann (V (s, Nothing))  = V (s, ann)
replaceUndef ann v@(V (_, oldAnn)) = if ann < oldAnn then error "breaking monotony" else v
replaceUndef ann (C name terms)    = C name . fmap (replaceUndef ann) $ terms


argsOrder :: [Term (S, Ann)] -> [[G (S, Ann)]] -> ArgsOrder
argsOrder terms = ArgsOrder (zip annOfEveryGroup (fmap (S.fromList . fmap fst) groupedS))
  where
    groupedS :: [[(S, Ann)]]
    groupedS = groupBy (\(_, ann1) (_, ann2) -> ann1 == ann2)
             . sortBy (\(_, ann1) (_, ann2) -> compare ann1 ann2)
             . zip [0..]
             . fmap maxAnn $ terms
 
    annOfEveryGroup :: [Ann]
    annOfEveryGroup = fmap (snd . head) groupedS

----------------------------------------------------------------------------------------------------

meet :: Ann -> Ann -> Ann
meet Nothing  x        = x
meet x        Nothing  = x
meet (Just x) (Just y) = Just $ max x y


meetGoals :: [G (S, Ann)] -> [G (S, Ann)]
meetGoals conjs = fmap (\(s, ann) -> (s, fromMaybe ann $ lookup s updAnnots)) <$> conjs
  where
    updAnnots :: [(S, Ann)]
    updAnnots = commonVarAnns conjs


commonVarAnns :: [G (S, Ann)] -> [(S, Ann)]
commonVarAnns = fmap (second (foldl' meet Nothing) . first head . unzip)
          . groupBy (\(s1, _) (s2, _) -> s1 == s2)
          . sortBy (\(s1, _) (s2, _) -> compare s1 s2)
          . concatMap getVars


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

annotate :: E.Gamma -> [[G (S, Ann)]] -> ([[G (S, Ann)]], Stack)
annotate gamma goal = annotateInternal gamma . (, M.empty) $ goal


annotateInternal :: E.Gamma -> ([[G (S, Ann)]], Stack) -> ([[G (S, Ann)]], Stack)
annotateInternal gamma@(defByName, (_, xToTs), _) = fixPoint
  where
    fixPoint :: ([[G (S, Ann)]], Stack) -> ([[G (S, Ann)]], Stack)
    fixPoint goalStack =
      let goalStack1 = annotateGoal goalStack
          goalStack2 = annotateGoal goalStack1
       in if goalStack1 == goalStack2
            then goalStack1
            else fixPoint goalStack2

    execWithSt :: ((a, b) -> (a, b)) -> ([a], b) -> ([a], b)
    execWithSt exec (list, stD) = foldr (\x (acc, st) -> (: acc) `first` exec (x, st)) ([], stD) list

    annotateGoal :: ([[G (S, Ann)]], Stack) -> ([[G (S, Ann)]], Stack)
    annotateGoal = execWithSt annotateDisj

    annotateDisj :: ([G (S, Ann)], Stack) -> ([G (S, Ann)], Stack)
    annotateDisj = first meetGoals . execWithSt annotateConj

    annotateConj :: (G (S, Ann), Stack) -> (G (S, Ann), Stack)
    annotateConj (unif@(t1 :=: t2), stack) = (meetTerm t1 t2, stack)
      where
        meetTerm :: Term (S, Ann) -> Term (S, Ann) -> G (S, Ann)
        meetTerm (V (s, Nothing)) _ = V (s, fmap succ . maxAnn $ t2) :=: t2
        meetTerm (V (s, ann))     _ = t1 :=: replaceUndef (succ <$> ann) t2
        meetTerm (C name1 terms1) (C name2 terms2)
          | name1 == name2, length terms1 == length terms2 =
             let (t1', t2') = unzip
                            . fmap ((\(x :=: y) -> (x, y)) . uncurry meetTerm) $ zip terms1 terms2
              in C name1 t1' :=: C name2 t2'
          | otherwise = error "fail of ctors unification"
        meetTerm _                _ = let (t2' :=: t1') = meetTerm t2 t1 in t1 :=: t2
    
    annotateConj goal@(Invoke name terms, stack)
      | isAllUndef   = goal
      | checkInStack = updConj selfUpdTerms undefined
      | otherwise    = annotateUnfolded . unfoldByDef . defByName $ name
      where
        -- general funcs
        addToStack :: [Term (S, Ann)] -> [[G (S, Ann)]] -> Stack -> Stack
        addToStack terms goal stack =
          let updArgsOrderSet = maybe S.empty (S.insert (argsOrder terms goal)) $ M.lookup name stack
           in M.insert name updArgsOrderSet stack
        
        updTermsByAnn :: [Ann] -> [Term (S, Ann)]
        updTermsByAnn anns = uncurry replaceUndef <$> zip anns terms

        -- if all terms undefined - skip it
        isAllUndef :: Bool
        isAllUndef = isNothing $ maxAnnList terms

        -- in stack
        checkInStack :: Bool
        checkInStack = maybe False (S.member (argsOrder terms undefined)) $ M.lookup name stack

        selfUpdTerms :: [Term (S, Ann)]
        selfUpdTerms = updTermsByAnn . replicate (length terms) . fmap succ . maxAnnList $ terms

        updConj :: [Term (S, Ann)] -> [[G (S, Ann)]] -> (G (S, Ann), Stack)
        updConj updTerms goal = (Invoke name updTerms, addToStack updTerms goal stack)

        -- not in stack
        unfoldByDef :: Def -> ([[G (S, Ann)]], [S], Stack)
        unfoldByDef (Def _ args goal) =
          let (unfreshesGoal, _, _) = E.preEval gamma goal
              normalizeGoal         = LC.normalize unfreshesGoal
              xAnn                  = zip args (maxAnn <$> terms)
              firAnnotateGoal       = fmap (initAnnotate xToTs xAnn) <$> normalizeGoal
              interestS             = fmap ((\(V s) -> s) . xToTs) args
              stackWithTheGoal      = addToStack terms firAnnotateGoal stack
           in (firAnnotateGoal, interestS, stackWithTheGoal)

        annotateUnfolded :: ([[G (S, Ann)]], [S], Stack) -> (G (S, Ann), Stack)
        annotateUnfolded (goal, interestS, stackWithTheGoal) =
          let (annotateGoal, updStack) = annotateInternal gamma (goal, stackWithTheGoal)
              argsAnns                 = argsAnnsByGoal interestS annotateGoal
              updTerms                 = updTermsByAnn argsAnns
              updUpdStack              = addToStack updTerms annotateGoal updStack
           in (Invoke name updTerms, updUpdStack)

        argsAnnsByGoal :: [S] -> [[G (S, Ann)]] -> [Ann]
        argsAnnsByGoal interestS goal = 
          let varAnns  = commonVarAnns . concat $ goal
              sToAnn   = M.fromList . filter ((`elem` interestS) . fst) $ varAnns
              argsAnns = (\s -> fromMaybe Nothing $ M.lookup s sToAnn) <$> interestS
           in argsAnns

    annotateConj _                  = error "forbidden goal for conj"

----------------------------------------------------------------------------------------------------
