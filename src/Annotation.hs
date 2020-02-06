{-# LANGUAGE TupleSections #-}

module Annotation (
    translate
  ) where


import           Data.Bifunctor        (first, second)
import           Data.Foldable         (foldl')
import qualified Data.Map.Strict  as M
import           Data.Maybe            (fromMaybe, isJust)
import           Data.List             (groupBy, sortBy, nub)
import qualified Data.Set         as S

import qualified CPD.LocalControl as LC
import qualified Eval             as E
import           Syntax

{-
scope :: [Def]
scope = [
          Def "reverso" ["x","y"] $
            V "x" :=: C "Nil" [] :/\: V "y" :=: C "Nil" [] :\/: Fresh "h" (Fresh "t" (Fresh "rt" (V "x" :=: C "Cons" [V "h",V "t"] :/\: (Invoke "reverso" [V "t",V "rt"] :/\: Invoke "appendo" [V "rt",C "Cons" [V "h",C "Nil" []],V "y"]))))
        , Def "appendo" ["x","y","xy"] $
            V "x" :=: C "Nil" [] :/\: V "xy" :=: V "y" :\/: Fresh "h" (Fresh "t" (Fresh "ty" (V "x" :=: C "Cons" [V "h",V "t"] :/\: (V "xy" :=: C "Cons" [V "h",V "ty"] :/\: Invoke "appendo" [V "t",V "y",V "ty"]))))
        ]
-}

----------------------------------------------------------------------------------------------------

translate :: Program -> [(X, PreAnn)] -> ([[G (S, Ann)]], Stack)
translate (Program scope goal) = uncurry annotate . initTranslation gamma goal
  where
    gamma = E.updateDefsInGamma E.env0 scope

----------------------------------------------------------------------------------------------------

initTranslation ::  E.Gamma -> G X -> [(X, PreAnn)] -> (E.Gamma, [[G (S, Ann)]])
initTranslation gamma goal xPreAnn =
  let (goalWithoutFreshes, gamma', _)                 = E.preEval gamma goal
      (oneStepUnfoldGoal, gamma''@(_, (_, xToTs), _)) = LC.oneStepUnfold goalWithoutFreshes gamma'
      normalizeGoal                                   = LC.normalize oneStepUnfoldGoal
      xAnn                                            = second preAnnToAnn <$> xPreAnn
      firAnnotateGoal                                 = fmap (initAnnotate xToTs xAnn) <$> normalizeGoal
   in (gamma'', firAnnotateGoal)


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

type ArgsOrder = [S.Set S]
type Stack = M.Map Name (M.Map ArgsOrder [[G (S, Ann)]])

----------------------------------------------------------------------------------------------------

maxAnn :: Term (S, Ann) -> Ann
maxAnn (V (s, ann)) = ann
maxAnn (C _ terms)  = fmap maximum . sequence . fmap maxAnn $ terms


replaceUndef :: Ann -> Term (S, Ann) -> Term (S, Ann)
replaceUndef ann (V (s, Nothing))  = V (s, ann)
replaceUndef ann v@(V (_, oldAnn)) = if ann < oldAnn then error "breaking monotony" else v
replaceUndef ann (C name terms)    = C name . fmap (replaceUndef ann) $ terms


argsOrder :: [Term (S, Ann)] -> ArgsOrder
argsOrder terms = fmap (S.fromList . fmap fst)
                . groupBy (\(_, ann1) (_, ann2) -> ann1 == ann2)
                . sortBy (\(_, ann1) (_, ann2) -> compare ann1 ann2)
                . zip [0..]
                . fmap maxAnn $ terms

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
annotate gamma = annotateInternal gamma . (, M.empty)


annotateInternal :: E.Gamma -> ([[G (S, Ann)]], Stack) -> ([[G (S, Ann)]], Stack)
annotateInternal gamma@(defByName, (_, xToTs), _) = findFixPoint
  where
    findFixPoint :: ([[G (S, Ann)]], Stack) -> ([[G (S, Ann)]], Stack)
    findFixPoint goalStack =
      let goalStack1@(goal1, _) = annotateGoal goalStack
          goalStack2@(goal2, _) = annotateGoal goalStack1
       in if goal1 == goal2
            then goalStack1
            else findFixPoint goalStack2

    execWithSt :: ((a, b) -> (a, b)) -> ([a], b) -> ([a], b)
    execWithSt exec (list, stD) = foldl' (\(acc, st) x -> (: acc) `first` exec (x, st)) ([], stD) list

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
    
    annotateConj info@(Invoke name terms, stack)
      | checkInStack = info
      | otherwise    = annotateByUnfolded . unfoldByDef . defByName $ name
      where
        checkInStack :: Bool
        checkInStack = isJust $ M.lookup name stack >>= M.lookup (argsOrder terms)

        unfoldByDef :: Def -> ([[G (S, Ann)]], [S])
        unfoldByDef (Def _ args goal) =
          let (goalWithoutFreshes, _, _) = E.preEval gamma goal
              normalizeGoal              = LC.normalize goalWithoutFreshes
              xAnn                       = zip args (maxAnn <$> terms)
              firAnnotateGoal            = fmap (initAnnotate xToTs xAnn) <$> normalizeGoal
              interestS                  = fmap ((\(V s) -> s) . xToTs) args
           in (firAnnotateGoal, interestS)

        annotateByUnfolded :: ([[G (S, Ann)]], [S]) -> (G (S, Ann), Stack)
        annotateByUnfolded (goal, interestS) =
          let (annotateGoal, updStack) = annotateInternal gamma (goal, stack)
              interestAnn              = annByGoal interestS annotateGoal
              updTerms                 = updTermsAnn interestAnn
              updUpdStack              = makeStackUpdIf interestAnn updTerms goal $ updStack
           in (Invoke name updTerms, updUpdStack)

        annByGoal :: [S] -> [[G (S, Ann)]] -> [Ann]
        annByGoal interestS goal = fmap (\s -> fromMaybe Nothing $ M.lookup s sToAnn) interestS
          where
            sToAnn :: M.Map S Ann
            sToAnn = M.fromList . filter (\(s, _) -> s `elem` interestS) . commonVarAnns . concat $ goal

        updTermsAnn :: [Ann] -> [Term (S, Ann)]
        updTermsAnn = fmap (uncurry (flip replaceUndef)) . zip terms

        makeStackUpdIf :: [Ann] -> [Term (S, Ann)] -> [[G (S, Ann)]] -> Stack -> Stack
        makeStackUpdIf interestAnn updTerms goal =
          maybe id (\_ -> addToStack updTerms goal) $ sequence interestAnn

        addToStack :: [Term (S, Ann)] -> [[G (S, Ann)]] -> Stack -> Stack
        addToStack updTerms goal updStack =
          let argsMask   = argsOrder updTerms
              maybeInsts = M.lookup name updStack
              instances  = (maybe M.singleton (\x k a -> M.insert k a x) maybeInsts) argsMask goal
           in M.insert name instances updStack

    annotateConj _                  = error "forbidden goal for conj"

----------------------------------------------------------------------------------------------------
