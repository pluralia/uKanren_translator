{-# LANGUAGE TupleSections #-}

module Translator (
    translate
  ) where

import           Data.Either          (partitionEithers)
import           Data.Bifunctor       (bimap, first, second)
import           Data.Foldable        (foldl')
import           Data.List            (partition, sortOn, groupBy, (\\))
import qualified Data.Map.Strict as M
import           Data.Tuple           (swap)

import           Embed                (Instance (..))
import           Syntax

import           AFSyntax
import           Annotator.Main
import           Annotator.Types

import           Debug.Trace

-----------------------------------------------------------------------------------------------------

showS :: S -> String
showS s = 's' : show s

termToAtom :: Term (S, A) -> Atom
termToAtom (V (s, _))     = Var . showS $ s
termToAtom (C name terms) = Ctor name . fmap termToAtom $ terms

-----------------------------------------------------------------------------------------------------

translate :: [AnnDef] -> HsProgram
translate = HsProgram . fmap translateAnnDef


translateAnnDef :: AnnDef -> F
translateAnnDef (AnnDef name args goal) =
  let newName = (name ++) . concatMap (\a -> if a == 0 then "I" else "O") . snd . unzip $ args
   in F newName . fmap (disjToLine args) $ goal

-----------------------------------------------------------------------------------------------------

disjToLine :: [(S, A)] -> [Conj] -> Line
disjToLine args disj =
  let
      (inArgs, outArgs) = bimap (fmap fst) (fmap fst) $ partition ((== 0) . snd) args

      (dupVarsPats, conjs) = extractPats inArgs disj
      (patGuards, pats)    = handleDupVars 'p' dupVarsPats

      (assignGuardsFir, annAssigns) = partitionEithers . fmap conjToGuardOrAssign $ conjs
      sortedAssigns                 = fmap snd . sortOn fst $ annAssigns
      (assignGuardsSec, assigns)    = handleDupVarsInAssigns sortedAssigns
      assignGuards                  = assignGuardsFir ++ assignGuardsSec
      genAssigns                    = generateIfNotAssign assigns (showS <$> outArgs)

      expr = Term . Tuple . fmap showS $ outArgs
   in Line pats patGuards genAssigns assignGuards expr 

-----------------------------------------------------------------------------------------------------

generateIfNotAssign :: [Assign] -> [String] -> [Assign]
generateIfNotAssign assigns outArgs =
  let
      assignedVars = (\(Assign atom _) -> getAtomVars atom) `concatMap` assigns
      varsForGen   = outArgs \\ assignedVars
   in assigns ++ ((\var -> Assign (Var var) (Term $ Ctor "gen" [])) <$> varsForGen)
  where
    getAtomVars :: Atom -> [String]
    getAtomVars (Var v)        = [v]
    getAtomVars (Ctor _ atoms) = getAtomVars `concatMap` atoms
    getAtomVars (Tuple vs)     = vs

-----------------------------------------------------------------------------------------------------

handleDupVarsInAssigns :: [Assign] -> ([Guard], [Assign])
handleDupVarsInAssigns assigns =
  let
      (atoms, exprs)    = unzip . fmap (\(Assign atom expr) -> (atom, expr)) $ assigns
      pats              = (Pat Nothing) <$> atoms
      (guards, updPats) = handleDupVars 'c' pats
      updAtoms          = (\(Pat _ atom) -> atom) <$> updPats
      updAssigns        = zipWith Assign updAtoms exprs
   in (guards, updAssigns)


handleDupVarsInPats :: [Pat] -> ([Guard], [Pat])
handleDupVarsInPats = handleDupVars 'p'


handleDupVars :: Char -> [Pat] -> ([Guard], [Pat])
handleDupVars symb pats =
  let
      newVarsNames              = ((symb :) . show) <$> [0..]
      (updPats, (_, oldToNews)) = foldr rename ([], (newVarsNames, M.empty)) pats
      newToOld                  = M.fromList . fmap (swap . second head) . M.toList $ oldToNews
      updUpdPats                = backFirst newToOld <$> updPats
      listForGuards             = M.toList . M.filter (not . null . tail) $ oldToNews
      guards                    = (Guard . fmap Var . (\(v, _ : vs) -> v : vs)) <$> listForGuards
   in (guards, updUpdPats) 
  where
    backFirst :: M.Map String String -> Pat -> Pat
    backFirst newToOld = go
      where
        go :: Pat -> Pat
        go (Pat Nothing atom)  = Pat Nothing . backFirstAtom $ atom
        go (Pat (Just v) atom) = Pat (Just . maybe v id $ M.lookup v newToOld) . backFirstAtom $ atom
    
        backFirstAtom :: Atom -> Atom
        backFirstAtom (Var v)          = Var . maybe v id $ M.lookup v newToOld
        backFirstAtom (Ctor name args) = Ctor name . fmap backFirstAtom $ args
        backFirstAtom (Tuple vars)     = Tuple . fmap ((\(Var v) -> v) . backFirstAtom . Var) $ vars


    rename :: Pat -> 
              ([Pat], ([String], M.Map String [String])) ->
              ([Pat], ([String], M.Map String [String]))
    rename (Pat Nothing atom)  (pats, nsOldToNews) =
      ((: pats) . Pat Nothing) `first` renameAtom atom nsOldToNews
    rename (Pat (Just v) atom) (pats, ((n : ns), oldToNew)) =
      ((: pats) . Pat (Just n)) `first` renameAtom atom (ns, M.insertWith (++) v [n] oldToNew)


    renameAtom :: Atom ->
                  ([String], M.Map String [String]) ->
                  (Atom, ([String], M.Map String [String]))
    renameAtom (Var v)          ((n : ns), oldToNews) =
      (Var n, (ns, M.insertWith (++) v [n] oldToNews))
    renameAtom (Ctor name args) nsOldToNews           =
      first (Ctor name) .
      foldr (\arg (acc, info) -> (: acc) `first` renameAtom arg info) ([], nsOldToNews) $ args
    renameAtom (Tuple vars)     nsOldToNews           =
      first (Tuple . fmap (\(Var v) -> v)) .
      foldr (\arg (acc, info) -> (: acc) `first` renameAtom arg info) ([], nsOldToNews) .
      fmap Var $ vars

-----------------------------------------------------------------------------------------------------

extractPats :: [S] -> [Conj] -> ([Pat], [Conj])
extractPats inVars conjs =  
  let
      (conjsForPats, conjsForAll) = partition isForPat conjs
      (pats, conjsFromPats)       = getPats . toMap $ conjsForPats
   in (pats, conjsFromPats ++ conjsForAll)
  where
    isForPat :: Conj -> Bool
    isForPat (U (V (s, _)) _) = s `elem` inVars
    isForPat (U _ (V (s, _))) = s `elem` inVars
    isForPat _                = False

    toMap :: [Conj] -> M.Map S [Term (S, A)]
    toMap []                            = M.empty
    toMap ((U (V (s, _)) term) : conjs) = M.insertWith (++) s [term] (toMap conjs)
    toMap ((U term (V (s, _))) : conjs) = M.insertWith (++) s [term] (toMap conjs)

    getPats :: M.Map S [Term (S, A)] -> ([Pat], [Conj])
    getPats patMap = 
      second concat .
      sequence .
      fmap (\s -> maybe ([Pat Nothing (Var . showS $ s)], []) (onePat s) $ M.lookup s patMap) $ inVars

    onePat :: S -> [Term (S, A)] -> ([Pat], [Conj])
    onePat _ []     = error "extractPats: impossible case"
    onePat s terms  =
      bimap ((: []) . Pat (Just . showS $ s) . termToAtom) (fmap (U (V (s, 0)))) $ chooseTermForPat terms

    chooseTermForPat :: [Term (S, A)] -> (Term (S, A), [Term (S, A)])
    chooseTermForPat [term]         = (term, [])
    chooseTermForPat (tx : [ty])
      | tx `isInst` ty = (ty, [tx])
      | otherwise      = (tx, [ty])
    chooseTermForPat (tx : ty : terms)
      | tx `isInst` ty = second (tx :) $ chooseTermForPat (ty : terms)
      | otherwise      = second (ty :) $ chooseTermForPat (tx : terms)

-----------------------------------------------------------------------------------------------------

getMaxAnn :: Term (S, A) -> A
getMaxAnn (V (_, a))  = a
getMaxAnn (C _ [])    = 0
getMaxAnn (C _ terms) = maximum $ getMaxAnn <$> terms


chooseDirection :: [(S, A)] -> (([S], (A, [S])), String)
chooseDirection args =
  let
    maxAnn    = maximum . fmap snd $ args
    direction = concatMap (\a -> if a == maxAnn then "O" else "I") . snd . unzip $ args
   in (bimap (fmap fst) ((maxAnn,) . fmap fst) . partition ((/= maxAnn) . snd) $ args, direction)


conjToGuardOrAssign :: Conj -> Either Guard (A, Assign)
conjToGuardOrAssign (U u1 u2)     =
  case compare (getMaxAnn u1) (getMaxAnn u2) of
    LT -> Right (getMaxAnn u2, Assign (termToAtom u2) (Term $ termToAtom u1))
    GT -> Right (getMaxAnn u1, Assign (termToAtom u1) (Term $ termToAtom u2))
    EQ -> Left . Guard . fmap termToAtom $ [u1, u2]
conjToGuardOrAssign (I name args) =
  let ((inArgs, (ann, outArgs)), direction) = chooseDirection args
      newName                             = name ++ direction
   in Right (ann, Assign (Tuple . fmap showS $ outArgs) (Call newName . fmap (Var . showS) $ inArgs))

-----------------------------------------------------------------------------------------------------

