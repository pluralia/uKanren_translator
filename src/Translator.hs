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
translate annDefs =
  let defNames = makeNewName <$> annDefs
   in HsProgram . fmap (translateAnnDef defNames) $ annDefs


translateAnnDef :: [Name] -> AnnDef -> F
translateAnnDef defNames annDef@(AnnDef name args goal) =
  let newName = makeNewName annDef
   in F newName . fmap (disjToLine defNames args) $ goal


makeNewName :: AnnDef -> Name
makeNewName (AnnDef name args _) =
  (name ++) . concatMap (\a -> if a == 0 then "I" else "O") . snd . unzip $ args

-----------------------------------------------------------------------------------------------------

disjToLine :: [Name] -> [(S, A)] -> [Conj] -> Line
disjToLine defNames args disj =
  let
      (inArgs, outArgs) = bimap (fmap fst) (fmap fst) $ partition ((== 0) . snd) args

      (dupVarsPats, conjs) = extractPats inArgs disj
      (patGuards, pats)    = handleDupVars 'p' dupVarsPats

      (assignGuardsFir, annAssigns) = partitionEithers . fmap (conjToGuardOrAssign defNames) $ conjs
      sortedAssigns                 = fmap snd . sortOn fst $ annAssigns
      (assignGuardsSec, assigns)    = handleDupVarsInAssigns sortedAssigns
      assignGuards                  = assignGuardsFir ++ assignGuardsSec
      genAssigns                    = generateIfNotAssign pats assigns (showS <$> outArgs)

      expr = Term . Tuple . fmap showS $ outArgs
   in Line pats patGuards genAssigns assignGuards expr 

-----------------------------------------------------------------------------------------------------

generateIfNotAssign :: [Pat] -> [Assign] -> [String] -> [Assign]
generateIfNotAssign pats assigns outArgs =
  let
      assignVars = (\(Assign atom _) -> getAtomVars atom) `concatMap` assigns
      patVars    =
        (\(Pat mb atom) -> let res = getAtomVars atom in maybe res (: res) mb) `concatMap` pats
      varsForGen = outArgs \\ (assignVars ++ patVars)
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


chooseDirection :: [Name] -> Conj -> (([S], (A, [S])), Name)
chooseDirection defNames (I name args) =
  let
    anns = snd <$> args
    maxAnn  = maximum anns
    genAll  = (name ++) . take (length anns) . repeat
    (isSwp, newName) = if all (== maxAnn) anns
                        then retResName defNames (genAll 'I') (genAll 'O')
                        else (False, name ++ (fmap (\ann -> if ann == maxAnn then 'O' else 'I') anns))
    inOutArgs        = (if isSwp then swap else id) . partition ((/= maxAnn) . snd) $ args
   in (bimap (fmap fst) ((maxAnn,) . fmap fst) $ inOutArgs, newName)
  where
    retResName :: [Name] -> Name -> Name -> (Bool, Name)
    retResName defNames nameI nameO
      | nameI `elem` defNames = (True, nameI)
      | nameO `elem` defNames = (False, nameO)
      | otherwise             = error $ "Undef function: " ++ nameI ++ " or " ++ nameO 


conjToGuardOrAssign :: [Name] -> Conj -> Either Guard (A, Assign)
conjToGuardOrAssign _ (U u1 u2)     =
  case compare (getMaxAnn u1) (getMaxAnn u2) of
    LT -> Right (getMaxAnn u2, Assign (termToAtom u2) (Term $ termToAtom u1))
    GT -> Right (getMaxAnn u1, Assign (termToAtom u1) (Term $ termToAtom u2))
    EQ -> Left . Guard . fmap termToAtom $ [u1, u2]
conjToGuardOrAssign defNames inv =
  let ((inArgs, (ann, outArgs)), newName) = chooseDirection defNames inv
   in Right (ann, Assign (Tuple . fmap showS $ outArgs) (Call newName . fmap (Var . showS) $ inArgs))

-----------------------------------------------------------------------------------------------------

