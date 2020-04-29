{-# LANGUAGE TupleSections #-}

module Translator (
    translate
  ) where

import           AFSyntax
import           Annotator.Main
import           Annotator.Types (AnnDef (..), Conj (..))

import           Syntax

import           Data.Either (partitionEithers)
import           Data.Bifunctor (bimap, first, second)
import           Data.List (partition, sortOn, groupBy)
import qualified Data.Map.Strict as M
import           Data.Tuple (swap)

import           Debug.Trace

-----------------------------------------------------------------------------------------------------

type A = Word

showS :: S -> String
showS s = 's' : show s

termToPat :: Term (S, A) -> Pat
termToPat (V (s, _))     = Var . showS $ s
termToPat (C name terms) = Ctor name . fmap termToPat $ terms

-----------------------------------------------------------------------------------------------------

translate :: [AnnDef] -> [F]
translate = fmap translateAnnDef


translateAnnDef :: AnnDef -> F
translateAnnDef (AnnDef name args goal) = F name . fmap (disjToLine args) $ goal

-----------------------------------------------------------------------------------------------------

disjToLine :: [(S, A)] -> [Conj] -> Line
disjToLine args disj =
  let
      (inArgs, outArgs)     = bimap (fmap fst) (fmap fst) $ partition ((== 0) . snd) args
      (guardsPats, conjs)   = extractPats inArgs disj
      (guardsP, pats)       = patsToGuardsAndPats guardsPats
      (guardsA, annAssigns) = partitionEithers . fmap conjToGuardOrAssign $ conjs
      assigns               = fmap snd . sortOn fst $ annAssigns
      expr                  = Term . Tuple . fmap showS $ outArgs
   in Line pats (guardsP ++ guardsA) assigns expr 

-----------------------------------------------------------------------------------------------------

patsToGuardsAndPats :: [Pat] -> ([Guard], [Pat])
patsToGuardsAndPats = sequence . fmap go
  where
    go :: Pat -> ([Guard], Pat)
    go (Tuple _)        = error "patsToGuardsAndPats: impossible case"
    go pat              = normalizePat pat

    normalizePat :: Pat -> ([Guard], Pat)
    normalizePat pat =
      let (updPat, (_, oldToNew)) = rename pat ((('g' :) . show) <$> [0..], M.empty)
          newToOld                = M.fromList . fmap (swap . second head) . M.toList $ oldToNew
          updUpdPat               = replace newToOld updPat
          guards                  =
            fmap (Guard . fmap Var) . filter ((> 1) . length) . M.elems $ oldToNew
       in (guards, updUpdPat) 

    replace :: M.Map String String -> Pat -> Pat
    replace newToOld (Var v)          = Var $ maybe v id $ M.lookup v newToOld
    replace newToOld (Ctor name args) = Ctor name . fmap (replace newToOld) $ args
    replace _        (Tuple _)        = error "patsToGuardsAndPats: impossible case"

    rename :: Pat -> ([String], M.Map String [String]) -> (Pat, ([String], M.Map String [String]))
    rename (Var v)          ((n : ns), oldToNew) = (Var n, (ns, M.insertWith (++) v [n] oldToNew))
    rename (Ctor name args) nsOldToNew           =
      first (Ctor name) .
      foldr (\arg (acc, info) -> (: acc) `first` rename arg info) ([], nsOldToNew) $ args
    rename (Tuple _)        _                    = error "patsToGuardsAndPats: impossible case"

-----------------------------------------------------------------------------------------------------

extractPats :: [S] -> [Conj] -> ([Pat], [Conj])
extractPats inVars conjs =  
  let (conjsForPats, conjsForAll) = partition isForPat conjs
      (pats, conjsFromPats)       = getPats . toMap $ conjsForPats
   in (pats, conjsFromPats ++ conjsForAll)
  where
    isForPat :: Conj -> Bool
    isForPat (U (V (_, 0)) _) = True
    isForPat (U _ (V (_, 0))) = True
    isForPat _                = False

    toMap :: [Conj] -> M.Map S [Term (S, A)]
    toMap []                            = M.empty
    toMap ((U (V (s, 0)) term) : conjs) = M.insertWith (++) s [term] (toMap conjs)
    toMap ((U term (V (s, 0))) : conjs) = M.insertWith (++) s [term] (toMap conjs)

    getPats :: M.Map S [Term (S, A)] -> ([Pat], [Conj])
    getPats patMap = 
      bimap (fmap termToPat) concat .
      sequence .
      fmap (\s -> maybe ([V (s, 0)], []) (onePat s) $ M.lookup s patMap) $ inVars

    onePat :: S -> [Term (S, A)] -> ([Term (S, A)], [Conj])
    onePat _ []             = error "extractPats: impossible case"
    onePat _ [term]         = ([term], [])
    onePat s (term : terms) = ([term], (\t -> U (V (s, 0)) t) <$> terms)

-----------------------------------------------------------------------------------------------------

getMinAnn :: Term (S, A) -> A
getMinAnn (V (_, a))  = a
getMinAnn (C _ [])    = 0
getMinAnn (C _ terms) = minimum $ getMinAnn <$> terms


chooseDirection :: [(S, A)] -> ([S], (A, [S]))
chooseDirection args = bimap (fmap fst) ((maxAnn,) . fmap fst) . partition ((/= maxAnn) . snd) $ args
  where
    maxAnn = maximum . fmap snd $ args


conjToGuardOrAssign :: Conj -> Either Guard (A, Assign)
conjToGuardOrAssign (U u1 u2)     =
  case compare (getMinAnn u1) (getMinAnn u2) of
    LT -> Right (getMinAnn u2, Assign (termToPat u2) (Term $ termToPat u1))
    GT -> Right (getMinAnn u1, Assign (termToPat u1) (Term $ termToPat u2))
    EQ -> Left . Guard . fmap termToPat $ [u1, u2]
conjToGuardOrAssign (I name args) =
  let (inArgs, (ann, outArgs)) = chooseDirection args
   in Right (ann, Assign (Tuple . fmap showS $ outArgs) (Call name . fmap (Var . showS) $ inArgs))

-----------------------------------------------------------------------------------------------------

