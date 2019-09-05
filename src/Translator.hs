{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Translator (
    translator
  , strTranslator
  ) where

import           AFSyntax
import           MKSyntax
import           Control.Applicative (liftA2)
import           Data.Bifunctor (bimap, first, second)
import           Data.Biapplicative (biliftA2)
import           Data.Char (ord)
import qualified Data.Map.Strict as M
import           Data.List (delete, nub)
import           Data.List.NonEmpty (NonEmpty(..))
import           Debug.Trace
import           Text.Printf (printf)

-----------------------------------------------------------------------------------------------------

myPrint :: [String] -> String
myPrint []       = ""
myPrint (x : xs) = x ++ " | " ++ myPrint xs

-----------------------------------------------------------------------------------------------------

translator :: (Functor f) => f Def -> f F
translator = fmap go

strTranslator :: Maybe Def -> IO ()
strTranslator = maybe (putStrLn "Your program is not parsed") (print . go)

-----------------------------------------------------------------------------------------------------

chooseDirection :: [a] -> ([a], a)
chooseDirection vars = (init vars, last vars) 

go :: Def -> F
go def@(name, vars, goal) =
  F name . addLine . fmap (toLine (chooseDirection vars)) . toDNF . prrr $ goal
  where
    addLine :: [Line] -> [Line]
    addLine lines@((Line pats _ _ _) : ls) = 
      let failLine =
            Line (replicate (length pats) (Var "_")) [] [] (Call "fail" [Var "\"Illegal arguments\""])
       in lines ++ [failLine]
    addLine _                              = error "go : addLine : impossible case"

    prrr = trace (show $ toDNF goal)

-----------------------------------------------------------------------------------------------------

-- disjunction of conjunctions of calls and unifications with fresh vars
toDNF :: G a -> [([Name], [G a])]
toDNF (g1 :/\: g2)      = liftA2 (biliftA2 (++) (++)) (toDNF g1) (toDNF g2)
toDNF (g1 :\/: g2)      = toDNF g1 ++ toDNF g2
toDNF (Fresh varName g) = first (varName :) <$> toDNF g
toDNF g                 = [([], [g])]

-----------------------------------------------------------------------------------------------------

term2pat :: Term X -> Pat
term2pat (V var)        = Var var
term2pat (C name terms) = Ctor name (fmap term2pat terms)

pat2name :: Pat -> [Name]
pat2name (Var varName)        = [varName]
pat2name (Ctor ctorName pats) = pat2name `concatMap` pats

-- REWRITE!! get the first pat for arg but doesn't check another pat - need unification
patWalk :: [Name] -> Name -> [G X] -> ([Pat], [G X])
patWalk []           _   goals = ([], goals)
patWalk (arg : args) res goals = (pat :) `first` patWalk args res goals'
  where
    (pat, goals') = patWalk' goals
    
    patWalk' :: [G X] -> (Pat, [G X])
    patWalk' []                  = (Var arg, [])
    patWalk' (g@(t1 :=: t2) : gs)
      | t1 == V arg, t2 /= V res = (term2pat t2, gs)
      | t2 == V arg, t1 /= V res = (term2pat t1, gs)
    patWalk' (g : gs)            = (g :) `second` patWalk' gs


extractExpr :: Name -> [Assign] -> Maybe (Expr, [Assign])
extractExpr _   [] = Nothing
extractExpr res (assig@(Assign var expr) : assigns)
  | res == var = Just (expr, assigns)
  | otherwise  = second (assig :) <$> extractExpr res assigns

concatGuards :: Guard -> Guard -> Guard
concatGuards (Guard list1) (Guard list2) = Guard . nub $ list1 ++ list2

concatMbGuards :: Maybe Guard -> Maybe Guard -> Maybe Guard
concatMbGuards (Just g1) (Just g2) = Just $ concatGuards g1 g2
concatMbGuards Nothing   mbGuard   = mbGuard
concatMbGuards mbGuard   _         = mbGuard

-----------------------------------------------------------------------------------------------------

toLine :: ([Name], Name) -> ([Name], [G X]) -> Line
-- frehshes weren't used!
toLine (args, res) (freshes, conjs) = line
  where
    line = Line pats' guards assigns' expr

    (pats, restConjs) = patWalk args res conjs
    knownVars         = pat2name `concatMap` (nub pats)
    (pats', guards)   = removeDuplicate pats
    assigns           = toAssign knownVars restConjs
    (expr, assigns')  = maybe (error "res is undefined") id $ extractExpr res assigns

-----------------------------------------------------------------------------------------------------

removeDuplicate :: [Pat] -> ([Pat], [Guard])
removeDuplicate []                            = ([], [])
removeDuplicate (pat@(Var varName) : pats)        =
  let (_, mbGuard, pats') = findNameInPatList varName 1 pats
      (resPats, guards)   = removeDuplicate pats'
   in (pat : resPats, maybe guards (: guards) mbGuard)
removeDuplicate ((Ctor ctorName args) : pats) =
  let (args', guardInfoCtor)           = handlingCtor args
      resPat                           = Ctor ctorName args'
      (substCtorPats, guardInfoCtor')  = pats `updateByGuard` guardInfoCtor
      guardsCtor                       = (\(_, _, x) -> x) <$> guardInfoCtor'
      (substCtorPats', guardsPatsCtor) = substCtorPats `updateByName` pat2name resPat
      (resPats, guardsPats)            = removeDuplicate substCtorPats'
   in (resPat : resPats, guardsCtor ++ guardsPatsCtor ++ guardsPats)


updateByName :: [Pat] -> [Name] -> ([Pat], [Guard])
updateByName pats []             = (pats, [])
updateByName pats (name : names) =
  let (_, mbGuard, pats')   = findNameInPatList name 1 pats
      (resPats, guardsPats) = pats' `updateByName` names
   in (resPats, maybe guardsPats (: guardsPats) mbGuard) 


handlingCtor :: [Pat] -> ([Pat], [(Name, Int, Guard)])
handlingCtor []                                = ([], [])
handlingCtor (arg@(Var varName) : args)        =
  let (num, mbGuardArg, args') = findNameInPatList varName 1 args
      (resArgs, guardInfoList) = handlingCtor args'
   in (arg : resArgs, maybe guardInfoList (\x -> (varName, num, x) : guardInfoList) mbGuardArg)
handlingCtor ((Ctor ctorName ctorArgs) : args) =
  let (ctorArgs', guardInfoCtor) = handlingCtor ctorArgs
      arg'                       = Ctor ctorName ctorArgs'
      (args', guardInfoList)     = args `updateByGuard` guardInfoCtor
   in (arg' : args', guardInfoList)


updateByGuard :: [Pat] -> [(Name, Int, Guard)] -> ([Pat], [(Name, Int, Guard)])
updateByGuard pats []                                   = (pats, [])
updateByGuard pats ((name, num, guard) : guardInfoList) =
  let (num', mbGuards, pats')   = findNameInPatList name num pats
      guardInfo                 = (name, num', maybe guard (\x -> concatGuards x guard) mbGuards)
      (resPats, guardInfoList') = pats' `updateByGuard` guardInfoList
   in (resPats, guardInfo : guardInfoList')


findNameInPatList :: Name -> Int -> [Pat] -> (Int, Maybe Guard, [Pat])
findNameInPatList name = findNameInPatList'
  where
    findNameInPatList' :: Int -> [Pat] -> (Int, Maybe Guard, [Pat])
    findNameInPatList' num []           = (num, Nothing, [])
    findNameInPatList' num (pat : pats) =
      let (num', mbGuard, pat')    = findNameInPat name num pat
          (num'', mbGuards, pats') = findNameInPatList' num' pats
       in (num'', mbGuard `concatMbGuards` mbGuards, pat' : pats')


findNameInPat :: Name -> Int -> Pat -> (Int, Maybe Guard, Pat)
findNameInPat name = findNameInPat'
  where
    findNameInPat' :: Int -> Pat -> (Int, Maybe Guard, Pat)
    findNameInPat' num pat@(Var varName)
      | varName == name =
          let varName' = varName ++ replicate num '\''
           in (succ num, Just . Guard $ [varName, varName'], Var varName')
      | otherwise       = (num, Nothing, pat)
    findNameInPat' num (Ctor ctorName args) =
      let (num', maybeGuard, args') = findNameInPatList name num args
       in (num', maybeGuard, Ctor ctorName args')
  
-----------------------------------------------------------------------------------------------------

data PatTree = Def Pat
             | UndefVar Name
             | UndefCtor Name [PatTree]
  deriving (Eq, Show)

patsOrForest :: [PatTree] -> Either [PatTree] [Pat]
patsOrForest patForest
  | all isDef patForest = Right (unpackDef <$> patForest)
  | otherwise           = Left patForest
    
isDef :: PatTree -> Bool
isDef (Def _) = True
isDef _       = False

unpackDef :: PatTree -> Pat
unpackDef (Def pat) = pat
unpackDef _         = error "try to unpack undef term"
    
pat2tree :: [Name] -> Pat -> PatTree
pat2tree knownVars = pat2tree'
  where
    pat2tree' :: Pat -> PatTree
    pat2tree' var@(Var v) = if v `elem` knownVars then Def var else UndefVar v
    pat2tree' (Ctor ctorName pats) =
      either (UndefCtor ctorName) (Def . Ctor ctorName) . patsOrForest $ pat2tree' <$> pats

walk :: M.Map Name Pat -> Pat -> Pat
walk subst = walk'
  where
    walk' :: Pat -> Pat
    walk' var@(Var v)          = maybe var walk' $ M.lookup v subst
    walk' (Ctor ctorName pats) = Ctor ctorName (walk' <$> pats)
    

data Undef = CallFunc Name Name [PatTree]
           | Unific PatTree PatTree
  deriving (Eq, Show)


unify :: [Name] -> M.Map Name Pat -> Pat -> Pat -> (M.Map Name Pat, [Undef])
unify knownVars subst pat1 pat2 = unify' (walk subst pat1) (walk subst pat2)
  where
    unify' :: Pat -> Pat -> (M.Map Name Pat, [Undef])
    unify' (Ctor ctorName1 args1) (Ctor ctorName2 args2)
      | ctorName1 == ctorName2 = bimap M.unions concat . unzip $ zipWith unify' args1 args2
      | otherwise              = error "ctors unification fails"
    unify' var@(Var _) pat =
      let knownVars'' = knownVars ++ M.keys subst
       in updateSubst subst (pat2tree knownVars'' var) (pat2tree knownVars'' pat)
    unify' ctor@(Ctor _ _) var@(Var _) = unify' var ctor

updateSubst :: M.Map Name Pat -> PatTree -> PatTree -> (M.Map Name Pat, [Undef])
updateSubst subst = updateSubst'
  where
    updateSubst' :: PatTree -> PatTree -> (M.Map Name Pat, [Undef])
    updateSubst' (Def pat1) (Def pat2)
      | pat1 == pat2 = (subst, [])
      | otherwise    = error "unification fails: var & ctor || var & var"
    -- ПОСМОТРЕТЬ ЭТОТ СЛУЧАЙ ПОДРОБНЕЕ
    updateSubst' (Def var)         (UndefCtor _ _)      = error "pattern matching in assign"
    updateSubst' (UndefVar var)    (Def pat)            =
      (M.insertWith (\_ _ -> error $ var ++ " has already defined! CURSED CHECK") var pat subst, [])
    updateSubst' var1@(UndefVar _) var2@(UndefVar _)    = (subst, [Unific var1 var2])
    updateSubst' var@(UndefVar _)  ctor@(UndefCtor _ _) = (subst, [Unific var ctor])
    updateSubst' _                 _                    = error "updateSubst: impossible case"


toAssign :: [Name] -> [G X] -> [Assign]
toAssign knownVars = toAssign' . getSubst [] M.empty [] 
  where
    toAssign' :: ([Assign], M.Map Name Pat, [Undef]) -> [Assign]
    toAssign' (funcDefs, subst, []) =
      (funcDefs ++) . fmap (uncurry Assign) . M.toList . M.map Term $ subst
    toAssign' (funcDefs, subst, undefs) =
      let next@(funcDefs', subst', undefs') = doDef funcDefs subst undefs
       in if undefs == undefs' then error "there are undef vars" else toAssign' next

    -- funcs for main loop
    doDef :: [Assign] -> M.Map Name Pat -> [Undef] ->
             ([Assign], M.Map Name Pat, [Undef])
    doDef funcDefs subst []                                            = (funcDefs, subst, [])
    doDef funcDefs subst (undef@(CallFunc res funcName args) : undefs) =
      let knownVars'' = getAllKnownVars funcDefs ++ M.keys subst
       in case patsOrForest (defPatTree knownVars'' subst <$> args) of
            Left _        -> (\(fd, s, u) -> (fd, s, undef : u)) $ (doDef funcDefs subst undefs)
            Right argPats -> doDef ((Assign res (Call funcName argPats)) : funcDefs) subst undefs
    doDef funcDefs subst ((Unific patTree1 patTree2) : undefs)         =
      let knownVars'' = getAllKnownVars funcDefs ++ M.keys subst
          patTree1'   = defPatTree knownVars'' subst patTree1
          patTree2'   = defPatTree knownVars'' subst patTree2
       in case updateSubst subst patTree1' patTree2' of
            (subst', []) -> doDef funcDefs subst' undefs
            (_, undef)   -> (\(fd, s, u) -> (fd, s, undef ++ u)) $ doDef funcDefs subst undefs

    defPatTree :: [Name] -> M.Map Name Pat -> PatTree -> PatTree
    defPatTree knownVars'' subst = defPatTree'
      where
        defPatTree' :: PatTree -> PatTree
        defPatTree' def@(Def _)           = def
        defPatTree' (UndefVar v)          =
          pat2tree knownVars'' $ walk subst (Var v)
        defPatTree' (UndefCtor name pats) =
          let patForest = defPatTree' <$> pats
           in either (UndefCtor name) (Def . Ctor name) $ patsOrForest patForest
    
    -- funcs for one interation
    getAllKnownVars :: [Assign] -> [Name]
    getAllKnownVars funcDefs = knownVars ++ ((\(Assign name _) -> name) <$> funcDefs)

    getSubst :: [Assign] -> M.Map Name Pat -> [Undef] ->
                [G X] ->
                ([Assign], M.Map Name Pat, [Undef])
    getSubst funcDefs subst undefs []                               = (funcDefs, subst, undefs)
    getSubst funcDefs subst undefs ((Invoke funcName vars) : conjs)
      | (terms, V res) <- chooseDirection vars =
          let knownVars''                  = getAllKnownVars funcDefs ++ M.keys subst
              (funcDefs', subst', undefs') =
                case patsOrForest (fmap (pat2tree knownVars'' . term2pat) terms) of
                  Left argForest -> (funcDefs, subst, (CallFunc funcName res argForest) : undefs)
                  Right argPats  -> (Assign res (Call funcName argPats) : funcDefs, subst, undefs)
           in getSubst funcDefs' subst' undefs' conjs
      | otherwise = error "res in func call is not var"
    getSubst funcDefs subst undefs ((term1 :=: term2) : conjs) =
      let knownVars''         = getAllKnownVars funcDefs ++ M.keys subst
          (subst', undefPlus) = unify knownVars'' subst (term2pat term1) (term2pat term2)
       in getSubst funcDefs subst' (undefs ++ undefPlus) conjs
    getSubst _        _     _      _                           = error "impossible conj"

-----------------------------------------------------------------------------------------------------
