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
import           Data.Bifunctor (first, second)
import           Data.Biapplicative (biliftA2)
import           Data.Char (ord)
import qualified Data.Map.Strict as M
import           Data.List (delete)
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
go def@(name, vars, goal) = F name . fmap (toLine (chooseDirection vars)) . toDNF . prrr $ goal
  where
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

term2expr :: Term X -> Expr
term2expr = Term . term2pat

pat2name :: Pat -> [Name]
pat2name (Var varName)        = [varName]
pat2name (Ctor ctorName pats) = pat2name `concatMap` pats

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
      | otherwise                = (g :) `second` patWalk' gs
    patWalk' (g : gs)            = (g :) `second` patWalk' gs

extractExpr :: Name -> [Assign] -> Maybe (Expr, [Assign])
extractExpr _   [] = Nothing
extractExpr res (assig@(Assign var expr) : assigns)
  | res == var = Just (expr, assigns)
  | otherwise  = second (assig :) <$> extractExpr res assigns

checkTerm :: [Name] -> Term X -> Bool
checkTerm knownVars (V var)        = var `elem` knownVars
checkTerm knownVars (C name terms) = all (checkTerm knownVars) terms

updateEnv :: ([Name], [Name]) -> Name -> ([Name], [Name])
updateEnv (knownVars, freshes) var = (var : knownVars, var `delete` freshes)

-----------------------------------------------------------------------------------------------------

toLine :: ([Name], Name) -> ([Name], [G X]) -> Line
toLine (args, res) (freshes, conjs) = Line pats assigns' expr
  where
    (pats, restConjs) = patWalk args res conjs
    knownVars         = pat2name `concatMap` pats
    assigns           = toAssign (knownVars, res : freshes) restConjs
    -- если результирующая переменная не определена, то пока просто падаем
    (expr, assigns')  = maybe undefined id $ extractExpr res assigns

toAssign :: ([Name], [Name]) -> [G X] -> [Assign]
toAssign _                        []             = []
toAssign env@(knownVars, freshes) (conj : conjs) = toAssign' conj
  where
    toAssign' :: G X -> [Assign]
    toAssign' (V var1 :=: V var2)
      | var1 `elem` freshes, var2 `elem` knownVars =
          (Assign var1 $ Term . Var $ var2) : toAssign (updateEnv env var1) conjs
      | var2 `elem` freshes, var1 `elem` knownVars =
          (Assign var2 $ Term . Var $ var1) : toAssign (updateEnv env var2) conjs
    toAssign' (V var :=: t)
      | var `elem` freshes, checkTerm knownVars t =
          (Assign var $ term2expr t) : toAssign (updateEnv env var) conjs
    toAssign' (t :=: V var)
      | var `elem` freshes, checkTerm knownVars t =
          (Assign var $ term2expr t) : toAssign (updateEnv env var) conjs
    toAssign' (Invoke name vars)
      | (args, Var res) <- chooseDirection . fmap (term2pat) $ vars, res `elem` freshes =
          (Assign res $ Call name args) : toAssign (updateEnv env res) conjs
    -- fail of unification ---> should work with func call & :=: (exclude ctor :=: ctor)
    toAssign' _ = undefined -- Assign "UNDEFINED" (Term $ Var "UNDEFINED")

-----------------------------------------------------------------------------------------------------
{-
varEncode :: X -> S
varEncode []       = 0
varEncode (x : xs) = ord x - 97 + 26 * varEncode xs

varDecode :: S -> X
varDecode n
  -- UNDEFINED
  | n < 0     = undefined
  | n < 26    = [['a'..'z'] !! n]
  | otherwise = ['a'..'z'] !! (n `mod` 26) : varDecode (n `div` 26)

codePair :: (a -> b) -> a -> (a, b)
codePair f x = (x, f x)

insertPair :: (Ord a) => (a, b) -> M.Map a b -> M.Map a b
insertPair (x, y) = M.insert x y

renameTerm :: forall a b. (Ord a, Show a) => M.Map a b -> Term a -> Either String (Term b)
renameTerm dict = renameTerm'
  where
    renameTerm' :: Term a -> Either String (Term b)
    renameTerm' (V var)            =
      maybe (Left $ "Error variable: " ++ show var) (Right . V) $ M.lookup var dict
    renameTerm' (C ctorName terms) = C ctorName <$> (sequence . fmap renameTerm' $ terms)

-- replace with number all variables in a goal
-- remove ctor `Fresh` from the goal
rename :: forall a b. (Ord a, Show a) =>
  (a -> b) -> M.Map a b -> G a -> Either String (M.Map a b, G b)
-- in case of variable name overlap value replaced
rename func dict (Fresh varName goal) = rename func (codePair func varName `insertPair` dict) goal
rename func dict goal                 = rename' goal
  where
    rename' :: G a -> Either String (M.Map a b, G b)
    -- forbidden: Ctor :=: Ctor
    rename' (C ctorName1 _ :=: C ctorName2 _) =
      Left . printf $ "Error unification: " ++ ctorName1 ++ " " ++ ctorName2
    rename' (t1 :=: t2) = do
      renamedT1 <- renameTerm dict t1
      renamedT2 <- renameTerm dict t2
      return (dict, renamedT1 :=: renamedT2)
    rename' (g1 :/\: g2) = do
      (dict1, renamedG1) <- rename func dict g1
      (dict2, renamedG2) <- rename func dict g2
      return (dict1 `M.union` dict2, renamedG1 :/\: renamedG2)
    rename' (g1 :\/: g2) = do
      (dict1, renamedG1) <- rename func dict g1
      (dict2, renamedG2) <- rename func dict g2
      return (dict1 `M.union` dict2, renamedG1 :\/: renamedG2)
    rename' (Invoke funcName terms) = (dict,) . Invoke funcName <$> (sequence . fmap (renameTerm dict) $ terms)
    rename' (Let _ _)               = Left "Let-ctor is forbidden"
-}
-----------------------------------------------------------------------------------------------------
