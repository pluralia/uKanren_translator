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
import           Debug.Trace
import           Text.Printf (printf)
-----------------------------------------------------------------------------------------------------

translator :: Maybe Def -> Maybe F
translator = fmap go

strTranslator :: Maybe Def -> IO ()
strTranslator = maybe (putStrLn "Your program is not parsed") (print . go)

-----------------------------------------------------------------------------------------------------

chooseDirection :: [a] -> (a, [a])
chooseDirection vars = (last vars, init vars)

go :: Def -> F
go def@(name, vars, goal) = F name . fmap (toLine (chooseDirection vars)) . prrr $ disjsOfConjs
  where
    dnfWithFresh = toDNF goal
    disjsOfConjs = snd <$> dnfWithFresh
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

-----------------------------------------------------------------------------------------------------

toLine :: (String, [String]) -> [G X] -> Line
toLine (res, args) conjs = Line pat assign expr
  where
    (pat, restConjs) = patWalk args conjs res
    assignsExpr      = toAssign <$> restConjs
    (expr, assign)   = maybe undefined id $ extractExpr res assignsExpr


toAssign :: G X -> Assign
toAssign (Invoke name vars) =
  let (Var res, args) = chooseDirection . fmap (term2pat) $ vars
   in Assign res (Call name args)
toAssign (V var :=: t)      = Assign var (Term $ term2pat t)
toAssign (t :=: V var)      = Assign var (Term $ term2pat t)
-- fail унификации
toAssign _                  = Assign "UNDEFINED" (Term $ Var "UNDEFINED")


extractExpr :: String -> [Assign] -> Maybe (Expr, [Assign])
extractExpr _   [] = Nothing
extractExpr res (assig@(Assign var expr) : assigns)
  | res == var = Just (expr, assigns)
  | otherwise  = second (assig :) <$> extractExpr res assigns


patWalk :: [String] -> [G X] -> String -> ([Pat], [G X])
patWalk []           goals _   = ([], goals)
patWalk (arg : args) goals res = (pat :) `first` patWalk args goals' res
  where
    (pat, goals') = patWalk' goals
    
    patWalk' :: [G X] -> (Pat, [G X])
    patWalk' []                  = (Var arg, [])
    patWalk' (g@(t1 :=: t2) : gs)
      | t1 == V arg, t2 /= V res = (term2pat t2, gs)
      | t2 == V arg, t1 /= V res = (term2pat t1, gs)
      | otherwise                = (g :) `second` patWalk' gs
    patWalk' (g : gs)            = (g :) `second` patWalk' gs

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
