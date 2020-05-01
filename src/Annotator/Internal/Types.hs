{-# LANGUAGE FlexibleInstances #-}
module Annotator.Internal.Types (
    Ann (..)
  , Stack (..)
  , ArgsOrder (..)
  ) where


import           Data.Bifunctor        (bimap)
import qualified Data.Map.Strict  as M
import           Data.List             (groupBy, sortBy, intercalate, permutations, intersect)
import qualified Data.Set         as S
import           Text.Printf           (printf)

import           Syntax

import           Debug.Trace

----------------------------------------------------------------------------------------------------

-- Binding-time annotation:
-- Nothing ~ Undefined
-- Just x ~ x
type Ann = Maybe Word

----------------------------------------------------------------------------------------------------

type Stack = M.Map Name (S.Set ArgsOrder)

----------------------------------------------------------------------------------------------------

data ArgsOrder = ArgsOrder [Ann] [[G (S, Ann)]] [S]


instance Eq ArgsOrder where
  argsOrder1 == argsOrder2 = argsOrder1 <= argsOrder2 || argsOrder2 <= argsOrder1


instance Ord ArgsOrder where
  (ArgsOrder anns1 _ _) <= (ArgsOrder anns2 _ _) = all (\(x, y) -> x <= y) $ zip anns1 anns2


instance Show ArgsOrder where
  show (ArgsOrder anns goal vars) =
       "\n"
    ++ "( " ++ intercalate "   |   " (printVarsAnns vars anns) ++ " )\n"
    ++ printGoal goal
    ++ "\n"
    where
      printVarsAnns :: [S] -> [Ann] -> [String]
      printVarsAnns vars = zipWith (\s x -> show s ++ ": " ++ (maybe "undef" show $ x)) vars

      printGoal :: [[G (S, Ann)]] -> String
      printGoal = show . foldl1 (|||) . fmap (foldl1 (&&&))

----------------------------------------------------------------------------------------------------

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
