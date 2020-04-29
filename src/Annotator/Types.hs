{-# LANGUAGE FlexibleInstances #-}
module Annotator.Types (
    AnnDef (..)
  , Conj (..)
  , PreAnn (..)
  , Ann (..)
  , Stack (..)
  , ArgsOrder (..)
  ) where

import qualified Data.Map.Strict  as M
import           Data.List             (groupBy, sortBy, intercalate, permutations, intersect)
import qualified Data.Set         as S
import           Text.Printf           (printf)

import           Syntax

----------------------------------------------------------------------------------------------------

data AnnDef = AnnDef Name [(S, Word)] [[Conj]]
  deriving Show


data Conj = U (Term (S, Word)) (Term (S, Word))
          | I Name [(S, Word)]
  deriving Show

----------------------------------------------------------------------------------------------------

data PreAnn = In
  deriving (Show, Eq)

-- Binding-time annotation:
-- Nothing ~ Undefined
-- Just x ~ x
type Ann = Maybe Word

----------------------------------------------------------------------------------------------------

type Stack = M.Map Name (S.Set ArgsOrder)

----------------------------------------------------------------------------------------------------

annToMask :: [Ann] -> [[S]]
annToMask = fmap (fmap fst)
          . groupBy (\(_, ann1) (_, ann2) -> ann1 == ann2)
          . sortBy (\(_, ann1) (_, ann2) -> compare ann1 ann2)
          . zip [0..]


data ArgsOrder = ArgsOrder [Ann] [[G (S, Ann)]] [S]


instance Eq ArgsOrder where
  (ArgsOrder anns1 _ _) == (ArgsOrder anns2 _ _) =
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
  (ArgsOrder anns1 _ _) <= (ArgsOrder anns2 _ _) = annToMask anns1 <= annToMask anns2


instance Show ArgsOrder where
  show (ArgsOrder anns goal vars) =
       "\n"
    ++ "( " ++ intercalate "   |   " (printAnns anns) ++ " )\n"
    ++ "( " ++ intercalate "   |   " (show <$> vars) ++ " )\n"
    ++ printGoal goal
    ++ "\n"
    where
      printAnns :: [Ann] -> [String]
      printAnns = fmap (\(x, ann) -> show x ++ ": " ++ maybe "undef" show ann) . zip [0..]

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
