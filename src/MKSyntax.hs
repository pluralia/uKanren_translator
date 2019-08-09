module MKSyntax where

import Data.List
import Text.Printf

type X    = String -- Syntactic variables
type Name = String -- Names of variables/definitions

-- Terms
data Term v = V v
            | C String [Term v]
  deriving (Eq, Ord, Show)

instance Functor Term where
  fmap f (V v)    = V $ f v
  fmap f (C s ts) = C s $ map (fmap f) ts

-- Definitions
type Def = (Name, [Name], G X)

-- Goals
data G a = Term a :=: Term a
         | G a :/\: G a
         | G a :\/: G a
         | Fresh  Name (G a)
         | Invoke Name [Term a]
         | Let Def (G a)
  deriving (Eq, Ord, Show)

infix  8 :=:
infixr 7 :/\:
infixr 6 :\/:

infixr 7 &&&
infixr 6 |||
infix  8 ===

(===) :: Term a -> Term a -> G a
(===) = (:=:)

(|||) :: G a -> G a -> G a
(|||) = (:\/:)

(&&&) :: G a -> G a -> G a
(&&&) = (:/\:)

{-
type S    = Int    -- Semantic variables

def :: Name -> [Name] -> G X -> Def
def = (,,)

fresh :: [Name] -> G a -> G a
fresh xs g = foldr Fresh g xs

call :: Name -> [Term a] -> G a
call = Invoke
-}
