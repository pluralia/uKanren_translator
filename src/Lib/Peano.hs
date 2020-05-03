module Lib.Peano where

import Lib.Generator


data Peano = O | S Peano
  deriving (Eq, Ord)


p2i :: Peano -> Int
p2i O     = 0
p2i (S x) = succ $ p2i x

i2p :: Int -> Peano
i2p 0 = O
i2p n
 | n < 0     = O
 | otherwise = S (i2p $ pred n)


instance Show Peano where
  show = show . p2i

instance Enum Peano where
  toEnum   = i2p
  fromEnum = p2i

instance Num Peano where
  fromInteger = i2p . fromInteger
  p1 + p2     = i2p $ p2i p1 + p2i p2
  p1 * p2     = i2p $ p2i p1 * p2i p2
  p1 - p2     = i2p $ p2i p1 - p2i p2
  abs         = i2p . abs . p2i
  signum      = i2p . signum . p2i

instance Generator Peano where
  gen = i2p <$> gen
