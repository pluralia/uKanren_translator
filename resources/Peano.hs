module Peano where


data Peano = O | S Peano
  deriving (Show, Eq, Ord)


p2i :: Peano -> Int
p2i O     = 0
p2i (S x) = succ $ p2i x

i2p :: Int -> Peano
i2p 0 = O
i2p n = S (i2p $ pred n)


instance Enum Peano where
  toEnum   = i2p
  fromEnum = p2i

instance Num Peano where
  fromInteger = i2p . fromInteger
