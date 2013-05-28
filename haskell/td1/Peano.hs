module Peano where

data Peano = Zero | Succ(Peano) deriving (Eq, Ord)

instance Show Peano where
  show Zero = "Z"
  show (Succ a) = ('S' : show a)

instance Num Peano where
  a + Zero = a
  a + (Succ b) = Succ(a) + b

  a - Zero = a
  (Succ a) - (Succ b) = a - b

  a * Zero = Zero
  a * (Succ b) = a + (a * b)

  abs a = a

  signum Zero = 0
  signum a = 1

  fromInteger 0 = Zero
  fromInteger a = Succ (fromInteger (a - 1))

instance Real Peano where
  toRational Zero = 0
  toRational (Succ x) = 1 + (toRational x)

instance Enum Peano where
  toEnum 0 = Zero
  toEnum n = Succ (toEnum (n - 1))

  fromEnum Zero = 0
  fromEnum (Succ x) = 1 + (fromEnum x)

quottmp :: Peano -> Peano -> Peano -> (Peano, Peano)
quottmp q r b | (r < b) = (q, r)
quottmp q r b | otherwise = quottmp (Succ q) (r - b) b

instance Integral Peano where
  toInteger Zero = 0
  toInteger (Succ a) = 1 + (toInteger a)

  quotRem a b = quottmp 0 a b

  div a b = (\(q,r) -> q) (quotRem a b)

instance Read Peano where
--  readsPrec p str = [(fromInteger x, oth) | (x,oth) <- (readsPrec p str)]
  readsPrec _ ('Z' : xs) = [(Zero, xs)]
  readsPrec p ('S' : xs) = (\((el, rest) : oth) -> ((Succ el, rest) : oth)) (readsPrec p xs)

