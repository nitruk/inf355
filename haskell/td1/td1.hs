module Peano where

import System.IO

data Peano = Zero | Succ(Peano) deriving (Eq, Ord, Show)

instance Num Peano where
  a + Zero = a
  a + (Succ b) = Succ(a) + b

  a * Zero = Zero
  Zero * a = Zero
  a * (Succ Zero) = a
  a * (Succ b) = (a + a) * (Succ b)

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

quottmp :: Peano -> Peano -> Peano -> Peano -> (Peano, Peano)
quottmp q r Zero b = (q, r)
quottmp q r (Succ a) b | (r == b) = quottmp (Succ q) Zero (Succ a) b
quottmp q r (Succ a) b | otherwise = quottmp q (Succ r) a b

instance Integral Peano where
  toInteger Zero = 0
  toInteger (Succ a) = 1 + (toInteger a)

  quotRem a b = quottmp 0 0 a b

instance Read Peano where
--  readsPrec p str = [(fromInteger x, oth) | (x,oth) <- (readsPrec p str)]
  readsPrec _ ('Z' : xs) = [(Zero, xs)]
  readsPrec p ('S' : xs) = (\((el, rest) : oth) -> ((Succ el, rest) : oth)) (readsPrec p xs)

type Stack = [Peano]

type Operator = Stack -> Stack
depth :: Stack -> Peano
depth [] = 0
depth (x : xs) = 1 + depth xs

pick :: Peano -> Stack -> Peano
pick _ [] = error "Not enough elements"
pick 1 (x : xs) = x
pick n (x : xs) = pick (n - 1) xs

parseOp :: String -> Operator
parseOp "+" = \ (x : (y : ys)) -> ((x + y) : ys)
parseOp "-" = \ (x : (y : ys)) -> ((x - y) : ys)
parseOp "*" = \ (x : (y : ys)) -> ((x * y) : ys)
parseOp "dup" = \ (x : xs) -> (x : (x : xs))
parseOp "swap" = \ (x : (y : ys)) -> (y : (x : ys))
parseOp "drop" = \ (x : xs) -> xs
parseOp "depth" = \ xs -> ((depth xs) : xs)
parseOp "pick" = \ (x : xs) -> ((pick x xs) : xs)
parseOp s = \ xs -> ((read s :: Peano) : xs)

eval :: Stack -> [Operator] -> Stack
eval s [] = s
eval s (x : xs) = eval (x s) xs

parse :: String -> [Operator]
parse s = fmap parseOp (words s)

repl :: Stack -> IO ()
repl stack = do
  putStr "> "
  hFlush stdout
  line <- getLine
  newstack <- return $ eval stack (parse line)
  putStrLn $ show $ reverse newstack
  repl newstack
main = repl []

