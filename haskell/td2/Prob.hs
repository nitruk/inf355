module Prob where

import Data.Ratio
import Data.List

newtype Prob a = Prob {probList :: [(a, Rational)]} deriving Show

sameProbability :: [a] -> Prob a
sameProbability l = Prob (map (\ x -> (x, 1 % toInteger(length l))) l)

instance Functor Prob where
  fmap f p = Prob (map (\ (x, r) -> (f x, r)) (probList p))

instance Monad Prob where
  return x = Prob [(x, 1)]
  (Prob l) >>= f = Prob [(y,r*s) | (x,r) <- l, (y,s) <- (probList (f x))]
  fail _ = Prob []

canonize :: Eq a => Prob a -> Prob a
canonize p@(Prob []) = p
canonize (Prob l@((x, r) : oth)) = let (l1,l2) = partition (\ (y,s) -> x == y) l in
  Prob ((x, sum ([s | (y,s) <- l1])) : probList (canonize (Prob l2)))

probability :: Eq a => a -> Prob a -> Rational
probability a p = let l = probList (canonize p) in
  case lookup a l of
    Nothing -> 0
    Just r -> r
  
dice = sameProbability [1,2,3,4,5,6]

double :: Prob Bool
double = do
  x <- dice
  y <- dice
  return $ x == y

pair :: Prob Int
pair = do
  x <- dice
  y <- dice
  return $ x + y

sick :: Prob Bool
sick = Prob [(True, 1%100000), (False, 99999%100000)]

positive :: Bool -> Prob Bool
positive b = Prob [(b, 999%1000), (not b, 1%1000)]

results :: Prob Bool
results = do
  s <- sick
  p <- positive s
  --return s
  if p then return s else fail "Negative"

renormalize :: Prob a -> Prob a
renormalize (Prob l) = let rtotal = sum (map snd l) in
  Prob [(x,r/rtotal) | (x,r) <- l]

