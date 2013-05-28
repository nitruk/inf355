module CheckRPN where

import Control.Monad
import Peano
import RPN
import Data.List
import Test.QuickCheck

instance Arbitrary Peano where
  arbitrary = liftM toEnum $ choose (0, 10)
  shrink (Succ a) = [a]
  shrink Zero = []

prop_dup s = length s > 0 ==> case s of (x : _) -> parseOp "dup" s == x : s
         where types = s :: [Peano]
prop_drop s = length s > 0 ==> parseOp "drop" s == tail s
         where types = s :: [Peano]
prop_swap s = length s >= 2 ==> case s of (a : b : xs ) -> parseOp "swap" s == b : a : xs
         where types = s :: [Peano]
prop_depth s = parseOp "depth" s == (fromIntegral $ length s) : s
         where types = s :: [Peano]
prop_pick s = length s >= 2 && (fromIntegral (head s) < length s - 1) && head s >= 0 ==>
              case s of (d : xs) -> parseOp "pick" s == (genericIndex xs d) : xs
         where types = s :: [Peano]
prop_add s = length s >= 2 ==> case s of (b : a : xs) -> parseOp "+" s == (a+b) : xs
         where types = s :: [Peano]
prop_sub s = length s >= 2 && fromIntegral (s !! 1) >= fromIntegral (s !! 0) ==>
             case s of (b : a : xs) -> parseOp "-" s == (a-b) : xs
         where types = s :: [Peano]
prop_mul s = length s >= 2 ==> case s of (b : a : xs) -> parseOp "*" s == (a*b) : xs
         where types = s :: [Peano]
prop_div s = length s >= 2 && fromIntegral (s !! 0) /= 0 ==>
             case s of (b : a : xs) -> parseOp "/" s == (a `div` b) : xs
         where types = s :: [Peano]

main = do
        quickCheck prop_dup
        quickCheck prop_drop
        quickCheck prop_swap
        quickCheck prop_depth
        quickCheck prop_pick
        quickCheck prop_add
        quickCheck prop_sub
        quickCheck prop_mul
        quickCheck prop_div
