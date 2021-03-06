module RPN where

import Peano
import System.IO

type Stack = [Peano]

type Operator = Stack -> Stack
depth :: Stack -> Peano
depth [] = 0
depth (x : xs) = 1 + depth xs

pick :: Peano -> Stack -> Peano
pick _ [] = error "Not enough elements"
pick Zero (x : xs) = x
pick n (x : xs) = pick (n - 1) xs

parseOp :: String -> Operator
parseOp "+" = \ (x : (y : ys)) -> ((x + y) : ys)
parseOp "-" = \ (x : (y : ys)) -> ((y - x) : ys)
parseOp "*" = \ (x : (y : ys)) -> ((x * y) : ys)
parseOp "/" = \ (x : (y : ys)) -> ((div y x) : ys)
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

