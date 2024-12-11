module Main where

import Text.Megaparsec
import Text.Megaparsec.Char (char)
import Text.Megaparsec.Char.Lexer qualified as L
import Utils

inputP :: Parser (Int, [Int])
inputP = do
  result <- L.decimal
  _ <- lexeme (char ':')
  nums <- Text.Megaparsec.some (lexeme L.decimal)
  return (result, nums)

digits :: Int -> Int
digits = (+ 1) . floor @Double . logBase 10 . fromIntegral

append :: Int -> Int -> Int
append n m = (n * 10 ^ digits m) + m

resultable1 :: [Int] -> Int -> Int -> Bool
resultable1 [] n target = n == target
resultable1 _ n target | n > target = False
resultable1 (x : xs) n target =
  resultable1 xs (n + x) target
    || resultable1 xs (n * x) target

resultable2 :: [Int] -> Int -> Int -> Bool
resultable2 [] n target = n == target
resultable2 _ n target | n > target = False
resultable2 (x : xs) n target =
  resultable2 xs (n + x) target
    || resultable2 xs (n * x) target
    || resultable2 xs (n `append` x) target

main :: IO ()
main = do
  inputs <-
    mapM (either (fail . errorBundlePretty) pure . parse inputP "")
      . lines
      =<< readFileTextUTF8 "./input.txt"
  print
    . sum
    . map fst
    $ filter (\(target, x : xs) -> resultable1 xs x target) inputs
  print
    . sum
    . map fst
    $ filter (\(target, x : xs) -> resultable2 xs x target) inputs
