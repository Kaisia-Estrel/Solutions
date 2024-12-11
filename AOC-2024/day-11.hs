module Main (main) where

import Data.IntMap qualified as I
import System.Directory.Internal.Prelude (readIO)
import Utils (applyN, readFileTextUTF8)

digits :: Int -> Int
digits n = 1 + floor (logBase @Double 10 (fromIntegral n))

split :: Int -> [Int]
split n = case divMod (digits n) 2 of
  (d, 0) -> [n `div` 10 ^ d, n `mod` 10 ^ d]
  _ -> error "Odd number of digits"

rule :: Int -> [Int]
rule n
  | n == 0 = [1]
  | even (digits n) = split n
  | otherwise = [n * 2024]

blinks :: IntMap Int -> IntMap Int
blinks =
  I.fromListWith (+)
    . concatMap (\(k, v) -> map (,v) $ rule k)
    . I.assocs

main :: IO ()
main = do
  stones <- mapM (readIO @Int . toString) . words =<< readFileTextUTF8 "./input.txt"

  putStrLn "Part 1:"
  print $ length <$> (iterate (concatMap rule) stones !!? 25)
  putStrLn "Part 2:"
  print
    . sum
    . applyN blinks 75
    . I.fromListWith (+)
    $ map (,1) stones
