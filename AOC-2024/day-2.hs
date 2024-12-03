{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NegativeLiterals #-}

module Main where

import Data.List.Split (divvy)
import Utils

isSafe :: [Int] -> Bool
isSafe xs =
  let
    xs' = map signum xs
   in
    (all (== 1) xs' || all (== -1) xs') && all (\x -> abs x <= 3 && abs x >= 1) xs

cut1s :: [a] -> [[a]]
cut1s xs =
  zipWith (\i -> map snd . filter ((/= i) . fst)) [(0 :: Int) ..]
    . replicate (length xs)
    $ zip [0 ..] xs

main :: IO ()
main = do
  levels <- map (map (readText' @Int) . words) . lines <$> readFileTextUTF8 "./input.txt"

  let differences = map (map (uncurry subtract . fromListToTuple @2) . divvy 2 1) levels

  putStrLn "Part 1:"
  print . length $ filter isSafe differences

  let differences' = map (map (map (uncurry subtract . fromListToTuple @2) . divvy 2 1) . cut1s) levels
  putStrLn "Part 2:"
  print . length $ filter (any isSafe) differences'
