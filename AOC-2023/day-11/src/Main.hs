{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NumericUnderscores #-}

module Main where

import Control.Lens
import Data.Containers.ListUtils (nubOrd)
import Data.List

expand :: [String] -> [String]
expand = concatMap \x -> if all (== '.') x then [x, x] else [x]

expand2 :: [String] -> [Integer]
expand2 = init . scanl (+) 0 . map \x -> if all (== '.') x then 1_000_000 else 1

manhattanDistance :: (Num a) => (a, a) -> (a, a) -> a
manhattanDistance (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

main :: IO ()
main = do
  space <- lines <$> readFile "input.txt"
  let expandedSpace = transpose . expand . transpose . expand $ space
  let galaxyCoords = iconcatMap (\i -> iconcatMap \j x -> ([(i, j) | x == '#'])) expandedSpace

  print $ sum . map (uncurry manhattanDistance) . nubOrd $ [(min x y, max x y) | x <- galaxyCoords, y <- galaxyCoords, x /= y]

  rows <- expand2 . lines <$> readFile "input.txt"
  columns <- expand2 . transpose . lines <$> readFile "input.txt"
  let galaxyCoords2 = concatMap concat (zipWith (\i -> zipWith (\j x -> [(i, j) | x == '#']) columns) rows space)
  print galaxyCoords
  print galaxyCoords2
  print $ sum . map (uncurry manhattanDistance) . nubOrd $ [(min x y, max x y) | x <- galaxyCoords2, y <- galaxyCoords2, x /= y]
