module Main where

import Control.Lens
import Data.Containers.ListUtils (nubOrd)
import Data.Text qualified as T
import Utils

type Coord = (Int, Int)

antinode :: Coord -> Coord -> Coord
antinode (y1, x1) (y2, x2) =
  let
    yDiff = y1 - y2
    xDiff = x1 - x2
   in
    (y1 + yDiff, x1 + xDiff)

antinodes :: Coord -> Coord -> [Coord]
antinodes (y1, x1) (y2, x2) =
  let
    yDiff = y1 - y2
    xDiff = x1 - x2
   in
    iterate (bimap (+ yDiff) (+ xDiff)) (y1, x1)

main :: IO ()
main = do
  Just fileLines <- nonEmpty . lines <$> readFileTextUTF8 "./input.txt"
  let height = length fileLines
  let width = T.length $ head fileLines
  let antennae =
        iconcatMap
          (\i -> iconcatMap (\j c -> ([((i, j), c) | c /= '.'])) . toString)
          fileLines

  let inGridRange = inRange (0, 0) (height - 1, width - 1)

  let combinations = [(fst l, fst r) | l <- antennae, r <- antennae, l /= r, snd l == snd r]
  print . length . nubOrd . filter inGridRange . map (uncurry antinode) $ combinations
  print . length . nubOrd . concatMap (takeWhile inGridRange . uncurry antinodes) $ combinations
