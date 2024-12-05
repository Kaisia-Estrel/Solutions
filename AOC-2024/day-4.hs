module Main where

import Control.Lens (FunctorWithIndex (..))
import Data.Map qualified as M
import Utils

adjacents :: M.Map (Int, Int) Char -> [String]
adjacents grid = concatMap getInstance (M.toList grid)
  where
    getInstance ((i, j), _) =
      let
        top = sequenceA [grid M.!? (i - n, j) | n <- [0 .. 3]]
        bottom = sequenceA [grid M.!? (i + n, j) | n <- [0 .. 3]]
        right = sequenceA [grid M.!? (i, j + n) | n <- [0 .. 3]]
        left = sequenceA [grid M.!? (i, j - n) | n <- [0 .. 3]]
        diagonalDR = sequenceA [grid M.!? (i + n, j + n) | n <- [0 .. 3]]
        diagonalUR = sequenceA [grid M.!? (i - n, j - n) | n <- [0 .. 3]]
        diagonalDL = sequenceA [grid M.!? (i + n, j - n) | n <- [0 .. 3]]
        diagonalUL = sequenceA [grid M.!? (i - n, j + n) | n <- [0 .. 3]]
       in
        catMaybes
          [ bottom
          , top
          , right
          , left
          , diagonalDR
          , diagonalUR
          , diagonalDL
          , diagonalUL
          ]

xmas :: M.Map (Int, Int) Char -> Int
xmas grid = getX (M.toList grid)
  where
    getX (((i, j), 'M') : xs) =
      if Just ("AS" :: String)
        == sequenceA [grid M.!? (i - n, j + n) | n <- [1, 2]]
        && elem
          (sequenceA [grid M.!? (i - 2 + n, j + n) | n <- [0, 1, 2]])
          ([Just "MAS", Just "SAM"] :: [Maybe String])
        then 1 + getX xs
        else getX xs
    getX (((i, j), 'S') : xs) =
      if Just ("AM" :: String)
        == sequenceA [grid M.!? (i - n, j + n) | n <- [1, 2]]
        && elem
          (sequenceA [grid M.!? (i - 2 + n, j + n) | n <- [0, 1, 2]])
          ([Just "MAS", Just "SAM"] :: [Maybe String])
        then 1 + getX xs
        else getX xs
    getX (_ : xs) = getX xs
    getX [] = 0

main :: IO ()
main = do
  input <- readFileTextUTF8 "./input.txt"
  let grid =
        M.fromList
          . concat
          . imap (\i -> imap (\j c -> ((i, j), c)) . toString)
          $ lines input
  putTextLn "Part 1:"
  print . length . filter (== "XMAS") $ adjacents grid
  putTextLn "Part 2:"
  print $ xmas grid
