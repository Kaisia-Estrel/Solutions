{-# LANGUAGE ViewPatterns #-}
module Main (main) where
import Data.List

data Direction = DR | R | UR | D | M | U | DL | L | UL deriving (Read, Show, Enum, Eq)

move :: Direction -> (Int,Int) -> (Int,Int)
move d (a,b) = [(a-x,b-y) | x <- [-1,0,1], y <- [1,0,-1]] !! fromEnum d

rotate :: Direction -> Direction
rotate d = [UL,L .. DR] !! fromEnum d

isNeighbor :: (Int,Int) -> (Int,Int) -> Bool
isNeighbor (a,b) t = t `elem` ([(a-x,b-y) | x <- [-1,0,1], y <- [1,0,-1]])

sim :: Direction -> (Int,Int) -> (Int,Int) -> (Int,Int)
sim d (move d -> x) t = if isNeighbor x t then t else move (rotate d) x

solve :: (Int,Int) -> (Int,Int) ->  [Direction] -> [(Int,Int)]
solve t _  [] = [t]
solve t h (d:xs) = let x = sim d h t in x : solve x (move d h) xs

main :: IO ()
main = do
  print
  . length
  . nub
  . solve (0,0) (0,0)
  . concatMap ( (\[a,b] -> replicate (read b) (read a))
              . words)
  . lines =<< readFile "input.txt"
