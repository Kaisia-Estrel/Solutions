module Main where
import Control.Arrow
import Data.List.Split (chunksOf)
import Data.Char (isDigit)
import Control.Lens
import Data.Tuple
import Data.List (transpose)

move :: ([a] -> [a]) -> [Int] -> [[a]] -> [[a]]
move aa [q,f,t] xs =
  let (l,r) = splitAt q (xs !! (f-1))
  in xs & ix (f-1) .~ r
        & ix (t-1) %~ (aa l++)
move _ _ _ = undefined

main :: IO ()
main = do
  print
  . (\f -> f id &&& f reverse) 
      (\x -> map head . uncurry (foldr ((.) . move x) id))
  . swap
  . (map (filter (/=' ')) . transpose . map (map head . chunksOf 4 . tail)
    ***
    (reverse . map (map read . filter (all isDigit) . words) . drop 2))
  . splitAt 8
  . lines =<< readFile "input.txt"
