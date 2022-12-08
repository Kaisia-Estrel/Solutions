module Main (main) where
import Data.List
import Data.Char (digitToInt)

isVisible :: [[Int]] -> [[Bool]]
isVisible = map (view [])
  where
    view :: [Int] -> [Int] -> [Bool]
    view _ [] = []
    view accum (x:xs) = all (<x) accum : view (x:accum) xs

viewingDistance :: [[Int]] -> [[Int]]
viewingDistance = map (view [])
  where
    view :: [Int] -> [Int] -> [Int]
    view _ [] = []
    view accum (x:xs) 
      | all (<x) accum = length accum : view (x:accum) xs
      | otherwise = length (takeWhile (<x) accum) + 1 : view (x:accum) xs

main :: IO ()
main = do
  input <- map (map digitToInt) . lines <$> readFile "input.txt"

  print  $ sum
         $ concatMap (map fromEnum)
         $ foldr1 (zipWith $ zipWith (||))
         $ zipWith ($) [id, map reverse, transpose, transpose . map reverse]
         $ map (isVisible .) [id, map reverse, transpose, map reverse . transpose] <*> [input]

  print  $ maximum
         $ concat
         $ foldr1 (zipWith $ zipWith (*))
         $ zipWith ($) [id, map reverse, transpose, transpose . map reverse]
         $ map (viewingDistance .) [id, map reverse, transpose, map reverse . transpose] <*> [input]

