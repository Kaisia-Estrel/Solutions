{-# LANGUAGE TypeApplications #-}
module Main where

solve1 :: String -> Int
solve1 = sum . map (sum . map last . getSequences . map (read @Int) . words) . lines
  where 
  getSequences [] = []
  getSequences xs 
    | all (==0) xs = [xs]
    | otherwise = xs : getSequences (zipWith (flip (-)) xs (tail xs) )

solve2 = sum . map (sum . map head . getSequences . map (read @Int) . words) . lines
  where 
  getSequences [] = []
  getSequences xs 
    | all (==0) xs = [xs]
    | otherwise = xs : getSequences (zipWith (-) xs (tail xs) )

main :: IO ()
main = do 
  print . solve1 =<< readFile "input.txt"
  print . solve2 =<< readFile "input.txt"
