module Main where
import Data.List
windows :: Int -> [a] -> [[a]]
windows _ [] = []
windows n xs | length xs < n = []
windows n xs = take n xs : windows n (tail xs)

main :: IO ()
main = do 
  print . (+4) . length . takeWhile ((/=) <*> nub) . windows 4 =<< readFile "input.txt"
  print . (+14) . length . takeWhile ((/=) <*> nub) . windows 14 =<< readFile "input.txt"
