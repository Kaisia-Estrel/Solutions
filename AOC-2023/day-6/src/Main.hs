{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

solve1 :: [(Int, Int)] -> Int
solve1 = product . map (uncurry winningPlays)

winningPlaysLarge :: Int -> Int -> Int
winningPlaysLarge (fromIntegral -> (time :: Double)) (fromIntegral -> record) =
  let
    startX = ceiling $ (-time + sqrt (time ** 2 - (-4) * (-record))) / (-2)
    endX = floor $ (-time - sqrt (time ** 2 - (-4) * (-record))) / (-2)
   in endX - startX + 1

winningPlays :: Int -> Int -> Int
winningPlays time record 
  | time < 2000 = length $ filter (>record) [x * (time - x) | x <- [0..time] ]
  | otherwise = winningPlaysLarge time record

main :: IO ()
main = do
  [timeStr, distStr] <- lines <$> readFile "input.txt"

  putStrLn "Part1: "
  print
    . solve1
    $ zip
      (map (read @Int) . tail $ words timeStr)
      (map (read @Int) . tail $ words distStr)

  putStrLn "Part2: "
  print
    $ winningPlays
      ((read @Int) . concat $ tail $ words timeStr)
      ((read @Int) . concat $ tail $ words distStr)
