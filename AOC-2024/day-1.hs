{-# LANGUAGE DataKinds #-}

module Main where

import Utils

main :: IO ()
main = do
  input <- decodeUtf8 <$> readFileBS "./input.txt"
  let ids = map (mapTuple2 (readText' @Int) . fromListToTuple @2 . words) $ lines input

  putStrLn "Part 1:"
  print
    . sum
    . uncurry (zipWith (abs .*. (-)))
    . mapTuple2 sort
    $ unzip ids

  let (lIds, rIds) = unzip ids
  putStrLn "Part 2:"
  print $ sum $ map (\lId -> lId * length (filter (== lId) rIds)) lIds
