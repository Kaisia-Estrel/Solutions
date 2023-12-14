{-# LANGUAGE TypeApplications #-}

module Main where

import Data.List (group)
import Data.List.Split (splitOn)

possibleStates :: String -> [String]
possibleStates ('?' : xs) = map ('#' :) (possibleStates xs) ++ map ('.' :) (possibleStates xs)
possibleStates (x : xs) = map (x :) (possibleStates xs)
possibleStates [] = [[]]

arrangements :: [String] -> Int
arrangements [conditions, brokenGroupsStr] =
  let brokenGroups = map (read @Int) . splitOn "," $ brokenGroupsStr
      states = map (map length . filter (all (== '#')) . group) $ possibleStates conditions
   in length $ filter (== brokenGroups) states
arrangements _ = undefined

data WaveFunction a
  = Superposition [a]
  | Collapsed a
  deriving (Show)

waveCollapse [conditionsStr, brokenGroupsStr] =
  let brokenGroups = concat . replicate 5 . map (read @Int) . splitOn "," $ brokenGroupsStr
      conditions = concat $ replicate 5 conditionsStr
   in (conditions, brokenGroups)
waveCollapse [] = undefined

main :: IO ()
main = do
  -- putStrLn "part 1:"
  -- print . sum . (map (arrangements . words) . lines) =<< readFile "input.txt"

  putStrLn "part 2:"
  mapM_ print . (map (waveCollapse . words) . lines) =<< readFile "input.txt"
