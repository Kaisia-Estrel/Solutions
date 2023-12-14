module Main where

import Data.List (transpose)
import Data.Map.Strict qualified as M
import Debug.Trace (traceShowId, traceShow)

dropStones :: [Char] -> [Char]
dropStones [] = []
dropStones ('O' : xs) = 'O' : dropStones xs
dropStones ('#' : xs) = '#' : dropStones xs
dropStones ('.' : xs) = case span (== '.') ('.' : xs) of
  (l, '#' : xs') -> l <> "#" <> dropStones xs'
  (l, 'O' : xs') -> 'O' : dropStones (l <> xs')
  (l, r) -> l <> r
dropStones _ = undefined

cycle :: [[Char]] -> [[Char]]
cycle =
  dropEast . dropSouth . dropWest . dropNorth
  where
    dropEast = map (reverse . dropStones . reverse)
    dropSouth = transpose . map (reverse . dropStones . reverse) . transpose
    dropWest = map dropStones
    dropNorth = transpose . map dropStones . transpose

repeatF :: (Ord a) => (a -> a) -> Int -> a -> M.Map a (Either Int Int) -> a
repeatF _ n x _
  | traceShowId n >= 1_000_000_000 = x
repeatF f n x loops = traceShow (map snd $ M.toList loops) $ case loops M.!? x of
  Just (Left n') -> repeatF f (n + 1) (f x) (M.update (\_ -> Just (Right (n - n'))) x loops)
  Just (Right n') -> repeatF f (n + n') (f x) loops
  Nothing -> repeatF f (n + 1) (f x) (M.insert x (Left n) loops)

main :: IO ()
main = do
  grid <- lines <$> readFile "input.txt"
  putStrLn "Part 1:"
  let dropped = transpose $ map dropStones $ transpose grid
      getLoad multiplier line = multiplier * length (filter (== 'O') line)
  print $ sum $ zipWith getLoad [length dropped, length dropped - 1 .. 1] dropped

