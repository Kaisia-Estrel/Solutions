{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use next" #-}
module Main (main) where

import Control.Lens (FunctorWithIndex (imap), iconcatMap)
import Data.Containers.ListUtils (nubOrd)
import Data.Graph (Tree (Node))
import Data.Map qualified as M
import Data.Maybe (fromJust)
import Data.Tree qualified as T
import Utils

type Terrain = M.Map Coord Char

trail :: Terrain -> Coord -> T.Tree Coord
trail terrain (x, y) = Node (x, y) $ map (trail terrain) validSteps
  where
    c = fromMaybe (error "Coord outside terrain") $ terrain M.!? (x, y)
    validStep coord = case terrain M.!? coord of
      Just c' -> succ c == c'
      Nothing -> False
    validSteps = filter validStep [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]

main :: IO ()
main = do
  terrain <-
    M.fromList
      . iconcatMap (\i -> imap (\j c -> ((j, i), c)) . toString)
      . lines
      <$> readFileTextUTF8 "input.txt"

  putStrLn "Part 1"
  print
    . sum
    . map
      ( length
          . nubOrd
          . filter (\c -> Just '9' == (terrain M.!? c))
          . T.flatten
          . trail terrain
          . fst
      )
    . filter ((== '0') . snd)
    $ M.toList terrain

  let rating = length . filter ((Just '9' ==) . (terrain M.!?)) . T.flatten

  putStrLn "Part 2"
  print
    . sum
    . map (rating . trail terrain . fst)
    . filter ((== '0') . snd)
    $ M.toList terrain
