{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}

import Control.Lens
import Control.Parallel.Strategies (parMap, r0, rpar, rseq)
import Data.Containers.ListUtils (nubOrd)
import Data.Map ((!?))
import Data.Map qualified as M
import Data.Maybe (isJust, mapMaybe)
import Data.Set qualified as S
import Data.Traversable (mapAccumR)
import Debug.Trace (traceShow, traceShowId)

type Coord = (Int, Int)
type Grid = M.Map Coord Char

neighbors :: Coord -> [Coord]
neighbors (x, y) = [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]

data Direction = U | D | L | R
  deriving stock (Show, Ord, Eq, Enum)

move :: Direction -> Coord -> Coord
move U (row, col) = (row - 1, col)
move D (row, col) = (row + 1, col)
move L (row, col) = (row, col - 1)
move R (row, col) = (row, col + 1)

data Beam = Beam {location :: Coord, pointing :: Direction}
  deriving stock (Show, Eq, Ord)

energized :: Coord -> Grid -> S.Set Beam
energized start grid = energized' S.empty [Beam start R]
  where
    step (Beam loc' pointing') =
      let beam = Beam
       in case grid !? loc' of
            Just '|' -> case pointing' of
              U -> [beam (move pointing' loc') pointing']
              D -> [beam (move pointing' loc') pointing']
              L -> [beam (move U loc') U, beam (move D loc') D]
              R -> [beam (move U loc') U, beam (move D loc') D]
            Just '-' -> case pointing' of
              L -> [beam (move pointing' loc') pointing']
              R -> [beam (move pointing' loc') pointing']
              U -> [beam (move L loc') L, beam (move R loc') R]
              D -> [beam (move L loc') L, beam (move R loc') R]
            Just '/' -> case pointing' of
              L -> [beam (move D loc') D]
              R -> [beam (move U loc') U]
              U -> [beam (move R loc') R]
              D -> [beam (move L loc') L]
            Just '\\' -> case pointing' of
              L -> [beam (move U loc') U]
              R -> [beam (move D loc') D]
              U -> [beam (move L loc') L]
              D -> [beam (move R loc') R]
            Just '.' -> [beam (move pointing' loc') pointing']
            _ -> []

    validateBeam visited (Beam loc' pointing') = isJust (grid !? loc') && S.notMember (Beam loc' pointing') visited

    energized' :: S.Set Beam -> [Beam] -> S.Set Beam
    energized' _ [] = S.empty
    energized' visited beams =
      let validBeams = filter (validateBeam visited) beams
          visited' = foldr S.insert visited validBeams
       in S.union (S.fromList validBeams) $ energized' visited' (concatMap step validBeams)

main :: IO ()
main = do
  grid <- M.fromList . iconcatMap (\i -> imap \j c -> ((i, j), c)) . lines <$> readFile "input.txt"
  putStrLn "Part 1:"
  print . S.size $ S.map location $ energized (0, 0) grid
