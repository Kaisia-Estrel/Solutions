{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Main (main) where

import Control.Lens
import Data.Generics.Labels ()
import Data.Set qualified as S
import Data.Text qualified as T
import GHC.Records (HasField (..))
import Utils

data Direction = U | D | L | R
  deriving stock (Show, Eq, Ord)

data Guard = Guard
  { y :: Int
  , x :: Int
  , dir :: Direction
  }
  deriving stock (Generic, Show)

type Coord = (Int, Int)

data Lab = Lab
  { guard :: Guard
  , obstacles :: S.Set Coord
  , size :: Coord
  }
  deriving stock (Generic, Show)

instance HasField "x" (a, b) b where
  getField :: (a, b) -> b
  getField (_, x) = x

instance HasField "y" (a, b) a where
  getField :: (a, b) -> a
  getField (y, _) = y

rotate :: Direction -> Direction
rotate U = R
rotate R = D
rotate D = L
rotate L = U

recordVisited :: Lab -> S.Set Coord -> (Maybe Lab, S.Set Coord)
recordVisited m visited =
  let (Guard guardY guardX guardDir) = m.guard
      (nextY, nextX) = case guardDir of
        U -> (guardY - 1, guardX)
        D -> (guardY + 1, guardX)
        L -> (guardY, guardX - 1)
        R -> (guardY, guardX + 1)
   in if S.member (nextY, nextX) m.obstacles
        then recordVisited (m & #guard . #dir %~ rotate) visited
        else
          if nextY >= 0 && nextY < m.size.y && nextX >= 0 && nextX < m.size.x
            then
              ( Just $ m & #guard .~ Guard nextY nextX guardDir
              , S.insert (nextY, nextX) visited
              )
            else (Nothing, visited)

inRange :: Coord -> Coord -> Coord -> Bool
inRange (y1, x1) (y2, x2) (y3, x3) =
  (y1 <= y3 && y3 <= y2) && x1 <= x3 && x3 <= x2

step :: Direction -> Coord -> Coord
step U = _1 -~ 1
step D = _1 +~ 1
step L = _2 -~ 1
step R = _2 +~ 1

guardedStep :: Lab -> Maybe Lab
guardedStep m = do
  newCoord <- validate (inRange (0, 0) m.size) $ step m.guard.dir m.guard.yx
  return $ m
    & #guard . #y .~ newCoord.y
    & #guard . #x .~ newCoord.x

guardSteps :: Lab -> S.Set Coord -> S.Set Coord
guardSteps m visited = case recordVisited m visited of
  (Just m', visited') -> guardSteps m' visited'
  (Nothing, visited') -> visited'

instance HasField "yx" Guard Coord where
  getField guard = (guard.y, guard.x)

walk :: Lab -> Maybe Lab
walk m = do
  m' <- guardedStep m
  let (nextY, nextX) = m'.guard.yx
  if S.member (nextY, nextX) m.obstacles
    then Just (m & #guard . #dir %~ rotate)
    else walk m'

isLooping :: S.Set (Coord, Direction) -> Lab -> Bool
isLooping visited m | S.member (m.guard.yx, m.guard.dir) visited = True
isLooping visited m = case walk m of
  Just m' -> isLooping (S.insert (m.guard.yx, m.guard.dir) visited) m'
  Nothing -> False

main :: IO ()
main = do
  input <- lines <$> readFileTextUTF8 "./input.txt"
  let labHeight = length input
  Just labWidth <- pure $ T.length <$> viaNonEmpty head input
  let lab =
        ifoldr
          ( \i cs acc ->
              ifoldr
                ( \cases
                    _ '.' acc' -> acc'
                    j '#' acc' -> acc' & #obstacles %~ S.insert (i, j)
                    j '^' acc' ->
                      acc' & #guard .~ Guard i j U
                    _ c _ -> error $ "Unknown Character " <> show c
                )
                acc
                (toString cs)
          )
          (Lab (error "Guard not found") S.empty (labHeight, labWidth))
          input

  let steps = guardSteps lab S.empty
  putStrLn "Part 1:"
  print $ 1 + length steps
  putStrLn "Part 2:"
  print
    . length
    . filter (\c -> isLooping S.empty (lab & #obstacles %~ S.insert c))
    $ S.toList steps
