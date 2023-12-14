{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RankNTypes #-}

module Main where

import Control.Applicative
import Control.Monad
import Data.List
import Data.List.Split (splitOn)
import Data.Maybe (mapMaybe)

getReflectionPoint pattern =
  fmap (+ 1)
    . findIndex (uncurry isReflection)
    . init
    . tail
    $ zip (inits pattern) (tails pattern)
  where
    isReflection x y = and $ zipWith (==) (reverse x) y

getReflectionPoint2 pattern =
  fmap (+ 1)
    . findIndex (uncurry isReflection)
    . init
    . tail
    $ zip (inits pattern) (tails pattern)
  where
    isReflection x y =
      (== 1)
        . length
        . filter not
        $ zipWith (==) (concat (reverse x)) (concat y)

main :: IO ()
main = do
  patterns <- splitOn [""] . lines <$> readFile "input.txt"

  putStr "part1: "
  print
    . sum
    $ mapMaybe
      (\x -> fmap (* 100) (getReflectionPoint x) <|> getReflectionPoint (transpose x))
      patterns

  putStr "part2: "
  print
    . sum
    $ mapMaybe
      (\x -> fmap (* 100) (getReflectionPoint2 x) <|> getReflectionPoint2 (transpose x))
      patterns

