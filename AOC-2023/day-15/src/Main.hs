{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Main where

import Control.Applicative (Alternative (some))
import Control.Lens (FunctorWithIndex (..))
import Data.Either (rights)
import Data.Foldable (Foldable (foldl', toList))
import Data.IntMap.Lazy qualified as I
import Data.List.Split (splitOn)
import Data.Sequence qualified as S
import Text.Parsec (Parsec, char, choice, digit, letter, parse)

type Parser = Parsec String ()

hash :: String -> Int
hash = foldl' (\n c -> ((fromEnum c + n) * 17) `mod` 256) 0

data OperationType = Assign Int | Remove
  deriving stock (Show)

data LensOperation = LensOperation {label :: String, operation :: OperationType}

instance Show LensOperation where
  show (LensOperation label Remove) = label <> "-"
  show (LensOperation label (Assign n)) = label <> "=" <> show n

lensOperation :: Parser LensOperation
lensOperation = do
  label <- some letter
  operation <- choice [Remove <$ char '-', Assign . read <$> (char '=' *> some digit)]
  return LensOperation {label, operation}

data Lens = Lens {label :: String, focalLength :: Int}
  deriving stock (Eq)
instance Show Lens where
  show (Lens label focalLength) = "[" <> label <> " " <> show focalLength <> "]"

type Box = S.Seq Lens

getBoxes :: [LensOperation] -> I.IntMap Box
getBoxes (LensOperation lensLabel op : xs) =
  case op of
    Remove -> I.update (Just . S.filter (\x -> lensLabel /= x.label)) (hash lensLabel) $ getBoxes xs
    Assign lensFocLength -> I.insertWith appendBox (hash lensLabel) [Lens lensLabel lensFocLength] $ getBoxes xs
  where
    appendBox [newLens] existingLenses = case S.findIndexL (\x -> newLens.label == x.label) existingLenses of
      Just n -> S.update n newLens existingLenses
      Nothing -> existingLenses S.|> newLens
    appendBox _ _ = error ""
getBoxes [] = I.empty

main :: IO ()
main = do
  input <- splitOn "," . filter (/= '\n') <$> readFile "input.txt"
  putStrLn "Part 1:"
  print $ sum $ map hash input

  putStrLn "Part 2:"
  print
    . sum
    . concatMap toList
    . imap (\boxNum -> imap \j (Lens _ focalLength) -> (1+boxNum) * (1+j) * focalLength )
    . getBoxes
    . reverse
    . rights
    $ map (parse lensOperation "") input
