{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Main where

import Control.Lens (FunctorWithIndex (imap), iconcatMap)
import Data.Char (digitToInt)
import Data.Sequence (Seq ((:<|), (:|>)))
import Data.Sequence qualified as S

toFileBlocks :: [Int] -> [Maybe Int]
toFileBlocks = go 0
  where
    go fileId (x : y : xs) = replicate x (Just fileId) ++ replicate y Nothing ++ go (fileId + 1) xs
    go fileId [x] = go fileId [x, 0]
    go _ [] = []

data FileBlock
  = EmptyBlock {size :: Int}
  | FileBlock {fileId :: Int, size :: Int}
  deriving stock (Show, Generic)

toSizedFileBlocks :: [Int] -> [FileBlock]
toSizedFileBlocks = go 0
  where
    go fileId (x : y : xs) = FileBlock fileId x : EmptyBlock y : go (fileId + 1) xs
    go fileId [x] = go fileId [x, 0]
    go _ [] = []

compact :: S.Seq (Maybe Int) -> [Int]
compact S.Empty = []
compact (Just n :<| xs) = n : compact xs
compact (Nothing :<| xs) = case getFromEnd xs of
  Just (n, xs') -> n : compact xs'
  Nothing -> []
  where
    getFromEnd S.Empty = Nothing
    getFromEnd (ys :|> Nothing) = getFromEnd ys
    getFromEnd (ys :|> Just n) = Just (n, ys)

compactSized :: S.Seq FileBlock -> [FileBlock]
compactSized S.Empty = []
compactSized (x@FileBlock {} :<| xs) = x : compactSized xs
compactSized (x@EmptyBlock {size} :<| xs) = case getFromEnd xs of
  Nothing -> x : compactSized xs
  Just (y, xs') ->
    y
      : if size - y.size == 0
        then compactSized xs'
        else compactSized (EmptyBlock (size - y.size) :<| xs')
  where
    getFromEnd S.Empty = Nothing
    getFromEnd (ys :|> y@(EmptyBlock {})) = second (:|> y) <$> getFromEnd ys
    getFromEnd (ys :|> y@(FileBlock {}))
      | y.size <= size = Just (y, ys :|> EmptyBlock y.size)
      | otherwise = second (:|> y) <$> getFromEnd ys

main :: IO ()
main = do
  Just input <- viaNonEmpty init . decodeUtf8 @String <$> readFileBS "./input.txt"

  putStrLn "Part 1"
  print
    . sum
    . imap (*)
    . compact
    . S.fromList
    . toFileBlocks
    $ fmap digitToInt input

  putStrLn "Part 2"
  print
    . sum
    . iconcatMap (\i -> maybe [] \x -> [x * i])
    . concatMap
      ( \case
          FileBlock {size, fileId} -> replicate size (Just fileId)
          EmptyBlock {size} -> replicate size Nothing
      )
    . compactSized
    . S.fromList
    . toSizedFileBlocks
    $ fmap digitToInt input
