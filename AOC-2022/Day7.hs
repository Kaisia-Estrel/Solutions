{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
module Main
  ( main
  ) where

import           Control.Arrow                  (Arrow((&&&), first, second)
                                                )
import           Control.Lens            hiding ( uncons )
import           Control.Monad.State
import qualified Data.Map                      as M
import           Data.Tree

data File
  = Written Int
  | Directory (M.Map String File)
  deriving (Show)

data Files = Files
  { _upper  :: Maybe Files
  , _dir    :: M.Map String File
  , _lastCd :: String
  }
  deriving Show
makeLenses ''Files

file :: String -> Lens' Files (Maybe File)
file s = dir . at s

getFileSystem :: [String] -> State Files ()
getFileSystem [] = do
  lastCd' <- gets _lastCd
  gets _upper >>= \case
    Just x -> do
      prevDir <- gets _dir
      put x
      file lastCd' %= Just . maybe (Directory prevDir)
                                   (\(Directory a) -> Directory (a <> prevDir))
      getFileSystem []
    Nothing -> return ()

getFileSystem ("$ ls"                      : xs) = getFileSystem xs
getFileSystem (('d' : 'i' : 'r' : ' ' : x) : xs) = do
  dir %= M.insert x (Directory M.empty)
  getFileSystem xs
getFileSystem ("$ cd .." : xs) = do
  lastCd' <- gets _lastCd
  gets _upper >>= \case
    Just x -> do
      prevDir <- gets _dir
      put x
      file lastCd' %= Just . maybe (Directory prevDir)
                                   (\(Directory a) -> Directory (a <> prevDir))
    Nothing -> pure ()
  getFileSystem xs
getFileSystem (('$' : ' ' : 'c' : 'd' : ' ' : x) : xs) = do
  state' <- get
  gets (^. file x) >>= \case
    Just (Written   _) -> error $ "Attempt to cd into file: " ++ x
    Just (Directory d) -> put (Files (Just state') d x)
    Nothing            -> put (Files (Just state') M.empty x)
  getFileSystem xs
getFileSystem (x : xs) = do
  let [size, name] = words x
  dir %= M.insert name (Written $ read size)
  getFileSystem xs

toTree :: File -> Tree [Int]
toTree (Written   _ ) = undefined
toTree (Directory xs) = Node l (map toTree r)
 where
  (l, r) = partitionFiles $ map snd $ M.toList xs
  partitionFiles :: [File] -> ([Int], [File])
  partitionFiles []                 = ([], [])
  partitionFiles (Written   x : ys) = first (x :) $ partitionFiles ys
  partitionFiles (Directory x : ys) = second (Directory x :) $ partitionFiles ys

subTrees :: Tree a -> Tree [a]
subTrees (Node x []) = Node [x] []
subTrees (Node x ts) = Node (x : concatMap flatten ts) $ map subTrees ts

solution2 :: Tree Int -> Int
solution2 xs =
  let requiredSpace = 30000000 - (70000000 - sum (flatten xs))
  in  minimum $ filter (>= requiredSpace) $ flatten $ sum <$> subTrees xs

main :: IO ()
main =
  do
      print
    .   ((sum . filter (<= 100000) . flatten . fmap sum . subTrees) &&& solution2)
    .   fmap sum
    .   toTree
    .   Directory
    .   _dir
    .   flip execState (Files Nothing M.empty "/")
    .   getFileSystem
    .   tail
    .   lines
    =<< readFile "input.txt"
