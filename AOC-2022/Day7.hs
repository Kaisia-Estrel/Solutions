{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
module Main
  ( main
  ) where

import           Control.Arrow                  ( Arrow((&&&), first, second) )
import           Control.Lens            hiding ( uncons )
import           Control.Monad.State
import           Data.Char
import qualified Data.Map.Strict               as M
import           Data.Maybe                     ( fromMaybe )
import           Data.Tree

type Dir = M.Map String File
data File
  = Written Int
  | Directory Dir
  deriving (Show)

data Files = Files
  { _path :: [String]
  , _tree :: Dir
  }
  deriving Show
makeLenses ''Files

recursiveAlterF
  :: (Functor f) => (Maybe Dir -> f (Maybe Dir)) -> [String] -> Dir -> f Dir
recursiveAlterF f []       x = fmap (fromMaybe mempty) $ f $ Just x
recursiveAlterF f (k : ks) x = M.alterF f' k x
 where
  f' (Just (Written _)) = error "cd into File"
  f' (Just (Directory dir)) | null ks   = fmap (fmap Directory) <$> f $ Just dir
                            | otherwise = Just . Directory <$> recursiveAlterF f ks dir
  f' Nothing = Nothing <$ f Nothing

curDir :: Lens' Files Dir
curDir f (Files path' tree') = fmap
  (Files path')
  (recursiveAlterF (maybe (Nothing <$ f tree') (fmap Just . f)) path' tree')

fileSystem :: [String] -> State Files (M.Map String File)
fileSystem [] = gets (view tree)
fileSystem ("$ ls" : xs) = fileSystem xs
fileSystem (('d' : 'i' : 'r' : ' ' : x) : xs) = do
  curDir %= M.insert x (Directory mempty)
  fileSystem xs
fileSystem ("$ cd .." : xs) = do
  path %= init
  fileSystem xs
fileSystem (('$' : ' ' : 'c' : 'd' : ' ' : x) : xs) = do
  path %= (<> [x])
  fileSystem xs
fileSystem ((span isDigit -> (size,name)) : xs) = do
  curDir %= M.insert (tail name) (Written (read size))
  fileSystem xs

toTree :: M.Map String File -> Tree [Int]
toTree xs = Node l (map toTree r)
 where
  (l, r) = partitionFiles $ map snd $ M.toList xs
  partitionFiles :: [File] -> ([Int], [M.Map String File])
  partitionFiles []                 = ([], [])
  partitionFiles (Written   x : ys) = first (x :) $ partitionFiles ys
  partitionFiles (Directory x : ys) = second (x :) $ partitionFiles ys

subTrees :: Tree a -> Tree (Tree a)
subTrees (Node x []) = Node (Node x []) []
subTrees (Node x ts) = Node (Node x ts) $ map subTrees ts

solution2 :: [Int] -> Int
solution2 [] = 0
solution2 (x : xs) =
  let requiredSpace = 30000000 - (70000000 - x) in minimum $ filter (>= requiredSpace) xs

main :: IO ()
main =
  do
      print
    .   ((sum . filter (<= 100000)) &&& solution2)
    .   flatten
    .   fmap (sum . fmap sum)
    .   subTrees
    .   toTree
    .   flip evalState (Files [] mempty)
    .   fileSystem
    .   tail
    .   lines
    =<< readFile "input.txt"
