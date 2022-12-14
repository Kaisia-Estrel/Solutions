module Main
  ( main
  ) where

import           Algorithm.Search
import           Data.Matrix hiding (trace)
import Data.Maybe (fromJust)


locate :: Eq a => a -> Matrix a -> (Int,Int)
locate a m = head[(x,y) | x <- [1..nrows m], y <- [1..ncols m], getElem x y m == a]

main :: IO ()
main = do
  input <- fromLists . lines <$> readFile "input.txt"
  let s = locate 'S' input
  let e = locate 'E' input
  let input' = unsafeSet '{' e $ unsafeSet 'a' s input
  let neighbors f (r,c) = filter
                        (\(a,b)-> Just True == (f <$> safeGet r c input' <*> safeGet a b input'))
                        [(r+1,c),(r-1,c),(r,c+1),(r,c-1)]
  print $ (\x->x-2) $ fst $ fromJust 
    $ dijkstra (neighbors (\x y -> succ x >= y)) (\_ _ -> 1) (==e) s
  print $ (\x->x-2) $ fst $ fromJust 
    $ dijkstra (neighbors (\x y -> x <= succ y)) (\_ _ -> 1) (\(a,b) -> getElem a b input' == 'a') e
