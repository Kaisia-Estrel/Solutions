{-# LANGUAGE ViewPatterns #-}
module Main
  ( main
  ) where
import           Control.Lens
import           Control.Monad
import           Data.List.Split
import           Data.Maybe                     ( fromMaybe )
import           Text.Read                      ( readMaybe )
import Control.Monad.State
import Data.List
import Data.Ord

data Monkey = Monkey
  { _items       :: [Int]
  , _operation   :: (Char, Maybe Int)
  , _test        :: Int
  , _conditions  :: (Int, Int)
  , _inspections :: Int
  }
  deriving Show
makeLenses ''Monkey

parse :: [String] -> Monkey
parse [startingItems, oper, divisibleBy, trueCond, falseCond] = Monkey
  { _items       = map read $ tail $ splitOneOf ":," startingItems
  , _operation   = (\(x : xs) -> (x, readMaybe xs)) $ drop 23 oper
  , _test        = read $ drop 21 divisibleBy
  , _conditions  = (read $ drop 29 trueCond, read $ drop 30 falseCond)
  , _inspections = 0
  }

oneRound :: (Int -> Int) -> [Monkey] -> [Monkey]
oneRound reducer m' = execState (oneRoundSt [0..length m'-1]) m'
  where
  toOp x = case x of '+' -> (+); _ -> (*)
  oneRoundSt :: [Int] -> State [Monkey] ()
  oneRoundSt [] = return ()
  oneRoundSt (n:ms) = do
    m <- gets (!!n)
    let (Monkey items'  (toOp -> (+*), arg) target' (condT,condF) _) = m
    forM_ items' $ \i -> do
      let i' = reducer (i +* fromMaybe i arg)
      let j = if 0 == mod i' target'  then condT else condF
      ix j . items %= (<> [i'])
      ix n . inspections += 1
      ix n . items %= tail
    oneRoundSt ms

main :: IO ()
main = do
  input <- map (parse . tail) . chunksOf 6 . filter (not . null) . lines <$> readFile "input.txt"
  print $ product $ take 2 $ sort $ map (Down . _inspections) $ iterate (oneRound (`div` 3)) input !! 20
  let reducer = flip mod $ product $ map _test input
  print $ product $ take 2 $ sort $ map (Down . _inspections) $ iterate (oneRound reducer) input !! 10000
