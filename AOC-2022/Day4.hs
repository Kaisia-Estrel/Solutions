{-# LANGUAGE TypeApplications #-}
module Main where
import Control.Monad (join)
import Control.Arrow
import Data.Function (on)

split :: Eq a => a -> [a] -> ([a], [a])
split x xs = tail <$> span (/=x) xs

elems1, elems2 :: (Eq a) => [a] -> [a] -> Bool
elems1 xs ys = all (`elem` ys) xs || all (`elem` xs) ys
elems2 xs ys = any (`elem` ys) xs || any (`elem` xs) ys

main :: IO ()
main = 
  print . join (***) (sum . map fromEnum)
        . unzip
        . map (uncurry elems1 &&& uncurry elems2 ) 
        . fmap (join (***) (uncurry ((\x y->[x..y]) `on` (read @Int)) . split '-') . split ',') 
        . lines =<< readFile "input.txt"
