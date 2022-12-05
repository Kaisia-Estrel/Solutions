{-# LANGUAGE LambdaCase #-}
module Main where

import Control.Arrow
import Control.Monad 

split :: Eq a => a -> [a] -> ([a], [a])
split x xs = tail <$> span (/=x) xs

data RPS = A|B|C deriving (Eq,Show,Read,Enum)
instance Ord RPS where
  compare A B = GT
  compare B C = GT
  compare C A = GT
  compare x y | x == y = EQ 
              | otherwise = LT

compare' :: (RPS,RPS) -> RPS
compare' (x,y)  = head $ filter ((==(toEnum $ fromEnum y)) . compare x) [A,B,C]

main :: IO ()
main = do
  print 
    . join (***) sum
    . unzip
    . map ( join (***) 
                 (liftM2 (+) (succ . fromEnum . snd) 
                             ((*3) . fromEnum . uncurry compare))
          . (id &&& (liftM2 (,) fst compare'))
          . join (***) ((\case 'X'->A;'Y'->B;'Z'->C;x->read [x]) 
                       . head)
          . split ' ')
    . lines =<< readFile "input.txt"
