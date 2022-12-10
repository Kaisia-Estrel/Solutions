module Main
  ( main
  ) where

import Data.List
import Data.Complex
import Data.Maybe

onTup f (a,b) (c,d) = (f a c, f b d)
both f (a,b) = (f a, f b)

neighbor t h = case  onTup (-) t h of (a,b) -> abs a <= 1 && abs b <= 1
follow t h = if neighbor t h then t else onTup (+) t $ both signum (onTup (-) h t)

main :: IO ()
main = do
  input <- map (\(x:+y)-> (round x,round y) )
        .  scanl1 (+)
        .  map (((0 :+ 1) ^) . fromJust . flip elemIndex "RULD")
        .  concatMap (\x -> replicate (read $ drop 2 x) (head x) )
        .  lines <$> readFile "input.txt"
  print $ length $ nub $ scanl follow (0,0) input
  print $ length $ nub $ iterate (scanl follow (0,0)) input
