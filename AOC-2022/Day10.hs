module Main
  ( main
  ) where
import           Data.List.Split                ( chunksOf )

ins :: Int -> [String] -> [Int]
ins _ []       = []
ins n (('a' : 'd' : 'd' : 'x' : ' ' : x) : xs) = [n, n] ++ ins (n + read x) xs
ins n (_ : xs) = n : ins n xs

sol1 =
  sum . map (uncurry (*)) . filter ((`elem` (20 : [60, 100 .. 220])) . fst) . zip [1 ..]

sol2 = chunksOf 40 . zipWith
  (\d p -> if d `elem` [ p + x | x <- [-1, 0, 1] ] then '#' else '.')
  (cycle [0 .. 39])

main :: IO ()
main =
  do
      (>>) <$> (print . sol1) <*> (mapM_ putStrLn . sol2)
    .   ins 1
    .   lines
    =<< readFile "input.txt"
