module Main

import System.File.ReadWrite
import Data.String
import Data.List1
import Data.Maybe
import Data.Nat

%default total

covering
main : IO ()
main = do 
  file <- either (const [""]) lines <$> readFile "input.txt"
  printLn $ sum 
          $ take 3 
          $ sortBy (flip compare)
          $ toList
          $ map (sum . map (fromMaybe 0 . parsePositive))
          $ splitOn "" 
          $ file

