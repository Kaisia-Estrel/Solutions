{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Utils

muls :: String -> [(Int, Int)]
muls ('m' : 'u' : 'l' : '(' : xs) = case reads @Int xs of
  [(n, ',' : xs')] -> case reads @Int xs' of
    [(m, ')' : xs'')] -> (n, m) : muls xs''
    _ -> muls xs'
  _ -> muls xs
muls (_ : xs) = muls xs
muls [] = []

cutDonts :: String -> String
cutDonts $(prefixPat "don't()" "xs") = cutDo xs
  where
    cutDo $(prefixPat "do()" "ys") = cutDonts ys
    cutDo (_ : ys) = cutDo ys
    cutDo [] = []
cutDonts (x : xs) = x : cutDonts xs
cutDonts [] = []

main :: IO ()
main = do
  input <- readFileTextUTF8 "./input.txt"
  putTextLn "Part 1:"
  print $ sum $ map (uncurry (*)) $ muls $ toString input
  putTextLn "Part 2:"
  print $ sum $ map (uncurry (*)) $ muls $ cutDonts $ toString input
