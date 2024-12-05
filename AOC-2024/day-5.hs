{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Main where

import Data.List ((!!))
import Data.Map qualified as M
import Data.Text qualified as T
import Utils

main :: IO ()
main = do
  input <- lines <$> readFileTextUTF8 "./input.txt"
  (orderingRaw, _ : updatesRaw) <- pure $ span (T.elem '|') input
  let ordering =
        foldr
          ( (\(l, r) -> M.insertWith (++) l [r])
              . mapTuple2 (readText' @Int)
              . listToTuple @2
              . T.split (== '|')
          )
          M.empty
          orderingRaw

  let updates = map (map (readText' @Int) . T.split (== ',')) updatesRaw

  let ordered xs =
        not
          $ any
            ( \case
                (x : xs) -> maybe False (any (`elem` xs)) $ ordering M.!? x
                _ -> False
            )
          . tails
          $ reverse xs
  let midpoint xs = xs !! (length xs `div` 2)

  let order l r = case ordering M.!? l of
        Just xs | r `elem` xs -> LT
        _ -> case ordering M.!? r of
          Just xs | l `elem` xs -> GT
          _ -> EQ

  putTextLn "Part 1:"
  print $ sum $ map midpoint $ filter ordered updates
  putTextLn "Part 2:"
  print $ sum $ map (midpoint . sortBy order) $ filter (not . ordered) updates
