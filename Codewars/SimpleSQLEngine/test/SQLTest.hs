{-# LANGUAGE ImportQualifiedPost #-}
module Main where

import SimpleSQLEngineSpec qualified as SE
import SimpleSQLEngineParserSpec qualified as SP
import Test.Hspec (hspec)

main :: IO()
main = do 
  -- hspec SE.spec
  hspec SP.spec
