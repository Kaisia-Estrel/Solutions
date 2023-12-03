{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module SimpleSQLEngineParserSpec where

import Data.Void (Void)
import PyF (fmt)
import SimpleSQLEngine.Kata
import Test.Hspec (Expectation, SpecWith, describe, it)
import Test.Hspec.Megaparsec (succeedsLeaving)
import Test.Hspec.Megaparsec qualified as T
import Text.Megaparsec (ParseErrorBundle, State, runParser, runParser')
import Test.Hspec.QuickCheck
import Data.Char (isLetter, isAlphaNum)
import Test.QuickCheck ((==>), withMaxSuccess)

shouldParse :: (Eq a, Show a) => Parser a -> String -> a -> Expectation
shouldParse parser str = T.shouldParse (runParser parser "HSPEC" str)

shouldSucceedOn :: (Show a, Eq a) => Parser a -> String -> Expectation
shouldSucceedOn parser = T.shouldSucceedOn (runParser parser "HSPEC")

shouldFailOn :: (Show a, Eq a) => Parser a -> String -> Expectation
shouldFailOn parser = T.shouldFailOn (runParser parser "HSPEC")

parsing' :: Parser a -> String -> (State String Void, Either (ParseErrorBundle String Void) a)
parsing' p s = runParser' p (T.initialState s)

parsing :: Parser a -> String -> Either (ParseErrorBundle String Void) a
parsing = (snd .) . parsing'

spec :: SpecWith ()
spec = do
  describe "tableName" do 
    prop "Should parse regular identifier" \(x :: Char) (xs :: String) -> 
      let validChar c = isLetter c || c `elem` "@#_"  in
      withMaxSuccess 10 $
      validChar x ==>
      all (\c -> validChar c || isAlphaNum c) xs ==>
      identifier `shouldSucceedOn` (x:xs)
    prop "Should parse delimited identifier" \(s :: String) -> do
      identifier `shouldSucceedOn` [fmt|"{s}"|]
      identifier `shouldSucceedOn` [fmt|"{(2 + 4)}"|]
    it "Should parse with valid residue" do
      identifier `parsing'` "foo" `succeedsLeaving` ""
      identifier `parsing'` "foo " `succeedsLeaving` " "
      identifier `parsing'` "foo," `succeedsLeaving` ","
    it "Should not parse" do
      identifier `shouldFailOn` ""
      identifier `shouldFailOn` "12foo"
      identifier `shouldFailOn` "12foo"
