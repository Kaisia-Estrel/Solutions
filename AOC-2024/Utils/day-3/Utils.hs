{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

module Utils (
  FromListToTuple (..),
  Unwrappable (..),
  readText',
  mapTuple2,
  mapTuple3,
  mapTuple4,
  mapTuple5,
  mapTuple6,
  (.*.),
  readFileTextUTF8,
  prefixPat,
) where

import Language.Haskell.TH qualified as TH
import UtilsTuple (FromListToTuple (..), fromListToTupleInstance, mapTupleInstance)

$(fromListToTupleInstance 0)
$(fromListToTupleInstance 1)
$(fromListToTupleInstance 2)
$(fromListToTupleInstance 3)
$(fromListToTupleInstance 4)
$(fromListToTupleInstance 5)
$(fromListToTupleInstance 6)

$(mapTupleInstance 2)
$(mapTupleInstance 3)
$(mapTupleInstance 4)
$(mapTupleInstance 5)
$(mapTupleInstance 6)

class Unwrappable (f :: Type -> Type) where
  expect :: Text -> f a -> a

instance Unwrappable Maybe where
  expect _ (Just x) = x
  expect s _ = error s

unwrap :: (Unwrappable f) => f a -> a
unwrap = expect "Pattern Match Failure on Maybe"

readText' :: (Read a) => Text -> a
readText' = unwrap . readMaybe . toString

readFileTextUTF8 :: FilePath -> IO Text
readFileTextUTF8 path = decodeUtf8 <$> readFileBS path

infixr 9 .*.

--- | Blackbird Combinator
--- (.*.) g f x y = g (f x y)
(.*.) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(.*.) = (.) . (.)

prefixPat :: String -> String -> TH.PatQ
prefixPat s name = do
  let
    name' = TH.VarP $ TH.mkName name
    s' = map (TH.LitP . TH.CharL) s
  return $ foldr (\l r -> TH.InfixP l (TH.mkName ":") r) name' s'

-- return $ foldl' (\l r -> TH.InfixP r (TH.mkName ":") l) name' (reverse s')
