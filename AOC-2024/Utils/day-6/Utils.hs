{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

module Utils (
  ListToTuple (..),
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
  applyN,
  validate,
  iterateWhile,
  ifThenElse,
  traceIfId,
  traceIf,
) where

import Debug.RecoverRTTI (traceAnything, traceAnythingId)
import Language.Haskell.TH qualified as TH
import UtilsTuple

$(listToTupleInstance 0)
$(listToTupleInstance 1)
$(listToTupleInstance 2)
$(listToTupleInstance 3)
$(listToTupleInstance 4)
$(listToTupleInstance 5)
$(listToTupleInstance 6)

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

applyN :: (a -> a) -> Int -> a -> a
applyN _ 0 x = x
applyN f n x = applyN f (n - 1) (f x)

validate :: (a -> Bool) -> a -> Maybe a
validate f x = if f x then Just x else Nothing

iterateWhile :: (a -> Maybe a) -> a -> [a]
iterateWhile f x = x : maybe [] (iterateWhile f) (f x)

ifThenElse :: forall {p}. p -> p -> Bool -> p
ifThenElse x y b = if b then x else y

traceIf :: Bool -> b -> a -> a
traceIf cond b a = if cond then traceAnything b a else a

traceIfId :: Bool -> a -> a
traceIfId cond a = if cond then traceAnythingId a else a
