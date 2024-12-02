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
) where

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
  unwrap :: f a -> a

instance Unwrappable Maybe where
  unwrap (Just x) = x
  unwrap _ = error "Pattern Match Failure on Maybe"

readText' :: (Read a) => Text -> a
readText' = unwrap . readMaybe . toString

infixr 9 .*.

--- | Blackbird Combinator
--- (.*.) g f x y = g (f x y)
(.*.) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(.*.) = (.) . (.)
