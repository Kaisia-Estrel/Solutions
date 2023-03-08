{-# LANGUAGE FunctionalDependencies, FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE GADTs #-}

module PolyvariadicFunctions where

class BuildList a r  | r -> a where
    polyList' :: [a] -> a -> r

instance BuildList Int Int where
   polyList' l y = sum l + y
instance BuildList a [a] where
    polyList' l x = tail $ reverse $ x:l
instance BuildList a r => BuildList a (a->r) where
    polyList' l x = polyList' (x:l)

polyAdd :: forall r . (BuildList Int r) => r
polyAdd = polyList' [] (0 :: Int)

polyList :: forall r a. (BuildList a r) => r
polyList = polyList' [] undefined

class BuildStr a r | r -> a where
  polyWord' :: [a] -> a -> r
instance BuildStr String String where
  polyWord' l x = unwords $ tail $ reverse (x:l)
instance BuildStr a r => BuildStr a (a->r) where
  polyWord' l x = polyWord' (x:l)

polyWords :: (BuildStr String r) => r
polyWords = polyWord' [] ""
