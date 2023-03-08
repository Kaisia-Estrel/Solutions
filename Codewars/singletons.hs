{-# LANGUAGE NoImplicitPrelude, AllowAmbiguousTypes, UndecidableInstances
             , GADTs, DataKinds, TypeFamilies, TypeOperators, RankNTypes #-}

{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Singletons where

import           Prelude hiding (drop, length, take, head, tail, zipWith
                               , replicate, map, (++))

data Vec a n where
  VNil :: Vec a 'Zero
  VCons :: a -> Vec a n -> Vec a ('Succ n)

instance Show a => Show (Vec a n) where
  show VNil = "[]"
  show (VCons x xs) = show x <> "," <> show xs
-- promoted to type level by data kinds
data Nat = Zero
         | Succ Nat

data SNat a where
  SZero :: SNat 'Zero
  SSucc :: SNat a -> SNat ('Succ a)

type family (a :: Nat) :<  (b :: Nat) :: Bool

type instance m :< 'Zero = 'False

type instance 'Zero :< 'Succ n = 'True

type instance ('Succ m) :< ('Succ n) = m :< n

type family (a :: Nat) :+:  (b :: Nat) :: Nat

type instance n :+: 'Zero = n
type instance n :+: ('Succ m) = 'Succ (n :+: m)

type family (a :: Nat) :-:  (b :: Nat) :: Nat
type instance 'Zero :-: _ = 'Zero
type instance n :-: 'Zero = n
type instance ('Succ n) :-: ('Succ m) = n :-: m

type family Min (a :: Nat) (b :: Nat) :: Nat

type instance Min _ 'Zero = 'Zero
type instance Min 'Zero _  = 'Zero
type instance Min ('Succ n) ('Succ m)  = 'Succ (Min n m)

length :: Vec a n -> SNat n
length VNil = SZero
length (VCons _ xs) = SSucc (length xs)

map :: (a -> b) -> Vec a n -> Vec b n
map _ VNil = VNil
map f (VCons x xs) = VCons (f x) (map f xs)

index :: ((a :< b) ~ 'True) => SNat a -> Vec s b -> s
index SZero (VCons x _) = x
index (SSucc n) (VCons _ xs) = index n xs

replicate :: s -> SNat a -> Vec s a
replicate _ SZero = VNil
replicate x (SSucc n) = VCons x (replicate x n)

-- Both vectors must be of equal length
zipWith :: (a -> b -> c) -> Vec a n -> Vec b n -> Vec c n
zipWith _ VNil VNil = VNil
zipWith f (VCons x xs) (VCons y ys) = VCons (f x y) (zipWith f xs ys)


(++) :: Vec a m -> Vec a n -> Vec a (n :+: m)
VNil ++ ys = ys
(VCons x xs) ++ ys = VCons x (xs ++ ys)

take :: SNat n -> Vec a m -> Vec a (Min n m)
take SZero _ = VNil
take _ VNil = VNil
take (SSucc n) (VCons x xs) = VCons x (take n xs)

drop :: SNat n -> Vec a m -> Vec a (m :-: n)
drop _ VNil = VNil
drop SZero xs = xs
drop (SSucc n) (VCons _ xs) = drop n xs

head :: Vec a ('Succ n) -> a
head (VCons x _) = x

tail :: Vec a ('Succ n) -> Vec a n
tail (VCons _ xs) = xs
