module PC where

type ISO a b = (a -> b, b -> a)

-- See https://www.codewars.com/kata/isomorphism
symm :: ISO a b -> ISO b a
symm (ab, ba) = (ba, ab)

substL :: ISO a b -> (a -> b)
substL = fst

substR :: ISO a b -> (b -> a)
substR = snd

liftISO2 :: ISO a b -> ISO (a -> a -> a) (b -> b -> b)
liftISO2 (ab, ba) =
  (\f x y -> ab $ f (ba x) (ba y), \f x y -> ba $ f (ab x) (ab y))

-- A Natural Number is either Zero,
-- or a Successor (1 +) of Natural Number.
-- We have (+)/(*) on Natural Number, or (-) it.
-- Since Natrual Number do not have negative, forall x, 0 - x = 0.
-- We also have pow on Natrual Number
-- Since Haskell is lazy, we also have infinity
class Nat n where
  zero :: n
  successor :: n -> n
  nat :: a -> (n -> a) -> n -> a -- Pattern Matching
  iter :: a -> (a -> a) -> n -> a -- Induction
  plus, minus, mult, pow :: n -> n -> n
  inf :: n
  inf = successor inf

  divide :: n -> n -> n
  l `divide` r
    | l == 0 && r == 0 = undefined
    | l < r = 0
    | otherwise = successor $ (l `minus` r) `divide` r

  -- notice (l `divide` 0) when l is not 0 will return inf
  isoP :: ISO n Peano -- See below for the definition of Peano
  isoP = (iter zero successor, iter zero successor)

  toP :: n -> Peano
  toP = substL isoP

instance {-# OVERLAPPABLE #-}Nat n => Show n where
  show = show . toP

instance {-# OVERLAPPABLE #-}Nat n => Eq n where
  l == r = toP l == toP r

instance {-# OVERLAPPABLE #-}Nat n => Ord n where
  l `compare` r = toP l `compare` toP r

instance {-# OVERLAPPABLE #-}Nat n => Enum n where
  toEnum = fromIntegral

  fromEnum = fromEnum . toP

instance {-# OVERLAPPABLE #-}Nat n => Num n where
  abs = id

  signum = nat zero (const 1)

  fromInteger n
    | n == 0 = zero
    | n > 0 = successor $ fromInteger (n - 1)
    | otherwise = undefined

  (+) = plus

  (*) = mult

  (-) = minus

-- We can encode Natrual Number directly as Algebraic Data Type(ADT).
data Peano = O
           | S Peano
  deriving (Eq, Ord)

instance Show Peano where
  show = show . toInt
    where
      toInt :: Peano -> Int
      toInt O = 0
      toInt (S n) = 1 + toInt n

-- Remember, 0 - x = 0 for all x.
instance Nat Peano where
  zero = O

  successor = S

  nat x _ O = x
  nat _ f (S n) = f n

  iter x _ O = x
  iter x f (S n) = f (iter x f n)

  plus n O = n
  plus n (S m) = S (plus n m)

  minus (S n) (S m) = minus n m
  minus n O = n
  minus O _ = O

  mult _ O = O
  mult O _ = O
  mult n (S O) = n
  mult n (S m) = plus n (mult n m)

  pow _ O = S O
  pow n (S m) = mult n (pow n m)

-- Peano is very similar to a basic data type in Haskell - List!
-- O is like [], and S is like :: (except it lack the head part)
-- When we want to store no information, we can use (), a empty tuple
-- This is different from storing nothing (called Void in Haskell),
-- as we can create a value of () by using (),
-- but we cannot create a value of Void.
-- Notice how you can implement everything once you have isoP,
-- By converting to Peano and using Nat Peano?
-- Dont do that. You wont learn anything.
-- Try to use operation specific to list.
instance Nat [()] where
  zero = []

  successor = (():)

  nat x _ [] = x
  nat _ f (_:xs) = f xs

  iter x f (_:xs) = f (iter x f xs)
  iter x _ [] = x

  plus = (<>)

  minus xs [] = xs
  minus [] _ = []
  minus (_:xs) (_:ys) = minus xs ys

  mult xs ys = concat $ replicate (length ys) xs

  pow _ [] = [()]
  pow xs (_:ys) = mult xs (pow xs ys)


-- Instead of defining Nat from zero, sucessor (and get Peano),
-- We can define it from Pattern Matching
newtype Scott = Scott { runScott :: forall a. a -> (Scott -> a) -> a }

instance Nat Scott where
  -- Other operation on Scott numeral is sort of boring,
  -- So we implement it using operation on Peano.
  -- You shouldnt do this - I had handled all the boring case for you.
  plus = substR (liftISO2 isoP) plus

  minus = substR (liftISO2 isoP) minus

  mult = substR (liftISO2 isoP) mult

  pow = substR (liftISO2 isoP) pow

  zero = Scott const

  successor n = Scott \_ s -> s n

  nat n u (Scott f) = f n u

  iter x f (Scott n) = n x (f . iter x f)

-- Or from induction!
newtype Church = Church { runChurch :: forall a. (a -> a) -> a -> a }

churchPred (Church n) = Church $ cFst (n phi (pair (\_ x -> x) (\_ x -> x)))
  where
  phi p = pair (cSnd p) (cSucc (cSnd p))
  pair a b f = f a b
  cSucc n' f x = f (n' f x)
  cFst p = p const
  cSnd p = p (const id)

instance Nat Church where
  zero = Church \_ x -> x

  successor (Church n) = Church ((.) >>= n)

  nat x f (Church n) = n (const (f (churchPred (Church n)))) x

  iter x f (Church n) = n f x

  plus (Church n) (Church m) = Church \f x -> n f (m f x)

  minus n m = runChurch m churchPred n

  mult (Church n) (Church m) = Church (n . m)

  pow (Church n) (Church m) = Church (m n)
