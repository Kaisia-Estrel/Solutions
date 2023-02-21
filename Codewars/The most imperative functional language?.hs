{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TupleSections #-}

module Main (main) where

import qualified Data.IntMap as M
import           Debug.Trace (traceShowM, traceM)
import           Data.IntMap ((!))
import           Control.Monad (when)
import Data.List (inits, tails, isPrefixOf)

newtype Mutation a =
  Mutation { runMutate :: M.IntMap Integer -> (M.IntMap Integer, a) }
  deriving Functor

instance Applicative Mutation where
  pure :: a -> Mutation a
  pure x = Mutation (, x)

  (<*>) :: Mutation (a -> b) -> Mutation a -> Mutation b
  (Mutation f) <*> (Mutation x) = Mutation
    \s -> let (s', f') = f s
              (s'', x') = x s'
          in (s'', f' x')

instance Monad Mutation where
  (Mutation x) >>= f = Mutation
    \s -> let (s', x') = x s
              (s'', x'') = runMutate (f x') s'
          in (s'', x'')

getVars :: Mutation (M.IntMap Integer)
getVars = Mutation \s -> (s, s)

getVarVal :: M.IntMap Integer -> Variable -> Integer
getVarVal _ (Lit x) = x
getVarVal s (Var h) = s ! h

updateVars :: M.IntMap Integer -> Mutation ()
updateVars m = Mutation (const (m, ()))

data Variable = Var Int
              | Lit Integer
  deriving Show

--
def :: Mutation Variable -> Integer
def m = let (s, x) = runMutate m $ M.fromList [(0, 0)]
        in case x of
             Var hash -> s ! hash
             Lit a    -> a

var :: Integer -> Mutation Variable
var v = do
  s <- getVars
  let (max_idx, _) = M.findMax s
  updateVars (M.insert (max_idx + 1) v s)
  return (Var (max_idx + 1))

lit :: Integer -> Variable
lit = Lit

while :: Variable -> (Integer -> Bool) -> Mutation () -> Mutation ()
while v cond action = do
  s <- getVars
  when (cond (getVarVal s v)) $ action >> while v cond action

forLoop :: Integer -> (Integer -> Bool) -> (Integer -> Integer) -> (Integer -> Mutation ()) -> Mutation ()
forLoop i cond incr action = do
  x <- var i
  while x cond do
    s <- getVars
    action (getVarVal s x)
    x %= incr

(%=) :: Variable -> (Integer -> Integer) -> Mutation ()
(Var hash) %= f = getVars >>= updateVars . M.adjust f hash
_ %= _ = return ()

(+=) :: Variable -> Variable -> Mutation ()
lhs += rhs = do
  s <- getVars
  lhs %= (+ getVarVal s rhs)

(-=) :: Variable -> Variable -> Mutation ()
lhs -= rhs = do
  s <- getVars
  lhs %= flip (-) (getVarVal s rhs)

(*=) :: Variable -> Variable -> Mutation ()
lhs *= rhs = do
  s <- getVars
  lhs %= (* getVarVal s rhs)

println :: Variable -> Mutation ()
println x = do
  s <- getVars
  traceShowM (getVarVal s x)

splitOn :: String -> String -> [String]
splitOn pattern str = case break (isPrefixOf pattern) (tails str) of
    (x,xs:_) -> map head x : splitOn pattern (drop (length pattern) xs)
    (x:_,[]) -> [x]
    _ -> []

printfmt :: String -> [Variable] -> Mutation ()
printfmt fmt_string vars = do
  s <- getVars
  traceM
    $ concat
    $ zipWith (<>) (splitOn "{}" fmt_string)
    $ map (show . getVarVal s) vars <> [""]

main :: IO ()
main = print
  $ def
    do
      a <- var 2
      printfmt "Init a: {}" [a]
      a += lit 1
      printfmt "a + 1: {}" [a]
      b <- var 3
      a *= b
      printfmt "a * b(currently: {}): {}" [b, a]

      while b (>0) do
        a *= lit 2
        printfmt "In iteration: " []
        println a
        b -= lit 1

      forLoop 0 (<10) (+1) \i -> printfmt "for loop: {}" [lit i]

      return a
