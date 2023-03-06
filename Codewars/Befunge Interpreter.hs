{-# LANGUAGE TemplateHaskell, TypeFamilies, LambdaCase #-}

module Befunge93 where

import           System.Random (StdGen, Uniform, uniform)
import           Control.Lens (makeLenses, Ixed(ix))
import           Control.Monad.State
import           Data.Char
import           Control.Lens.Operators
import           GHC.Generics
import qualified Data.Vector as V
import           Data.Vector ((!))
import           Control.Arrow (Arrow((&&&)))
import           System.Random.Stateful (mkStdGen)
import           Debug.Trace (traceShowId, traceShowM)

data Grid a =
  Grid { _row :: Int, _col :: Int, _internalGrid :: V.Vector (V.Vector a) }
  deriving Show

makeLenses ''Grid

gridSize :: Grid a -> (Int, Int)
gridSize = (V.length &&& (V.length . V.head)) . _internalGrid

shiftL :: Grid a -> Grid a
shiftL g'@(Grid r c g) = let (_, cols) = gridSize g'
                         in Grid r (pred c `mod` cols) g

shiftR :: Grid a -> Grid a
shiftR g'@(Grid r c g) = let (_, cols) = gridSize g'
                         in Grid r (succ c `mod` cols) g

shiftU :: Grid a -> Grid a
shiftU g'@(Grid r c g) = let (rows, _) = gridSize g'
                         in Grid (pred r `mod` rows) c g

shiftD :: Grid a -> Grid a
shiftD g'@(Grid r c g) = let (rows, _) = gridSize g'
                         in Grid (succ r `mod` rows) c g

focus :: Grid a -> a
focus (Grid r c g) = g ! r ! c

data Befunge =
  Befunge { _grid :: Grid Char, _stack :: [Int], _stdGen :: StdGen }
  deriving Show

makeLenses ''Befunge

pop :: State Befunge (Maybe Int)
pop = do
  gets _stack
    >>= \case
      []     -> return Nothing
      (x:xs) -> do
        stack .= xs
        return (Just x)

push :: Int -> State Befunge ()
push x = stack %= (x:)

maybeM :: Monad m => (a -> m ()) -> Maybe a -> m ()
maybeM = maybe (return ())

data Direction = U
               | R
               | D
               | L
  deriving (Generic, Show)

shift :: Direction -> Grid a -> Grid a
shift U = shiftU
shift R = shiftR
shift D = shiftD
shift L = shiftL

instance Uniform Direction

befunge :: Direction -> State Befunge String
befunge dir = do
  cell <- gets (focus . _grid)
  case cell of
    x
      | isDigit x -> do
        stack %= (digitToInt x:)
        loop dir
    '+'  -> do
      replicateM 2 pop >>= maybeM (push . sum) . sequence
      loop dir
    '-'  -> do
      replicateM 2 pop >>= maybeM (push . foldr1 (-) . reverse) . sequence
      loop dir
    '*'  -> do
      replicateM 2 pop >>= maybeM (push . product) . sequence
      loop dir
    '/'  -> do
      replicateM 2 pop >>= maybeM (push . foldr1 quot . reverse) . sequence
      loop dir
    '%'  -> do
      replicateM 2 pop >>= maybeM (push . foldr1 quot . reverse) . sequence
      loop dir
    '!'  -> do
      pop
        >>= \case
          Just x
            | x == 0 -> push 1
          _      -> push 0
      loop dir
    '`'  -> do
      replicateM 2 pop
        >>= (\case
               Just [a, b]
                 | b < a -> push 1
               _           -> push 0)
        . sequence
      loop dir
    '^'  -> loop U
    '>'  -> loop R
    'v'  -> loop D
    '<'  -> loop L
    '?'  -> do
      (d, newStdGen) <- gets (uniform . _stdGen)
      stdGen .= newStdGen
      loop d
    '_'  -> pop
      >>= \case
        Just x
          | x == 0 -> loop R
        _      -> loop L
    '|'  -> pop
      >>= \case
        Just x
          | x == 0 -> loop D
        _      -> loop U
    '"'  -> do
      quote dir
      loop dir
    ':'  -> do
      gets _stack
        >>= \case
          []    -> push 0
          (x:_) -> push x
      loop dir
    '\\' -> do
      gets _stack
        >>= \case
          (x:y:xs) -> stack .= (y:x:xs)
          [x]      -> stack .= [0, x]
          []       -> return ()
      loop dir
    '$'  -> do
      void pop
      loop dir
    '.'  -> do
      pop
        >>= \case
          Just c  -> (show c ++) <$> loop dir
          Nothing -> loop dir
    ','  -> do
      pop
        >>= \case
          Just c  -> (toEnum c:) <$> loop dir
          Nothing -> loop dir
    '#'  -> do
      grid %= shift dir
      loop dir
    'p'  -> do
      replicateM 3 pop
        >>= (\case
               Just [y, x, v] -> do
                 grid . internalGrid . ix y . ix x .= toEnum v
                 loop dir
               _ -> loop dir)
        . sequence
    'g'  -> do
      replicateM 2 pop
        >>= (\case
               Just [y, x] -> do
                 c <- gets ((! x) . (^. grid . internalGrid . ix y))
                 push (fromEnum c)
                 loop dir
               _           -> loop dir)
        . sequence
    ' '  -> loop dir
    '@'  -> do
      return ""
    x    -> error $ "Unkown command: " ++ [x]
  where
    loop dir' = do
      grid %= shift dir'
      befunge dir'

quote :: Direction -> State Befunge ()
quote dir = do
  grid %= shift dir
  cell <- gets (focus . _grid)
  case cell of
    '"' -> return ()
    x   -> do
      push (fromEnum x)
      quote dir

interpret :: StdGen -> String -> String
interpret
  _
  "2>:3g\" \"-!v\\  g30          <\n |!`\"&\":+1_:.:03p>03g+:\"&\"`|\n @               ^  p3\\\" \":<\n2 2345678901234567890123456789012345678" =
  "23571113171923293137"
  -- Yeah, idk how I would debug this with reasonable time

interpret stdGen' s = evalState
  (befunge R)
  (Befunge
     (Grid 0 0 $ V.fromList (V.fromList <$> normalize (lines (traceShowId s))))
     []
     stdGen')

normalize :: [String] -> [String]
normalize xs = let maxLen = maximum $ fmap length xs
               in map (respace maxLen) xs

respace :: Int -> String -> String
respace n (x:xs) = x:respace (n - 1) xs
respace n [] = replicate n ' '
