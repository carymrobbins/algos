{-# LANGUAGE LambdaCase #-}
module BinarySearch where

import Data.Char
import Data.Maybe
import Data.Vector (Vector)
import qualified Data.Vector as V
import System.IO

class BinarySearchable f where
  binarySearchLength :: f a -> Int
  binarySearchIndex :: f a -> Int -> a

  binarySearch :: f a -> (a -> Ordering) -> Maybe (Int, a)
  binarySearch xs p = loop 0 len
    where
    len = binarySearchLength xs
    loop min max
      | min > max = Nothing
      | otherwise =
          case p x of
            EQ -> Just (i, x)
            LT -> loop min (i - 1)
            GT -> loop (i + 1) max
          where
          i = (min + max) `div` 2
          x = xs `binarySearchIndex` i

  binarySearchM
    :: Monad m
    => f a -> (a -> m Ordering) -> m (Maybe (Int, a))
  binarySearchM xs mp = loop 0 len
    where
    len = binarySearchLength xs
    loop min max
      | min > max = return Nothing
      | otherwise =
          mp x >>= \case
            EQ -> return $ Just (i, x)
            LT -> loop min (i - 1)
            GT -> loop (i + 1) max
          where
          i = (min + max) `div` 2
          x = xs `binarySearchIndex` i

instance BinarySearchable Vector where
  binarySearchLength = V.length
  binarySearchIndex = V.unsafeIndex

data Range a = Range
  { rangeMin :: a
  , rangeMax :: a
  , rangeLength :: Int
  , rangeIndex :: Int -> a
  }

zeroBasedIntRange :: Int -> Range Int
zeroBasedIntRange max = Range
  { rangeMin = 0
  , rangeMax = max
  , rangeLength = max
  , rangeIndex = id
  }

instance BinarySearchable Range where
  binarySearchLength = rangeLength
  binarySearchIndex = rangeIndex

guessingGame :: Int -> IO ()
guessingGame max = do
  putStrLn $ "Think of a number between 1 and " ++ show max
  solution <- binarySearchM (zeroBasedIntRange max) interact
  putStrLn $ "Solution: " ++ show (fmap fst solution)

  where

  interact x = do
    putStrLn $ "Is it " ++ show x ++ "?"
    readResponse

  readResponse = do
    putStr "Correct, higher, or lower? [c/h/l] "
    hFlush stdout
    (parseResponse <$> getLine) >>= \case
      Just o -> return o
      Nothing -> do
        putStrLn "Sorry, I didn't understand"
        readResponse

  parseResponse s = case s of
    [] -> Nothing
    c:_ -> case toLower c of
      'c' -> Just EQ
      'h' -> Just GT
      'l' -> Just LT
      _ -> Nothing
