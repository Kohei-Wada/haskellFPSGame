module Utils where

import Debug.Trace


-- Functions for 3-item tuples.
fst3 (x, _, _) = x
snd3 (_, x, _) = x
thd3 (_, _, x) = x


-- Splits given list to chunks of size n.
splitChunks _ [] = []
splitChunks n list = first : (splitChunks n rest)
  where
    (first,rest) = splitAt n list


-- Fills given string with spaces to given length.
toLength :: String -> Int -> String
toLength what outputLength =
  what ++ [' ' | i <- [1.. outputLength - length(what)]]


-- Alternative version of trace for debugging.
trace2 :: a -> (a -> String) -> a
trace2 what func =
  trace (func what) what


-- Ensures given values is in given interval by clamping it.
clamp :: (Ord a) => a -> (a, a) -> a
clamp value (minimum, maximum) =
  (min maximum . max minimum) value


-- Adds two 2-item pair tuples, itemwise.
addPairs :: (Num a) => (a, a) -> (a, a) -> (a, a)
addPairs (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)


substractPairs :: Num a => (a, a) -> (a, a) -> (a, a)
substractPairs (x1, y1) (x2, y2) = (x1 - x2, y1 - y2)


-- Applies floor function to both items of a pair.
floorPair :: (RealFrac a) => (RealFrac b) => (a, b) -> (Int, Int)
floorPair couple = (floor (fst couple),floor (snd couple))


