module Utils where

import Data.Fixed 
import Types
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


-- Makes the angle safe for tan function.
tanSafeAngle :: Double -> Double
tanSafeAngle angle
  | mod' angle (pi / 2) == 0.0 = angle + 0.00001
  | otherwise                  = angle


vectorAngle :: Position2D -> Double
vectorAngle vector = atan2 (-1 * (snd vector)) (fst vector)


-- Returns the result of angle1 - angle2 closest to 0.
angleDifference :: Double -> Double -> Double
angleDifference angle1 angle2 
  | difference > pi = difference - 2 * pi
  | otherwise       = difference
  where difference = angleTo02Pi (angle1 - angle2)
    

angleTo02Pi :: Double -> Double
angleTo02Pi angle = mod' angle (2 * pi)


