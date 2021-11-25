module GameMap where

import Types
import Options
import Utils


data MapSquare = SquareEmpty | SquareWall deriving(Show, Eq)
type GameMap = [MapSquare]

grayscaleMap  = ['M','$','o','?','/','!',';',':','\'','.','-'] -- characters sorted by brigtness


sE = SquareEmpty                    -- short aliases
sW = SquareWall


gameMap1 :: GameMap
gameMap1 = 
  [
    sE,sE,sE,sE,sE,sE,sE,sE,sE,sE,sE,sW,sW,sE,sE,
    sE,sE,sE,sW,sE,sE,sW,sE,sE,sE,sE,sE,sE,sE,sE,
    sE,sE,sE,sW,sE,sE,sW,sE,sE,sE,sE,sE,sE,sE,sE,
    sE,sE,sE,sW,sE,sE,sW,sE,sE,sE,sE,sE,sE,sE,sE,
    sE,sE,sE,sW,sE,sE,sW,sE,sE,sE,sE,sE,sW,sE,sW,
    sE,sE,sE,sE,sE,sE,sE,sE,sE,sE,sE,sE,sE,sE,sE,
    sE,sE,sE,sE,sE,sE,sE,sE,sE,sE,sE,sE,sE,sE,sE,
    sE,sE,sE,sE,sE,sE,sE,sE,sE,sE,sE,sE,sE,sE,sE,
    sE,sE,sE,sE,sE,sE,sE,sE,sE,sE,sE,sE,sE,sW,sE,
    sE,sE,sE,sE,sE,sE,sE,sE,sE,sE,sE,sE,sE,sE,sE,
    sE,sE,sE,sE,sE,sE,sE,sE,sE,sE,sE,sE,sE,sE,sE,
    sE,sE,sE,sE,sE,sE,sE,sE,sE,sE,sE,sE,sE,sW,sW,
    sE,sE,sE,sE,sE,sE,sE,sE,sE,sE,sE,sE,sE,sW,sW,
    sE,sE,sE,sE,sE,sE,sE,sE,sE,sE,sE,sE,sE,sE,sE,
    sE,sE,sE,sE,sE,sE,sE,sE,sE,sE,sE,sE,sE,sE,sE
  ]



-- Gets distance of two points.
pointPointDistance :: Position2D -> Position2D -> Double
pointPointDistance point1 point2 =
  let dx = (fst point1) - (fst point2)
      dy = (snd point1) - (snd point2)
   in sqrt $ dx * dx + dy * dy
     

-- Converts 2D map coords to 1D array coords.
mapToArrayCoords :: (Int, Int) -> Int
mapToArrayCoords coords = snd coords * (fst mapSize) + fst coords


-- Converts 1D array coords to 2D map coords.
arrayToMapCoords :: Int -> (Int, Int)
arrayToMapCoords coords = (mod coords (fst mapSize), div coords (fst mapSize))


-- Computes an intersection point of two lines.
lineLineIntersection :: Position2D -> Double -> Position2D -> Double -> Position2D
lineLineIntersection (x1,y1) angle1 (x2,y2) angle2 = (x,y)
  where tan1 = tan (tanSafeAngle angle1)
        tan2 = tan (tanSafeAngle angle2)
        x = (y2 - tan2 * x2 - y1 + tan1 * x1) / (tan1 - tan2)
        y = if abs tan1 < abs tan2 then tan1 * x + (y1 - tan1 * x1) else tan2 * x + (y2 - tan2 * x2)

-- Maps normalized intensity to ASCII character.
intensityToChar :: Double -> Char
intensityToChar intensity = 
    let safeIndex = clamp (floor (intensity * fromIntegral (length grayscaleMap))) (0,(length grayscaleMap) - 1)
     in grayscaleMap !! safeIndex 
    

-- Returns an intensity addition (possibly negative) cause by distance.
distanceToIntensity :: Double -> Double
distanceToIntensity distance = (min (distance / 7.0) 1.0) * (-0.3)


-- Maps worldspace distance to normalized screenspace size (caused by perspective).
distanceToSize :: Double -> Double
distanceToSize distance = 1.0 / (distance + 1.0)



moveWithCollision :: GameMap -> Position2D -> Double -> Double -> Position2D
moveWithCollision gmap positionFrom angle distance =
  let
    plusX = cos angle * distance
    plusY = -1 * (sin angle * distance)
  in
    (
      fst positionFrom + 
      if positionIsWalkable gmap (fst positionFrom + plusX, snd positionFrom)
        then plusX else 0
        ,
      snd positionFrom + 
      if positionIsWalkable gmap (fst positionFrom, snd positionFrom + plusY)
        then plusY else 0
    )

-- Returns map square at given coords.
mapSquareAt :: GameMap -> (Int, Int) -> MapSquare
mapSquareAt gmap coords 
  | (fst coords) < (fst mapSize) && (fst coords) >= 0 && (snd coords) < (snd mapSize) && (snd coords) >= 0 = gmap !! (mapToArrayCoords coords)
  | otherwise = SquareWall


-- Checks if given player position is valid (collisions).
positionIsWalkable :: GameMap -> Position2D -> Bool
positionIsWalkable gmap position =
  (mapSquareAt gmap (floorPair position)) == SquareEmpty
 

