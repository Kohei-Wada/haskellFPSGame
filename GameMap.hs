module GameMap where

import Types
import Options
import Utils


data Normal = NormalNorth | NormalWest | NormalSouth | NormalEast deriving(Show, Eq)
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
pointDistance :: Position2D -> Position2D -> Double
pointDistance (px, py) (qx, qy) =
  let dx = px - qx
      dy = py - qy 
   in sqrt $ dx * dx + dy * dy
     

-- Converts 2D map coords to 1D array coords.
mapToArrayCoords :: (Int, Int) -> Int
mapToArrayCoords coords = snd coords * (fst mapSize) + fst coords


-- Converts 1D array coords to 2D map coords.
arrayToMapCoords :: Int -> (Int, Int)
arrayToMapCoords coords = (mod coords (fst mapSize), div coords (fst mapSize))


-- Computes an intersection point of two lines.
lineIntersection :: Position2D -> Double -> Position2D -> Double -> Position2D
lineIntersection (x1, y1) angle1 (x2, y2) angle2 = (x,y)
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
moveWithCollision gmap (pX, pY) angle distance = 
    let plusX = cos angle * distance
        plusY = -1 * (sin angle * distance)
     in ( pX + if positionIsWalkable gmap (pX + plusX, pY)  then plusX else 0
        , pY + if positionIsWalkable gmap (pX , pY + plusY) then plusY else 0
        )

-- Returns map square at given coords.
mapSquareAt :: GameMap -> (Int, Int) -> MapSquare
mapSquareAt gmap coords@(cX, cY) 
  | (fst coords) < (fst mapSize) && cX >= 0 && cY < (snd mapSize) && cY >= 0 = gmap !! (mapToArrayCoords coords)
  | otherwise = SquareWall


-- Checks if given player position is valid (collisions).
positionIsWalkable :: GameMap -> Position2D -> Bool
positionIsWalkable gmap p =
  (mapSquareAt gmap (floorPair p)) == SquareEmpty
 

-- Casts a ray and returns an information (distance, normal) about a wall it hits.
castRay :: GameMap -> Position2D -> (Int, Int) -> Double -> Int ->  (Double, Normal)
castRay gmap origin square direction maxIterations =
  let squareCoords = floorPair origin
      a = angleTo02Pi direction
   in if (mapSquareAt gmap square) /= SquareEmpty || maxIterations == 0
         then (0, NormalNorth)
         else let (p, offset) = castRaySquare square origin a
                  (d, nml)  = 
                    castRay gmap p (addPairs square offset) a (maxIterations - 1)

               in ( pointDistance origin p + d
                  , if d /= 0
                      then nml
                      else
                        case offset of
                          (1 , 0) -> NormalEast
                          (0 , 1) -> NormalSouth
                          (-1, 0) -> NormalWest
                          _       -> NormalNorth
                  )


-- Casts a ray inside a single square, 
-- returns (intersection point with square bounds,next square offset)

castRaySquare :: (Int, Int) -> Position2D -> Double -> (Position2D, (Int, Int))
castRaySquare squareCoords rayPosition rayAngle =
  let (sc1, sc2) = squareCoords 
      angle      = 2 * pi - rayAngle
      boundX     = sc1 + if angle < (pi / 2) || angle > (pi + pi / 2) then 1 else 0
      boundY     = sc2 + if angle < pi then 1 else 0
      intersection1 = 
          lineIntersection rayPosition angle (fromIntegral boundX, fromIntegral sc2) (pi / 2)
      intersection2 = 
          lineIntersection rayPosition angle (fromIntegral sc1, fromIntegral boundY) 0
  
   in if (pointDistance rayPosition intersection1) <= (pointDistance rayPosition intersection2)
      then (intersection1, (if boundX == sc1 then -1 else 1, 0))
      else (intersection2, (0, if boundY == sc2 then -1 else 1))



