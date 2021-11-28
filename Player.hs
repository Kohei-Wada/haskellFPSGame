{-#LANGUAGE RecordWildCards #-}
module Player where

import Types
import Utils
import Options
import GameMap
import Weapon


data Player = Player 
    { _playerPos :: Position2D
    , _playerRot :: Double -- rotation in radians, CCW, 0 = facing right
    } deriving Show


initialPlayer :: Player 
initialPlayer = Player
    { _playerPos = (7.5, 8.5)
    , _playerRot = 0.0
    }


turnLeft :: Player -> Player 
turnLeft p@Player{..} = p { _playerRot = angleTo02Pi (_playerRot + rotationStep) }


turnRight :: Player -> Player 
turnRight p@Player{..} = p { _playerRot = angleTo02Pi (_playerRot - rotationStep) }



wallDistance :: Player -> GameMap -> Double
wallDistance p@Player{..} gmap = 
    fst $ castRay gmap _playerPos (floorPair _playerPos) _playerRot maxRaycastIterations 


strafePlayer :: Player -> GameMap -> Double -> Player
strafePlayer p@Player{..} gmap distance = 
  p { _playerPos = moveWithCollision gmap _playerPos (angleTo02Pi (_playerRot + pi / 2)) distance }



movePlayerForward :: Player -> GameMap -> Double -> Player 
movePlayerForward p@Player{..} gmap distance =
    p { _playerPos = moveWithCollision gmap _playerPos _playerRot distance}


--TODO
movePlayerInDirection' :: Player -> GameMap -> Double -> Double -> Player
movePlayerInDirection' p@Player{..} gmap angle distance =
  let
    plusX = cos angle * distance
    plusY = -1 * (sin angle * distance)
  in
    p {
        _playerPos =
          ( fst _playerPos + 
            if positionIsWalkable gmap (fst _playerPos + plusX, snd _playerPos)
              then plusX else 0
                ,
            snd _playerPos + 
            if positionIsWalkable gmap (fst _playerPos, snd _playerPos + plusY)
              then plusY else 0
          )
      }


