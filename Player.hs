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
    , _weapon    :: Weapon
    , _fireCountdown :: Int       -- counter for implementing different fire rates
    } deriving Show


initialPlayer :: Player 
initialPlayer = Player
    { _playerPos = (7.5, 8.5)
    , _playerRot = 0.0
    , _weapon    = Knife
    , _fireCountdown = 0
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


movePlayerInDirection :: Player -> GameMap -> Double -> Double -> Player
movePlayerInDirection p@Player{..} gmap angle distance =
  let plusX = cos angle * distance
      plusY = -1 * (sin angle * distance)
      tmp = (fst _playerPos + plusX, snd _playerPos + plusY)
   in p { _playerPos = if positionIsWalkable gmap tmp then tmp else _playerPos }


changeWeapon :: Player -> Weapon -> Player
changeWeapon p@Player{..} w = p { _weapon = w }


updateFireCountDown :: Player -> Player 
updateFireCountDown p@Player{..} = 
    p { _fireCountdown = max ( _fireCountdown - 1) 0 }


attackRange p@Player{..} = 
    if _weapon == Knife then knifeAttackDistance else infinity


weaponSprite :: Player -> Int
weaponSprite p@Player{..} = weaponSprite' _weapon + if _fireCountdown /= 0 then 1 else 0


resetFireCount :: Player -> Player
resetFireCount p@Player{..} = p { _fireCountdown = weaponFireRate _weapon }


