{-#LANGUAGE RecordWildCards #-}
module Player where

import Types
import Utils
import Options
import GameMap


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

