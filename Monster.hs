{-#LANGUAGE RecordWildCards #-}

module Monster where

import Types
import Sprites
import Options
import Player
import GameMap
import Utils

data MonsterType = Zombie | Demon deriving(Show, Eq)


data Monster = Monster
  { _monsterType :: MonsterType
  , _monsterPos  :: Position2D
  , _health      :: Int
  , _countdownAI :: Int
  , _monsterRot  :: Double
  } deriving (Show)
 

newMonster :: MonsterType -> Position2D -> Monster
newMonster monsterType initialPosition = Monster
  { _monsterType = monsterType
  , _monsterPos  = initialPosition
  , _health      = if monsterType == Zombie then monsterHealthZombie else monsterHealthDemon
  , _countdownAI = 0
  , _monsterRot  = 0
  }
  

monsterSprite :: MonsterType -> Int
monsterSprite monsterId
  | monsterId == Zombie = spriteZombie
  | otherwise           = spriteDemon


mStepLen :: MonsterType -> Double
mStepLen monsterId
  | monsterId == Zombie = zombieStepLength
  | otherwise           = demonStepLength
 

monsterDamaged :: Monster -> Int -> Monster
monsterDamaged m@Monster{..} damage = 
    m { _health = _health - damage }


--TODO
nextMonsterRot :: Monster -> Position2D -> Int -> Double 
nextMonsterRot m@Monster{..} targetPos frameNum =  
    if _monsterType == Zombie-- zombie walks towards the player
        then vectorAngle $ substractPairs targetPos _monsterPos  
        else if _countdownAI == 0
            then angleTo02Pi $ 
                (fst _monsterPos) + (snd _monsterPos) + (fromIntegral frameNum) / 100.0
            else _monsterRot


--TODO
updateMonster :: Monster -> Player -> GameMap -> Int -> Monster
updateMonster m@Monster{..} p@Player{..} gmap frameNum=
  m 
      { _monsterPos  = moveWithCollision gmap _monsterPos _monsterRot (mStepLen _monsterType)
      , _monsterRot  = nextMonsterRot m _playerPos frameNum
      , _countdownAI = if _countdownAI <= 0 then recomputeAIin else _countdownAI - 1
      }



