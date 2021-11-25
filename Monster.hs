{-#LANGUAGE RecordWildCards #-}

module Monster where

import Types
import Sprites
import Options

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



monsterStepLength :: MonsterType -> Double
monsterStepLength monsterId
  | monsterId == Zombie = zombieStepLength
  | otherwise           = demonStepLength
 


monsterDamaged :: Monster -> Int -> Monster
monsterDamaged m@Monster{..} damage = 
    m { _health = _health - damage }
