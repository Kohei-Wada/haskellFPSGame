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
  , _hp          :: Int
  , _countdownAI :: Int
  , _monsterRot  :: Double
  , _sprite     :: Sprite
  } deriving (Show)
 

newMonster :: MonsterType -> Position2D -> Monster
newMonster monsterType initialPosition = Monster
  { _monsterType = monsterType
  , _monsterPos  = initialPosition
  , _hp          = if monsterType == Zombie then hpZombie else hpDemon 
  , _countdownAI = 0
  , _monsterRot  = 0
  , _sprite      = Sprite { _spriteType = monsterSprite monsterType 
                          , _spritePos  = initialPosition}
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
    m { _hp = _hp - damage }


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
      , _sprite      = _sprite { _spritePos = _monsterPos } 
      }


monsterDistance :: Monster -> Player -> Double
monsterDistance m@Monster{..} p@Player{..} = pointDistance _monsterPos _playerPos


monsterAngleDiff :: Monster -> Player -> Double
monsterAngleDiff m@Monster{..} p@Player{..} = 
     abs $ angleDifference _playerRot (vectorAngle $ substractPairs _monsterPos _playerPos)


attackToMonster :: Monster -> Player -> GameMap -> Monster
attackToMonster m p gmap = 
     if angleDiff < angleRange / 2 && md <= wd && md <= atkRange 
        then monsterDamaged m weaponDamage else m
            where angleDiff  = monsterAngleDiff m p
                  md         = monsterDistance m p 
                  angleRange = 1.0 / (md + aimAccuracy)
                  wd         = wallDistance p gmap
                  atkRange   = attackRange p


-- Runs the AI for each monster, updating their positions etc.
updateMonsters :: [Monster] -> Player -> GameMap -> Int -> [Monster]
updateMonsters ms p@Player{..} gmap frameNum =
  if disableAI 
     then ms 
     else map (\m -> updateMonster m p gmap frameNum) $ filter ((>0) . _hp) ms



