module Weapon where

import Sprites
import Options

data Weapon = Knife | Gun | Uzi deriving(Show, Eq)

weaponFireRate :: Weapon -> Int
weaponFireRate weaponId
  | weaponId == Knife = fireRateKnife
  | weaponId == Gun   = fireRateGun
  | weaponId == Uzi   = fireRateUzi
  | otherwise         = 1
 

weaponSprite :: Weapon -> Int
weaponSprite weaponId
  | weaponId == Knife = spriteFPKnife
  | weaponId == Gun   = spriteFPGun
  | weaponId == Uzi   = spriteFPUzi
  | otherwise         = spriteFPKnife
 
