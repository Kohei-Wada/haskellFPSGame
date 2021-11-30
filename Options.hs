module Options where 

-- key mapping:

import Types

disableAI = False                   -- for debug


--key bind
keyForward     = 'w'
keyBackward    = 's'
keyTurnLeft    = 'a'
keyTurnRight   = 'd'
keyStrafeLeft  = 'q'
keyStrafeRight = 'e'
keyFire        = 'p'
keyWeapon1     = '1'
keyWeapon2     = '2'
keyWeapon3     = '3'
keyQuit        = 'x'


frameDelayMs = 16 :: Int                   -- in millisecond
frameDelayUs = frameDelayMs * 1000 :: Int -- in microseconds

stepLength   = 0.1 :: Double
rotationStep = 0.06

zombieStepLength = 0.01
demonStepLength = 0.09

mapSize = (15,15) :: (Int, Int)
infoBarHeight = 5
screenSize = (150,45)
viewSize = ( (fst screenSize) , (snd screenSize) - infoBarHeight ) :: (Int, Int)
fieldOfView = pi / 2
focalLength = 0.5
maxRaycastIterations = 20 :: Int

spriteSize      = (15,10) :: (Int, Int)
spDepthBias = 1       :: Double    -- so that sprites don't disappear in walls
spAngleBias = 0.5     :: Double
spriteScale     = fromIntegral (snd viewSize) / fromIntegral (snd spriteSize) * 2

totalMapSquares = (fst mapSize) * (snd mapSize)
rayAngleStep = fieldOfView / fromIntegral (fst viewSize)
infinity = 1.0 / 0.0
animationFrameStep = 4 :: Int
backgroundChar = ' '
transparentChar = 'X'               -- marks transparency in sprites
emptyLines = 15                     -- number of empty lines added before each rendered frame
emptyLineString = ['\n' | i <- [1..emptyLines]]
recomputeAIin = 64 :: Int
aimAccuracy = 0.32 -- this constant is used in fire function to determine if a monster was hit

fireRateKnife = 6  :: Int
fireRateGun   = 10 :: Int
fireRateUzi   = 4  :: Int
knifeAttackDistance = 1.5
weaponDamage = 20 :: Int  -- damage of all weapons

--monster
hpZombie = 100 :: Int  -- initial health amounts
hpDemon  = 50  :: Int


weaponSpPos = 
    ((fst viewSize) - (fst viewSize) `div` 3
    ,1 + snd viewSize - snd spriteSize
    )




