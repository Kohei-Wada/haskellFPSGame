{-#LANGUAGE RecordWildCards #-}

module Game where

import Options
import Utils
import Sprites
import Types
import Monster
import GameMap 
import Weapon
import Player

import System.IO
import System.Timeout
import Data.Fixed
import Data.Char
import Control.Concurrent
import System.CPUTime
import Data.List


data GameState = GameState
  { _player        :: Player
  , _frameNumber   :: Int
  , _currentLevel  :: Int
  , _currentScore  :: Int
  , _gameMap       :: GameMap
  , _monsters      :: [Monster] 
  } deriving (Show)
 

initialGameState :: GameState
initialGameState = GameState
  { _player        = initialPlayer
  , _frameNumber   = 0
  , _currentLevel  = 1
  , _currentScore  = 0
  , _gameMap       = gameMap1
  , _monsters      =
      [ newMonster Zombie (6,7)
      , newMonster Demon (8,5)
      , newMonster Demon (10,2)
      , newMonster Demon (2,10)
      ]
  }


-- Gets the distance from projection origin to projection plane.
distanceToProjectionPlane :: Double -> Double -> Double
distanceToProjectionPlane focalDistance angleFromCenter = focalDistance * (cos angleFromCenter)


-- Renders the game state into string, simple version.
renderMap :: GameState -> String
renderMap gs@GameState{..} =
  concat (map
        (   
          \square ->
            (if mod (snd square) (fst mapSize) == 0 then "\n" else "")
            ++
            (if floor (fst (_playerPos _player)) == fst (arrayToMapCoords (snd square)) &&
                 floor (snd (_playerPos _player)) == snd (arrayToMapCoords (snd square))
                    then
                        case round (4.0 * (_playerRot _player) / pi)  of
                            0 -> "->"
                            1 -> "/^"
                            2 -> "|^"
                            3 -> "^\\"
                            4 -> "<-"
                            5 -> "./"
                            6 -> ".|"
                            7 -> "\\."
                            8 -> "->"
                    else if fst square == SquareEmpty
                          then "  "
                          else "[]"
            )
        ) (zip _gameMap [0..])
    )


-- Casts all rays needed to render player's view, returns a list of ray cast results.
castRays :: Player -> GameMap -> [(Double, Normal)]
castRays Player{..} gmap =
  [ let rayDirection = _playerRot + fieldOfView / 2 - (fromIntegral x) * rayAngleStep
        (rayX, rayY) = 
            castRay gmap _playerPos (floorPair _playerPos) rayDirection maxRaycastIterations
     in ( max (rayX - (distanceToProjectionPlane focalLength (abs $ _playerRot - rayDirection))) 0.0
        , rayY
        )
    | x <- [0..(fst viewSize) - 1]
  ]


-- Renders the lower info bar to String.
renderInfoBar :: GameState -> String
renderInfoBar GameState{..} = 
    let separatorPositions = [0, 15, 31, 63]
        separator = "+" 
                  ++ [if i `elem` separatorPositions then '+' else '~' | i <- [3..(fst viewSize)]] 
                  ++ "+"

        emptyLine = "|" 
                  ++ [if i `elem` separatorPositions then '|' else ' ' | i <- [3..(fst viewSize)]] 
                  ++ "|\n"

        infoLine =  "|  level: "  ++ (toLength (show _currentLevel) 3) 
                 ++ "|  score: "  ++ (toLength (show _currentScore ) 6) 
                 ++ "|  health: 100/100  ##########  |  ammo: 100/100"

     in separator 
     ++ "\n" 
     ++ emptyLine 
     ++ (toLength infoLine ((fst screenSize) - 1)) 
     ++ "|\n" 
     ++ emptyLine 
     ++ separator
      

-- projects one sprite (sprite,x,y) to a screen list [(sprite id,sprite x pixel,distance)]
projectSprite :: (SpriteType,Double,Double) -> [(SpriteType,Int,Double)] -> [(SpriteType,Int,Double)]  
projectSprite spriteInfo scrnLs = -- projects a single sprite to screen list
    let spritePos      = (snd3 spriteInfo) * fromIntegral ((length scrnLs) - 1)
        spriteLength   = (distanceToSize (thd3 spriteInfo)) * fromIntegral (fst spriteSize) * spriteScale
        spriteInterval = (floor (spritePos - spriteLength / 2) , floor (spritePos + spriteLength / 2))

     in map (\(spX, spY) -> if spY >= (fst spriteInterval) && spY <= (snd spriteInterval)
                then
                  ( (fst3 spriteInfo)
                  , round $ ((fromIntegral(spY - (fst spriteInterval))) / spriteLength) * fromIntegral ((fst spriteSize) - 1)
                  , (thd3 spriteInfo)
                  )
                else spX 
          )
          (zip scrnLs [0..])


-- Samples given sprite.
sampleSprite :: SpriteType -> (Int,Int) -> Int -> Char
sampleSprite spriteId coordinates animationFrame =
    let safeCoords = ( clamp (fst coordinates) (0,(fst spriteSize) - 1)
                     , clamp (snd coordinates) (0,(snd spriteSize) - 1)
                     )
     in ((spriteList !! (spriteId + animationFrame)) !! (snd safeCoords)) !! (fst safeCoords)
    

-- Overlays a string image over another
overlay :: String -> String -> (Int,Int) -> (Int,Int) -> Char -> String
overlay background foreground backgroundResolution foregroundResolution transparentChar =
    let backgroundLines = splitChunks (fst backgroundResolution) background
        (wX, wY)        = weaponSpPos
        (fstLs,restLs)  = splitAt wY backgroundLines
        (sndLs, thdLs)  = splitAt (snd foregroundResolution) restLs
        foregroundLs =
          [ take wX i2 ++
              [ if (fst chars) == transparentChar then (snd chars) else (fst chars)
                | chars <- zip i1 (take (fst foregroundResolution) (drop wX i2))
              ] 
              ++ drop (wX + fst foregroundResolution) i2
            | (i1, i2) <- zip (splitChunks (fst foregroundResolution) foreground) sndLs
          ]

     in concat (fstLs) ++ concat (foregroundLs) ++ concat (thdLs)
 
screenspaceSprite :: Player -> Monster -> (SpriteType, Double, Double) 
screenspaceSprite p@Player{..} m@Monster{..} = 
    let a = angleToMonster m p
        d = distanceToMonster m p

     in ( _spriteType _sprite 
        , (angleDifference _playerRot a) / fieldOfView + spAngleBias
        , d - spDepthBias -- sprite distance
        )


{- Projects sprites to screen space, returns a list representing screen, 
    each pixel has (sprite id,sprite x pixel,distance), sprite id = -1 => empty. -}

-- project all sprites to screenspace first:
projectSprites :: Player -> [Monster] -> [(SpriteType,Int,Double)]
projectSprites p@Player{..} ms =   
    foldl (\scrnLs1 scrnLs2 -> 
        map (\(a, b) -> if (thd3 a) <= (thd3 b) then a else b) (zip scrnLs1 scrnLs2))
        emptyLs
        [projectSprite s emptyLs | s <- sps ]
    where emptyLs = [(spriteNone, 0, infinity) | i <- [0..(fst viewSize) - 1]]
          sps = map (\m -> screenspaceSprite p m) ms


-- Renders the 3D player view (no bar or weapon) into String.
render3Dview :: [(Double, Normal)] -> [(SpriteType,Int,Double)] -> Int -> Int -> String
render3Dview wallInfo spriteMap height frameNumber =
  let middle = div height 2 + 1   -- middle line of the view
   in concat
      [
        let distFromMiddle    = middle - i
            absDistFromMiddle = abs distFromMiddle
         in map
            (\(iwall, isprite) ->
                  let (wdist, nml)  = iwall 
                      (t, x, sdist) = isprite
                      colHgt  = floor ((distanceToSize wdist) * (fromIntegral height))
                      wallSample = if abs distFromMiddle < colHgt
                          then 
                            if      nml == NormalNorth then intensityToChar $ 0.25 + distanceToIntensity wdist
                            else if nml == NormalEast  then intensityToChar $ 0.50 + distanceToIntensity wdist
                            else if nml == NormalSouth then intensityToChar $ 0.75 + distanceToIntensity wdist
                            else                            intensityToChar $ 1.00 + distanceToIntensity wdist
                          else backgroundChar
                      
                      spriteHalfHgt = floor (spriteScale * distanceToSize sdist * fromIntegral (snd spriteSize) / 2)
                      sampleX       = x 
                      sampleY       = round (((1 - (1 + (fromIntegral distFromMiddle) / (fromIntegral spriteHalfHgt)) / 2)) * fromIntegral ((snd spriteSize) - 1))
                      spriteSample  = sampleSprite t (sampleX, sampleY) (animationFrameForSprite t frameNumber)

                  in if sdist >= wdist -- is wall closer than sprite?
                        then wallSample                                 
                        else  -- sprite is closer  
                          if absDistFromMiddle <= spriteHalfHgt  
                            then
                              if spriteSample /= transparentChar
                                then spriteSample
                                else wallSample
                            else wallSample
            )
            (zip wallInfo spriteMap) ++ "\n"
        | i <- [1..height]
      ]
     

-- Renders the game in 3D.
renderGameState :: GameState -> String
renderGameState gs@GameState{..} =
  let wallDrawInfo = castRays _player _gameMap
   in (overlay
        (render3Dview wallDrawInfo (projectSprites _player _monsters) (snd viewSize) _frameNumber)
        (concat (spriteList !! weaponSprite _player))
        (addPairs viewSize (1,0))
        spriteSize
        transparentChar
      )
      ++
      renderInfoBar gs 
    

-- Computes the next game state.
nextGameState :: GameState -> GameState
nextGameState gs@GameState{..}  =
    gs { _frameNumber = _frameNumber + 1
       , _player      = updateFireCountDown _player
       , _monsters    = updateMonsters _monsters _player _gameMap _frameNumber
       }


-- Reads all available chars on input and returns the last one, or ' ' if not available.
getLastChar :: IO Char
getLastChar = do
    isInput <- hWaitForInput stdin 1
    if isInput
      then do
        c1 <- getChar
        c2 <- getLastChar
             
        if c2 == ' '
          then return c1
          else return c2
               
      else do
        return ' '
    

fire :: GameState -> GameState
fire gs@GameState{..} =
  if _fireCountdown _player == 0
    then gs { _player   = resetFireCount _player 
            , _monsters = filter ((>0) . _hp) $ map (\m -> attackToMonster m _player _gameMap) _monsters
            }
    else gs


inputHandler :: GameState -> Char -> GameState
inputHandler gs@GameState{..} c  
  | c == keyForward     = gs { _player = movePlayerForward _player _gameMap stepLength } 
  | c == keyBackward    = gs { _player = movePlayerForward _player _gameMap (-1 * stepLength) } 
  | c == keyTurnLeft    = gs { _player = turnLeft _player }
  | c == keyTurnRight   = gs { _player = turnRight _player }
  | c == keyStrafeLeft  = gs { _player = strafePlayer _player _gameMap stepLength }
  | c == keyStrafeRight = gs { _player = strafePlayer _player _gameMap (-1 * stepLength) }
  | c == keyWeapon1     = gs { _player = changeWeapon _player Knife }
  | c == keyWeapon2     = gs { _player = changeWeapon _player Gun }
  | c == keyWeapon3     = gs { _player = changeWeapon _player Uzi }
  | c == keyFire        = fire gs
  | otherwise           = gs


display :: GameState -> IO ()
display gs = putStrLn (emptyLineString ++ renderGameState gs)


gameLoop :: GameState -> IO ()
gameLoop gs = do
    t1 <- getCPUTime
    display gs
    c  <- getLastChar
    t2 <- getCPUTime
    threadDelay (frameDelayUs - ((fromIntegral (t2 - t1)) `div` 1000000)) 
    
--  t3 <- getCPUTime
--  putStrLn (show (fromIntegral (t3 - t1) / 10e9) ++ " ms")  -- for profiling, comment out otherwise
    
    hFlush stdout
    
    if c == keyQuit
      then do putStrLn "quitting"
      else do 
          let newState = inputHandler gs c 
          gameLoop (nextGameState newState )
      
        
gameMain :: IO ()
gameMain = do
    hSetBuffering stdin NoBuffering                     -- to read char without [enter]
    hSetBuffering stdout (BlockBuffering (Just 20000))  -- to read flickering
    hSetEcho stdout False                               
    gameLoop initialGameState

