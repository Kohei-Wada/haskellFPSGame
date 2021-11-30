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
  , _monsters      :: [Monster] -- list of monsters
  , _sprites       :: [Sprite]  -- list of sprites
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
  , _sprites       = []
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
castRays p@Player{..} gmap =
  [ let rayDirection = _playerRot + fieldOfView / 2 - (fromIntegral x) * rayAngleStep
        (rayX, rayY) = 
            castRay gmap _playerPos (floorPair _playerPos) rayDirection maxRaycastIterations
     in ( max (rayX - (distanceToProjectionPlane focalLength (abs $ _playerRot - rayDirection))) 0.0
        , rayY
        )
    | x <- [0..(fst viewSize) - 1]
  ]


-- Overlays a string image over another
overlay :: String -> String -> (Int,Int) -> (Int,Int) -> (Int,Int) -> Char -> String
overlay background foreground position backgroundResolution foregroundResolution transparentChar =
    let backgroundLines = splitChunks (fst backgroundResolution) background
        (firstLines,restLines) = splitAt (snd position) backgroundLines
        (secondLines,thirdLines) = splitAt (snd foregroundResolution) restLines
        foregroundLines =
          [
            take (fst position) (snd item) ++
              [
                if (fst chars) == transparentChar then (snd chars) else (fst chars)
                | chars <- zip (fst item) (take (fst foregroundResolution) (drop (fst position) (snd item)))
              ] ++
            drop (fst position + fst foregroundResolution) (snd item)
            | item <- zip (splitChunks (fst foregroundResolution) foreground) secondLines
          ]

     in concat (firstLines) ++ concat (foregroundLines) ++ concat (thirdLines)
 

-- Renders the lower info bar to String.
renderInfoBar :: GameState -> String
renderInfoBar gs@GameState{..} = 
    let separatorPositions = [0,15,31,63]
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
      


-- Renders the 3D player view (no bar or weapon) into String.
render3Dview :: [(Double, Normal)] -> [(SpriteType,Int,Double)] -> Int -> Int -> String
render3Dview wallMap spriteMap height frameNumber =
  let middle = div height 2 + 1                     -- middle line of the view
      heightDouble = (fromIntegral height)
   in concat
      [
        let distanceFromMiddle = middle - i
            absDistanceFromMiddle = abs distanceFromMiddle
         in map
            (
              \item ->
                  let normal       = (snd (fst item))
                      distance     = (fst (fst item))
                      columnHeight = floor ((distanceToSize distance) * heightDouble)
                      spriteInfo   = (snd item)
                      wallSample   = if absDistanceFromMiddle < columnHeight
                          then 
                            if normal      == NormalNorth then intensityToChar $ 0.25 + distanceToIntensity distance
                            else if normal == NormalEast  then intensityToChar $ 0.50 + distanceToIntensity distance
                            else if normal == NormalSouth then intensityToChar $ 0.75 + distanceToIntensity distance
                            else                               intensityToChar $ 1.00 + distanceToIntensity distance
                          else backgroundChar
                      
                      spriteHalfHeight = floor ( spriteScale * distanceToSize (thd3 spriteInfo) * fromIntegral (snd spriteSize) / 2 )
                      sampleX          = snd3 spriteInfo
                      sampleY          = round (((1 - (1 + (fromIntegral distanceFromMiddle) / (fromIntegral spriteHalfHeight)) / 2)) * fromIntegral ((snd spriteSize) - 1))
                      spriteSample     = sampleSprite (fst3 spriteInfo) (sampleX,sampleY) (animationFrameForSprite (fst3 spriteInfo) frameNumber)

                  in if (thd3 spriteInfo) >= distance -- is wall closer than sprite?
                        then wallSample                                 
                        else  -- sprite is closer  
                          if absDistanceFromMiddle <= spriteHalfHeight  
                            then
                              if spriteSample /= transparentChar
                                then spriteSample
                                else wallSample
                            else wallSample
            )
            (zip wallMap spriteMap) ++ "\n"
        | i <- [1..height]
      ]


-- projects one sprite (sprite,x,y) to a screen list [(sprite id,sprite x pixel,distance)]
projectSprite :: (SpriteType,Double,Double) -> [(SpriteType,Int,Double)] -> [(SpriteType,Int,Double)]  
projectSprite spriteInfo screenList = -- projects a single sprite to screen list
    let spritePos      = (snd3 spriteInfo) * fromIntegral ((length screenList) - 1)
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
          (zip screenList [0..])


{- Projects sprites to screen space, returns a list representing screen, 
   each pixel has (sprite id,sprite x pixel,distance), sprite id = -1 => empty. -}

-- project all sprites to screenspace first:
projectSprites :: [Sprite] -> Player -> [(SpriteType,Int,Double)]
projectSprites sps p@Player{..} =
    let emptyLs = [(spriteNone, 0, infinity) | i <- [0..(fst viewSize) - 1]]
        -- [(sprite id,sprite x pixel,distance)]
        (pX, pY) = _playerPos
        screenspaceSprites = 
          [
            -- sprite center in screenspace, normalized
            ( spType 
            , 0.5 + (angleDifference _playerRot (vectorAngle (spx - pX, spy - pY))) / fieldOfView
            , (pointDistance _playerPos spPos) - spriteDepthBias   -- sprite distance
            ) | Sprite spType spPos@(spx, spy) <- sps 
          ]

     -- compare depths
     in foldl ( \scrnLs1 scrnLs2 -> 
          map ( \(a, b) -> if (thd3 a) <= (thd3 b) then a else b) (zip scrnLs1 scrnLs2))
          emptyLs
     [projectSprite spriteItem emptyLs | spriteItem <- screenspaceSprites]
 

-- Samples given sprite.
sampleSprite :: SpriteType -> (Int,Int) -> Int -> Char
sampleSprite spriteId coordinates animationFrame =
    let safeCoords = ( clamp (fst coordinates) (0,(fst spriteSize) - 1)
                     , clamp (snd coordinates) (0,(snd spriteSize) - 1)
                     )
     in ((spriteList !! (spriteId + animationFrame)) !! (snd safeCoords)) !! (fst safeCoords)
    

-- Renders the game in 3D.
renderGameState :: GameState -> String
renderGameState gs@GameState{..} =
  let wallDrawInfo = castRays _player _gameMap
      gunSprite    = weaponSprite _player
   in (overlay
        (render3Dview wallDrawInfo (projectSprites _sprites _player) (snd viewSize) _frameNumber)
        (concat (spriteList !! gunSprite))
        weaponSpritePosition
        (addPairs viewSize (1,0))
        spriteSize
        transparentChar
      )
      ++
      renderInfoBar gs 
    
    

--TODO Encapsulate the sprites inside a monster.
-- Creates sprites and places them on the map depending on current state of things.
updateSprites ::  [Monster] -> [Sprite]
updateSprites ms = 
    map (\m -> Sprite { _spriteType = monsterSprite (_monsterType m)
                      , _spritePos  = (_monsterPos m)}) ms  
   

fire :: GameState -> GameState
fire gs@GameState{..} =
  if _fireCountdown _player == 0
    then gs { _player   = resetFireCount _player 
            , _monsters = filter ((>0) . _hp) $ 
                map (\m -> attackToMonster m _player _gameMap) _monsters
            }
    else gs


-- Computes the next game state.
nextGameState :: GameState -> GameState
nextGameState gs@GameState{..}  =
    gs { _frameNumber = _frameNumber + 1
       , _player      = updateFireCountDown _player
       , _monsters    = updateMonsters _monsters _player _gameMap _frameNumber
       , _sprites     = updateSprites _monsters
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
    

inputHandler :: GameState -> Char -> GameState
inputHandler gs@GameState{..} c  
  | c == keyForward     = gs { _player = movePlayerForward _player _gameMap stepLength } 
  | c == keyBackward    = gs { _player = movePlayerForward _player _gameMap (-1 * stepLength) } 
  | c == keyTurnLeft    = gs { _player = turnLeft _player }
  | c == keyTurnRight   = gs { _player = turnRight _player }
  | c == keyStrafeLeft  = gs { _player = strafePlayer _player _gameMap  stepLength }
  | c == keyStrafeRight = gs { _player = strafePlayer _player _gameMap  (-1 * stepLength) }
  | c == keyWeapon1     = gs { _player = changeWeapon _player Knife }
  | c == keyWeapon2     = gs { _player = changeWeapon _player Gun }
  | c == keyWeapon3     = gs { _player = changeWeapon _player Uzi }
  | c == keyFire        = fire gs
  | otherwise           = gs


gameLoop :: GameState -> IO ()
gameLoop gs = do
    t1 <- getCPUTime
    putStrLn (emptyLineString ++ renderGameState gs)
    c  <- getLastChar
    t2 <- getCPUTime
    threadDelay (frameDelayUs - ((fromIntegral (t2 - t1)) `div` 1000000)) 
    -- wait for the rest of frame time
    
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

