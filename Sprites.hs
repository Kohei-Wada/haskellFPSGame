module Sprites where

import Types

data Sprite = Sprite
  { spriteType :: SpriteType
  , spritePos  :: Position2D
  } deriving (Show)
 


type SpriteType = Int
spriteNone    = -1 :: Int
spriteTree    = 0 :: Int
spriteZombie  = 1 :: Int -- animated, 2 frames
-- skip
spriteDemon   = 3 :: Int -- animated, 2 frames
-- skip
spriteGun     = 5 :: Int
spriteUzi     = 6 :: Int
spriteMedkit  = 7 :: Int
-- skip
spriteFPKnife = 8 :: Int -- animated, 2 frames
-- skip
spriteFPGun   = 10 :: Int -- animated, 2 frames
-- skip
spriteFPUzi   = 12 :: Int -- animated, 2 frames


animatedSpriteIds = [1,3,8,10,12] :: [Int] -- list of sprite IDs that are animated

spriteList =
  [
    [ -- 0
      "XXXXXXXXXXXXXXX",
      "XXXX/'''''\\XXXX",
      "XX/'       ''\\X",
      "X|   O        |",
      "X|        O   |",
      "XX\\_  O      /X",
      "XXXX\\_  ____/XX",
      "XXXXXX||XXXXXXX",
      "XXXXXX||XXXXXXX",
      "XX-=#/__\\#=-XXX"],
    [ -- 1
      "XXXXXXXXXXXXXXX",
      "XXXXX/```\\XXXXX",
      "XXXXX[o.o]XXXXX",
      "XXXXX\\II]/XXXXX",
      "XXX/^^````(III)",
      "X(III]    /XXXX",
      "XXXX(_____)XXXX",
      "XXXX(  _  )XXXX",
      "XXXX| |X\\ |XXXX",
      "X--(__]=|__)--X"],
    [ -- 2
      "XXXXXXXXXXXXXXXX",
      "XXXXX/```\\XXXXX",
      "XXXXX[o.o]XXXXX",
      "XXXXX\\[II/XXXXX",
      "X(III]```^^\\XXX",
      "XXXX\\    (III)X",
      "XXXX|_____)XXXX",
      "XXXX(  _  )XXXX",
      "XXXX/ | | |XXXX",
      "X--(__/=|__)--X"],
    [ -- 3
      "X|\\X-''`''-X/|X",
      "X\\;`       `;/X",
      "X/ ,_     _, \\X",
      "X( \\0) , (0/ )X",
      "XX\\_:  ^  :_/XX",
      "XXXX\\ |^| /XXXX",
      "XXXX| [_] |XXXX",
      "XXXXX\\___/XXXXX",
      "XXXXXXXXXXXXXXX",
      "X--==#####==--X"],
    [ -- 4
      "|\\XX-'```'-XX/|",
      "\\ ;`       `; /",
      "X/ ,_ \\ / _, \\X",
      "X( (0)   (0) )X",
      "XX\\_   A   _/XX",
      "XXXX) ___ (XXXX",
      "XXXX( ' ' )XXXX",
      "XXXXX'---'XXXXX",
      "XXXXXXXXXXXXXXX",
      "X--==#####==--X"],
    [ -- 5
      "XXXXXXXXXXXXXXX",
      "XXXXXXXXXXXXXXX",
      "Xl_/_\\\\\\^^^^^^]",
      "X/  P\\_______/X",
      "/    ({_)XXXXXX",
      "|   /XXXXXXXXXX",
      "(___)XXXXXXXXXX",
      "XXXXXXXXXXXXXXX",
      "XXXXXXXXXXXXXXX",
      "X--==#####==--X"],
    [ -- 6
      "|^\\XXXXXXXXX/^|",
      "|  ^^^^^^^^^ _|",
      "|  -o-- [[[(_o)",
      "|___________  |",
      "XX\\  __|X[_]\\_|",
      "XX/ /_]XX| |XXX",
      "X/ /XXXXX| |XXX",
      "|_/XXXXXX|_|XXX",
      "XXXXXXXXXXXXXXX",
      "--===#####===--"],
    [ -- 7
      "XXXXXXXXXXXXXXX",
      "XX/`````````\\XX",
      "X| ..  _  .. |X",
      "X|   _| |_   |X",
      "X|  [  +  ]  |X",
      "X|   `|_|`   |X",
      "X| ..     .. |X",
      "XX\\_________/XX",
      "XXXXXXXXXXXXXXX",
      " --==#####==-- "],
    [ -- 8
      "XXXXXXXXXXXXXXX",
      "XXXXXXXXXXXXXXX",
      "XXXXXXXXXXXXXXX",
      "XXXXXXXXX/|XXXX",                   
      "XXXXXXXX/ |XXXX",
      "XXXXXXX( ,|XXXX",
      "XXXXXXX| |<XXXX",
      "XXXXXXX| |<XXXX",
      "XXXXXXX| |<XXXX",
      "XXXXXXX| |<XXXX"],
     [ -- 9
      "XXXXXXXXXXXXXXX",
      "XXXXXXXXXXXXXXX",
      "XX|\\XXXXXXXXXXX",
      "XX| \\XXXXXXXXXX",
      "XX( ,\\XXXXXXXXX",
      "XXX\\ \\LXXXXXXXX",
      "XXXX\\ \\LXXXXXXX",
      "XXXXX\\ \\LX/\\XXX",
      "XXXXXX\\ \\V /XXX",
      "XXXX\\^^ ,  \\XXX"],
     [ -- 10
      "X/^^\\XXXXXXXXXX",
      "[:\\ Y\\XXXXXXXXX",
      "X\\:\\__`\\XXXXXXX",
      "XX\\:\\ \\ \\XXXXXX",
      "XXX\\:\\`  `\\XXXX",
      "XXX/\\:\\____\\XXX",
      "XX(_|::\\ P |\\XX",
      "XX( \\_:|   | )X",
      "XX(\\_ )-___/ /X",
      "XXX\\ \\      |XX"],
     [ -- 11
      "XWWWWWWWXXXXXXX",
      "WW/^^\\WWXXXXXXX",
      "WW|:) Y\\WXXXXXX",
      "XW(:( _ \\XXXXXX",
      "XXX|:( \\ \\XXXXX",
      "XXX\\::)`  \\XXXX",
      "XXXX\\:(    \\XXX",
      "XXXX(::\\____|\\X",
      "XXX/|::|  P | |",
      "XX(  \\_|    | /"],
     [ -- 12
      "X[\\^^\\XXXXXXXXX",
      "X|:\\__\\XXXXXXXX",
      "C|::|__|XXXXXXX",
      "X|: :\\ \\XXXXXXX",
      "X|:  :\\ \\XXXXXX",
      "X|::\\ :\\ `\\XXXX",
      "XX\\_:\\ :\\  \\XXX",
      "XXX|\\:\\ :\\__\\XX",
      "XX/|:\\:O :\\  `\\",
      "X( |:|\\:::|^^^|"],
     [ -- 13
      "WXXXXXXXXXXXXXX",
      "WWXXXXXXXXXXXXX",
      "WWW[\\^^\\XXXXXXX",
      "XWW|:\\__\\XXXXXX",
      "XWC|::|__|XXXXX",
      "XXW|: :\\ \\XXXXX",
      "XXX|:  :\\ \\XXXX",
      "XXX|::\\ :\\ `\\XX",
      "XXXX\\_:\\ :\\  \\X",
      "XXXX/ \\:\\ :\\__\\"
     ]
  ]



