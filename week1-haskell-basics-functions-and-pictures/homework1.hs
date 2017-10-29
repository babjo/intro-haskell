{-# OPTIONS_GHC -fno-warn-warnings-deprecations -fno-warn-unused-binds #-}
import CodeWorld

main :: IO ()
main = exercise3

-- Exercise 1

createCircle :: Color -> Double -> Picture
createCircle c dx = colored c (translated 0 dx (solidCircle 1))

botCircle, midCircle, topCircle :: Color -> Picture
botCircle c = createCircle c (-2.25)
midCircle c = createCircle c 0
topCircle c = createCircle c 2.25

frame :: Picture
frame = rectangle 2.5 7

trafficLight :: Color -> Color -> Color -> Picture
trafficLight c1 c2 c3 = botCircle c1 & midCircle c2 & topCircle c3 & frame

trafficController :: Integer -> Picture
trafficController s
  | s >= 0 && s <= 2 = trafficLight green black  black 
  | s == 3           = trafficLight black yellow black
  | s >= 4 && s <= 6 = trafficLight black black  red
  | otherwise        = trafficLight black yellow red

trafficLightAnimation :: Double -> Picture
trafficLightAnimation t = trafficController (round t `mod` 8)

exercise1 :: IO ()
exercise1 = animationOf trafficLightAnimation

-- Exercise 2

blossom :: Double -> Picture
blossom t = colored yellow (solidCircle ((min t 10) / 50))

tree :: Picture -> Integer -> Picture
tree bl 0 = bl
tree bl n = translated 0 1 (rotated (pi/10) (tree bl (n-1)) 
                            & rotated (- pi/10) (tree bl (n-1))) 
            & path [(0,0),(0,1)]
            
blossomTree :: Double -> Picture
blossomTree t = tree (blossom t) 8
  
exercise2 :: IO ()
exercise2 = animationOf blossomTree

-- Exercise 3

wall, ground, storage, box :: Picture
wall =    colored (grey 0.4) (solidRectangle 1 1)
ground =  colored yellow (solidRectangle 1 1)
storage = solidCircle 0.3 & ground
box =     colored brown (solidRectangle 1 1)

drawTile :: Integer -> Picture
drawTile t
  | t == 1    = wall
  | t == 2    = ground
  | t == 3    = storage
  | t == 4    = box
  | otherwise = blank
         
drawMaze :: Integer -> Picture
drawMaze 11 = blank
drawMaze y = drawRow (-10) y & drawMaze (y+1)

drawRow :: Integer -> Integer -> Picture
drawRow 11 _ = blank
drawRow x y = drawTileAt x y & drawRow (x+1) y
     
pictureOfMaze :: Picture
pictureOfMaze = drawMaze (-10)

exercise3 :: IO ()
exercise3 = drawingOf pictureOfMaze

drawTileAt :: Integer -> Integer -> Picture
drawTileAt x y = translated (fromIntegral x) (fromIntegral y) (drawTile (maze x y))
         
maze :: Integer -> Integer -> Integer 
maze x y
  | abs x > 4  || abs y > 4  = 0
  | abs x == 4 || abs y == 4 = 1
  | x ==  2 && y <= 0        = 1
  | x ==  3 && y <= 0        = 3
  | x >= -2 && y == 0        = 4
  | otherwise                = 2
 