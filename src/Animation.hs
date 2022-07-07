module Animation where

import Control.Concurrent ( threadDelay )
import Control.Monad      ( forever ) 
import qualified System.Console.Terminal.Size as TS

verticalWallChar :: Char 
verticalWallChar = '|'

horizontalWallChar :: Char
horizontalWallChar = '-'

-- -------------------------------------------------------------------
-- Vector

data Vector =
  Vector {getX :: Int, getY :: Int}
  deriving (Show, Eq)

addVector :: Vector -> Vector -> Vector
addVector (Vector x1 y1) (Vector x2 y2) =
   Vector (x1 + x2) (y1 + y2)

(+->) :: Vector -> Vector -> Vector 
(+->) = addVector

test1 :: Bool 
test1 = let v1 = Vector 1 2
            v2 = Vector 3 4
         in v1 `addVector` v2 == Vector 4 6

test2 :: Bool
test2 = let v1 = Vector 1 2
            v2 = Vector 3 4
         in v1 +-> v2 == Vector 4 6

-- -------------------------------------------------------------------
-- Configuration 

data Config = Config
  { ballInitialVelocity :: Vector
  , ballInitialPosition :: Vector
  , frameWidth          :: Int
  , frameHeight         :: Int
  } deriving (Show)

data AnimationState = AnimationState
  { ballVelocity        :: Vector
  , ballPosition        :: Vector
  } deriving (Show)

-- -------------------------------------------------------------------
-- Game logic

mkState :: Vector -> Vector -> AnimationState
mkState = AnimationState 

nextStateX :: AnimationState -> Vector -> AnimationState
nextStateX AnimationState
             { ballVelocity = Vector vX vY
             , ballPosition = Vector pX pY }
           Vector
             { getX = w
             , getY = y }
  | pX >= w-2 = mkState (Vector (negate vX) vY) (Vector (w-3) pY)
  | pX <= 1   = mkState (Vector (negate vX) vY) (Vector 2 pY)
  | otherwise = mkState (Vector vX vY) (Vector (pX+vX) pY)

nextStateY :: AnimationState -> Vector -> AnimationState
nextStateY AnimationState
             { ballVelocity = Vector vX vY
             , ballPosition = Vector pX pY }
           Vector
             { getX = w
             , getY = h }
  | pY >= h-2 = mkState (Vector vX (negate vY)) (Vector pX (h-3))
  | pY <= 2   = mkState (Vector vX (negate vY)) (Vector pX 3)
  | otherwise = mkState (Vector vX vY) (Vector pX (pY+vY))

-- -------------------------------------------------------------------
-- Render logic

render :: Config -> AnimationState -> IO ()
render Config
         { frameWidth   = width
         , frameHeight  = height }
       AnimationState
         { ballPosition = pos } =

  -- Calculate total characters to draw on screen
  -- https://stackoverflow.com/questions/1730961/convert-a-2d-array-index-into-a-1d-index
  let bufferSize = width * height
      Vector x y = pos
      ballPos    = (y * width) + x    -- OR (row * row_width) + col for flipped axes

  -- Recursively construct string to draw to screen 
  in putStr $ "\n" ++ go 0 bufferSize ballPos 
  where
    go :: Int -> Int -> Int -> [Char] 
    go i target ballPos

      -- end of buffer
      | i > target = []

      -- draw ball
      | i == ballPos = 'o' : go (i+1) target ballPos

      -- draw corners
      | i == width || i == target-1 =
        '/' : go (i+1) target ballPos
      
      | i == (width*2-1) || i == (target-width) =
        '\\' :  go (i+1) target ballPos

      -- draw vertical walls 
      | i `rem` width == 0 && (i < (width * height)) ||
        i `rem` width == (width-1) =
          verticalWallChar : go (i+1) target ballPos

      -- draw horizontal wall
      | i `elem` [width..width*2] ++
                 [(width*height)-width..(width*height)]
        && (i < (width*height)) =
          horizontalWallChar : go (i+1) target ballPos

      -- default draw empty space 
      | otherwise = ' ' : go (i+1) target ballPos

-- -------------------------------------------------------------------
-- Initial config

mkSize :: IO Vector
mkSize = do
   size <- TS.size
   case size of
     Just (TS.Window x y) ->
       return $ Vector y x
     _ ->
       error "couldn't get terminal size"

mkConfig :: Vector -> Config
mkConfig size =
  Config
  { ballInitialVelocity = Vector 1 1
  , ballInitialPosition = Vector 10 3
  , frameWidth          = w
  , frameHeight         = h
  }
  where (Vector w h) = size

mkAnimationState :: Config -> AnimationState
mkAnimationState c = AnimationState
  { ballVelocity = ballInitialVelocity c
  , ballPosition = ballInitialPosition c
  }

-- -------------------------------------------------------------------
-- Animation logic

drawTimer :: Config -> AnimationState -> IO ()
drawTimer c s = do
  render c s
  
  --putStrLn $ "Pos: " ++ show (ballPosition s)
  --putStrLn $ "Vel: " ++ show (ballVelocity s)
  
  threadDelay 500000
  drawTimer c (nextStateY (nextStateX s wh) wh)
  where w  = frameWidth c
        h  = frameHeight c
        wh = Vector w h

-- -------------------------------------------------------------------
-- CLI, Main function

main :: IO ()
main = do
  terminalSize <- mkSize
  
  let config         = mkConfig terminalSize
      animationState = mkAnimationState config

  putStrLn $ "Terminal size: " ++ show terminalSize
  putStrLn $ "Pos: " ++ show (ballPosition animationState)
  putStrLn $ "Vel: " ++ show (ballVelocity animationState)
  
  drawTimer config animationState
  
  putStrLn "\nDone"
