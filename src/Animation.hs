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

updateState :: Vector -> Vector -> AnimationState
updateState = AnimationState 

nextStateY :: AnimationState -> Vector -> AnimationState
nextStateY AnimationState { ballVelocity = v, ballPosition = pos }
           Vector { getX = w, getY = h } =
  let Vector posX posY = pos
      Vector velX velY = v
  in
    if posY >= w-2 then
      updateState (Vector velX (negate velY)) (Vector posX (w-3))
      
    else if posY <= 1 then
      updateState (Vector velX (negate velY)) (Vector posX 2)
    else
      let xVel = Vector 0 (getY v) in
      AnimationState { ballVelocity = v, ballPosition = pos +-> xVel }

nextStateX :: AnimationState -> Vector -> AnimationState
nextStateX AnimationState { ballVelocity = v, ballPosition = pos }
           Vector { getX = w, getY = h } =
  let Vector posX posY = pos
      Vector velX velY = v
   in
    if posX >= h-2 then
      updateState (Vector (negate velX) velY) (Vector (h-3) posY)
    else if posX <= 2 then
      updateState (Vector (negate velX) velY) (Vector 3 posY)
    else
      let yVel = Vector (getX v) 0 in
      AnimationState { ballVelocity = v, ballPosition = pos +-> yVel }

-- -------------------------------------------------------------------
-- Render logic

render :: Config -> AnimationState -> IO ()
render Config {
         frameWidth   = width,
         frameHeight  = height
       }
       AnimationState {
         ballPosition = pos
       } =

  -- Calculate total characters to draw on screen
  -- https://stackoverflow.com/questions/1730961/convert-a-2d-array-index-into-a-1d-index
  let bufferSize = width * height
      Vector x y = pos
      ballPos    = (x * width) + y    -- (row * row_width) + col

  -- Recursively construct string to draw to screen 
  in putStr $ "\n" ++ go 0 bufferSize ballPos 
  where
    go :: Int -> Int -> Int -> [Char] 
    go i target ballPos

      -- end of buffer
      | i > target = []

      -- draw ball
      | i == ballPos = 'o' : go (i+1) target ballPos 

      -- draw vertical walls 
      | i == 0 ||
        i `rem` width == 0 && (i < (width * height)) ||
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
       return $ Vector x y
     _ ->
       error "couldn't get terminal size"

mkConfig :: Vector -> Config
mkConfig size =
  Config
  { ballInitialVelocity = Vector 1 (-1)
  , ballInitialPosition = Vector 3 10
  , frameWidth          = w
  , frameHeight         = h
  }
  where (Vector h w) = size

mkAnimationState :: Config -> AnimationState
mkAnimationState c = AnimationState
  { ballVelocity = ballInitialVelocity c
  , ballPosition = ballInitialPosition c
  }

-- -------------------------------------------------------------------
-- Animation logic

drawTimer :: Config -> AnimationState -> IO ()
drawTimer c s = forever $ do
  render c s
  threadDelay 1000

drawTimer' :: Config -> AnimationState -> IO ()
drawTimer' c s = do
  render c s
  
  --putStrLn $ "Pos: " ++ show (ballPosition s)
  --putStrLn $ "Vel: " ++ show (ballVelocity s)
  
  threadDelay 500000
  drawTimer' c (nextStateX (nextStateY s wh) wh) 
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
  
  drawTimer' config animationState
  
  --render config animationState
  putStrLn "\nDone"
