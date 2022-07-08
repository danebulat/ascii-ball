module Animation where

import Control.Concurrent ( threadDelay )
import Control.Monad      ( forever )
import System.IO          ( hSetBuffering, stdout, BufferMode(..) )

import qualified System.Console.ANSI as T

verticalWallChar :: Char
verticalWallChar = '|'

horizontalWallChar :: Char
horizontalWallChar = '-'

cornerChars :: (Char, Char)
cornerChars = ('/','\\')

ballChar :: Char
ballChar = '0'

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
-- Animation logic

mkState :: Vector -> Vector -> AnimationState
mkState = AnimationState

nextStateX :: AnimationState -> Vector -> AnimationState
nextStateX AnimationState
             { ballVelocity = Vector vX vY
             , ballPosition = Vector pX pY }
           Vector
             { getX = w
             , getY = y }
  | pX+vX >= w-2 = mkState (Vector (negate vX) vY)
                   (Vector (pX-vX-((pX+vX)-(w-2))) pY)
            -- bounce velocity ^
  | pX+vX <= 1   = mkState (Vector (negate vX) vY)
                   (Vector (pX+vX+abs(1-(pX+vX))) pY)
            -- bounce velocity ^
  | otherwise = mkState (Vector vX vY) (Vector (pX+vX) pY)

nextStateY :: AnimationState -> Vector -> AnimationState
nextStateY AnimationState
             { ballVelocity = Vector vX vY
             , ballPosition = Vector pX pY }
           Vector
             { getX = w
             , getY = h }
  | pY+vY >= h-2 = mkState (Vector vX (negate vY))
                   (Vector pX (pY-vY-((pY+vY)-(h-2))))
               -- bounce velocity ^
  | pY+vY <= 1  = mkState (Vector vX (negate vY))
                   (Vector pX (pY+vY+abs(1-(pY+vY))))
               -- bounce velocity ^
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
  let height'    = height - 1
      bufferSize = width * height'
      Vector x y = pos
      ballPos    = (y * width) + x    -- OR (row * row_width) + col for flipped axes

  -- Recursively construct string to draw to screen
  -- Remove 'init' to align correctly in GHCI
  in putStr $ go 0 bufferSize ballPos
  where
    go :: Int -> Int -> Int -> [Char]
    go i target ballPos

      -- end of buffer
      | i > target = []

      -- draw ball
      | i == ballPos = ballChar : go (i+1) target ballPos

      -- draw corners
      | i == 0 || i == target-1 =
        fst cornerChars : go (i+1) target ballPos

      | i == (width-1) || i == (target-width) =
        snd cornerChars :  go (i+1) target ballPos

      -- draw vertical walls 
      | i `rem` width == 0 && (i < (width * (height-1))) ||
        i `rem` width == (width-1) =
          verticalWallChar : go (i+1) target ballPos

      -- draw horizontal wall
      | i `elem` [1..width-1] ++
                 [(width*(height-1))-width..(width*(height-1))]
        && (i < (width*(height-1))) =
          horizontalWallChar : go (i+1) target ballPos

      -- default draw empty space 
      | otherwise = ' ' : go (i+1) target ballPos

-- -------------------------------------------------------------------
-- Initial config

mkSize :: IO Vector
mkSize = do
   size <- T.getTerminalSize
   case size of
     Just (x, y) ->
       return $ Vector y x
     _ ->
       error "couldn't get terminal size"

mkConfig :: Vector -> Config
mkConfig size =
  Config
  { ballInitialVelocity = Vector (2) (2)
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
-- Draw logic

drawTimer :: Config -> AnimationState -> Int -> IO ()
drawTimer c s n = do
  render c s
  go c s n
  where
    go c s n
      | n == 0 = do
          return ()

      | otherwise = do
          T.setCursorPosition 0 0
          T.cursorUp h
          render c s

          -- putStrLn $ "Pos: " ++ show (ballPosition s)
          -- putStrLn $ "Vel: " ++ show (ballVelocity s)

          threadDelay 100000
          go c (nextStateY (nextStateX s wh) wh) (n-1)
    w  = frameWidth c
    h  = frameHeight c
    wh = Vector w h

-- -------------------------------------------------------------------
-- CLI, Main function

run :: IO ()
run = do
  hSetBuffering stdout NoBuffering
  terminalSize <- mkSize

  let config         = mkConfig terminalSize
      animationState = mkAnimationState config

  putStrLn $ "Terminal size: " ++ show terminalSize
  putStrLn $ "Pos: " ++ show (ballPosition animationState)
  putStrLn $ "Vel: " ++ show (ballVelocity animationState)
  putStrLn "Start:"

  T.hideCursor
  drawTimer config animationState 500  -- pass how many frames to render
  T.showCursor

  putStrLn "\nDone"
