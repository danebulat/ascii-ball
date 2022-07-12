{-
This version reads data from an INI (initialisation)
file to set a default position and velocity for the
ball.

Edit the INI file to set the program's starting values.
-}

module Animation where

import Control.Concurrent ( threadDelay )
import System.IO          ( hSetBuffering, stdout, BufferMode(..) )
import System.Environment ( getArgs )
import InitParser         ( IniData, getIniData, mkIni )
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
  | pX+vX > w-2 =
    let newX = calcOvershoot (w-2) pX vX
    in mkState (Vector (negate vX) vY) (Vector newX pY)
    
  | pX+vX < 1 =
    let newX = calcOvershoot' 1 pX vX
    in mkState (Vector (negate vX) vY) (Vector newX pY)
    
  | otherwise = mkState (Vector vX vY) (Vector (pX+vX) pY)

nextStateY :: AnimationState -> Vector -> AnimationState
nextStateY AnimationState
             { ballVelocity = Vector vX vY
             , ballPosition = Vector pX pY }
           Vector
             { getX = w
             , getY = h }
  | pY+vY > h-3 =
    let  newY = calcOvershoot (h-3) pY vY
    in mkState (Vector vX (negate vY)) (Vector pX newY)
    
  | pY+vY < 1 =
    let newY = calcOvershoot' 1 pY vY
    in mkState (Vector vX (negate vY)) (Vector pX newY)
    
  | otherwise= mkState (Vector vX vY) (Vector pX (pY+vY))

calcOvershoot :: Int -> Int -> Int -> Int
calcOvershoot bound p v =
  let ov = abs((p + v) - bound) -- overshoot in next frame
      tw = bound - p            -- space to wall
      mv = abs (ov - tw)        -- amount to move in opposite direction from wall
  in bound - mv

calcOvershoot' :: Int -> Int -> Int -> Int
calcOvershoot' bound p v =
  let ov = abs((p + v) + bound) -- overshoot in next frame
      tw = bound + p            -- space to wall
      mv = abs (ov - tw)        -- amount to move in opposite direction from wall
  in bound + mv

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

mkConfig :: Vector -> IniData -> Config
mkConfig size ini =
  Config
  { ballInitialVelocity = Vector velX velY
  , ballInitialPosition = Vector posX posY
  , frameWidth          = w
  , frameHeight         = h
  }
  where (Vector w h) = size
        posX = getIniData "Position" "posX" ini
        posY = getIniData "Position" "posY" ini
        velX = getIniData "Velocity" "velX" ini
        velY = getIniData "Velocity" "velY" ini

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

          threadDelay 150000
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
  iniData <- mkIni
  
  let config         = mkConfig terminalSize iniData
      animationState = mkAnimationState config

  putStrLn $ "Terminal size: " ++ show terminalSize
  putStrLn $ "Pos: " ++ show (ballPosition animationState)
  putStrLn $ "Vel: " ++ show (ballVelocity animationState)
  putStrLn "Start:"

  T.hideCursor
  drawTimer config animationState 500  -- pass how many frames to render
  T.showCursor

  putStrLn "\nDone"
