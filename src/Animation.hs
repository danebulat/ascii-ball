module Animation where

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

-- Game logic
nextState :: AnimationState -> AnimationState
nextState AnimationState { ballVelocity = v, ballPosition = pos } =
  AnimationState { ballVelocity = v, ballPosition = pos +-> v }

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
  in putStr $ go 0 bufferSize ballPos
  where
    go :: Int -> Int -> Int -> [Char] 
    go i target ballPos

      -- end of buffer
      | i > target = []

      -- draw ball
      | i == ballPos = 'o' : go (i+1) target ballPos 

      -- draw vertical walls 
      | i == 0 ||
        i `rem` width == 0 ||
        i `rem` width == (width-1) =
          verticalWallChar : go (i+1) target ballPos

      -- draw horizontal wall
      -- TODO

      -- default draw empty space 
      | otherwise = ' ' : go (i+1) target ballPos

-- -------------------------------------------------------------------
-- Animation logic

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
  { ballInitialVelocity = Vector 1 0
  , ballInitialPosition = Vector 3 3
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
-- CLI, Main function

main :: IO ()
main = do
  terminalSize <- mkSize

  putStrLn $ "Terminal size: " ++ show terminalSize
  
  let config         = mkConfig terminalSize
      animationState = mkAnimationState config

  render config animationState 
