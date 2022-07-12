{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module InitParser where 

import Control.Applicative
import Text.Trifecta 
import Text.RawString.QQ
import qualified Data.Map as M
import qualified Data.ByteString as B

-- -------------------------------------------------------------------
-- Data types 

-- [Header]
newtype Header =
  Header String
  deriving (Eq, Ord, Show)

-- Assignments
-- key=value
type Name = String
type Value = String 
type AssignmentsMap = M.Map Name Value 

-- Section
data Section =
  Section Header AssignmentsMap
  deriving (Eq, Show)

-- IniData 
data IniData =
  IniData { getIniMap :: (M.Map Header AssignmentsMap) }
  deriving (Eq, Show)

-- -------------------------------------------------------------------
-- IniData functions

type Key = String

-- | TODO: Make data type to return other than Int 
getIniData :: Header -> Key -> IniData -> Int 
getIniData h k ini =
  let m = getIniMap ini
      (Just m') = M.lookup h m
      (Just v) = M.lookup k m'
   in read v

testGettingData =
  let Success iniData = parseByteString parseIni mempty sectionsEx
      posX = getIniData (Header "Position") "posX" iniData
      posY = getIniData (Header "Position") "posY" iniData
      velX = getIniData (Header "Velocity") "velX" iniData
      velY = getIniData (Header "Velocity") "velY" iniData
   in (posX, posY, velX, velY) == (4, 3, 1, 1)

iniFile :: FilePath
iniFile = "data/start.ini"

loadIniFile :: IO B.ByteString
loadIniFile = B.readFile iniFile 

testFileData :: IO Bool 
testFileData = do
  bs <- loadIniFile
  let Success iniData = parseByteString parseIni mempty bs
      posX = getIniData (Header "Position") "posX" iniData
      posY = getIniData (Header "Position") "posY" iniData
      velX = getIniData (Header "Velocity") "velX" iniData
      velY = getIniData (Header "Velocity") "velY" iniData    
  return ((posX, posY, velX, velY) == (4, 3, 1, 1))

-- -------------------------------------------------------------------
-- Data examples

headerEx :: B.ByteString
headerEx = "[Startup]"

assignmentEx :: B.ByteString
assignmentEx = "posX=10"

commentEx :: B.ByteString
commentEx = "; Set starting position"

sectionEx :: B.ByteString
sectionEx = [r|
; Initialise starting vals
[Startup]
posX=4
posY=3
velX=1
velY=1
|]

sectionsEx :: B.ByteString
sectionsEx = [r|
; Modify this file to
; set initial values

; Set starting position
[Position]
posX=4
posY=3

; Set starting velocity
[Velocity]
velX=1
velY=1
|]

-- -------------------------------------------------------------------
-- Parsers 

parseHeader :: Parser Header
parseHeader = do
  char '['
  h <- some letter
  char ']'
  return $ Header h

parseHeader' :: Parser Header
parseHeader' =
  char '[' *> (Header <$> some letter) <* char ']'

skipWS :: Parser ()
skipWS = do
  skipMany (char ' ' <|> char '\n')

skipComments :: Parser ()
skipComments = do
  skipMany (do _ <- char ';' <|> char '#'  -- comment can begin with ; or #
               skipMany (noneOf "\n")
               skipWS)

parseAssignment :: Parser (Name, Value)
parseAssignment = do
  name <- some letter
  char '='
  val  <- some (noneOf "\n")
  skipWS
  return (name, val)

parseSection :: Parser Section
parseSection = do
  skipWS
  skipComments
  h <- parseHeader
  skipWS
  a <- many parseAssignment
  skipWS
  return $ Section h (M.fromList a)

parseIni :: Parser IniData
parseIni = do
  sections <- many (try parseSection)
  let c = foldr (\(Section h xs) acc -> M.insert h xs acc) M.empty sections
  return $ IniData c

-- -------------------------------------------------------------------
-- Tests

(+++) :: B.ByteString -> B.ByteString -> B.ByteString
(+++) = B.append

headerTest :: Bool
headerTest =
  let Success h = parseByteString parseHeader mempty headerEx
  in h == Header "Startup"

commentTest :: Bool
commentTest =
  let Success h = parseByteString
                  (skipComments >> parseHeader)
                  mempty
                  (commentEx +++ "\n" +++ headerEx)
  in h == Header "Startup"

