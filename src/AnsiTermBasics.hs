module AnsiTermBasics where

import Control.Monad       ( replicateM_ ) 
import Control.Concurrent  ( threadDelay )
import System.IO           ( hFlush, stdout )
import System.Console.ANSI

-- --------------------------------------------------------------------------------
-- More Examples:
-- https://github.com/UnkindPartition/ansi-terminal/blob/master/app/Example.hs
-- --------------------------------------------------------------------------------

helloWorld :: IO ()
helloWorld = do
  setCursorPosition 0 5
  setTitle "ANSI Terminal Short Example"

  setSGR [ SetConsoleIntensity BoldIntensity
         , SetColor Foreground Vivid Red
         ]

  putStr "Hello "

  setSGR [ SetConsoleIntensity NormalIntensity
         , SetColor Foreground Vivid White
         , SetColor Background Dull Blue
         ]

  putStrLn "World"

  resetScreen

-- To reset the screen, the default graphic rendition must be selected
-- (setSGR [Reset]) before all character positions are put into the
-- erased state (clearScreen).

resetScreen :: IO ()
resetScreen = setSGR [Reset] >> clearScreen >> setCursorPosition 0 0

-- Helper function to temporarily delay execution

pause :: IO ()
pause = do
  hFlush stdout
  threadDelay 1000000

pause3 :: IO ()
pause3 = replicateM_ 3 pause

getTerminalSizeExample :: IO ()
getTerminalSizeExample = do
  result <- getTerminalSize
  case result of
    Just (h, w) ->
      putStrLn $ "The size of the terminal is " ++
                 show h ++ " rows by " ++
                 show w ++ " columns.\n"
    Nothing ->
      putStrLn "Error: unable to get the terminal size\n"

getCursorPositionExample :: IO ()
getCursorPositionExample = do
  putStrLn "         11111111112222222222"
  putStrLn "12345678901234567890123456789"
  putStr   "Report cursor position here:"

  result <- getCursorPosition

  putStrLn " (3rd row, 29th column) to stdin, as CSI 3 ; 29 R.\n"
  case result of
    Just (row, col) ->
      putStrLn $ "The cursor was at row number " ++
                 show (row + 1) ++ " and column number " ++
                 show (col + 1) ++ ".\n"
    Nothing ->
      putStrLn "Error: unable to get the cursor position\n"

  pause3

setCursorPositionExample :: IO ()
setCursorPositionExample = do
  resetScreen
  
  putStrLn "Line One"
  putStrLn "Line Two"
  pause3
  -- Line One
  -- Line Two

  setCursorPosition 0 5
  putStr "Foo"
  pause3
  -- Line Foo
  -- Line Two

  setCursorPosition 1 5
  putStr "Bar"
  pause3
  -- Line Foo
  -- Line Bar

  setCursorColumn 1
  putStr "oaf"
  pause3
  -- Line Foo
  -- Loaf Bar

saveRestoreCursorExample :: IO ()
saveRestoreCursorExample = do
  resetScreen
  putStr "Start sentence ..."
  pause3
  -- Start sentence ...

  saveCursor
  setCursorPosition 2 3
  putStr "SPLASH!"
  pause3
  -- Start sentence ...
  --
  --    SPLASH!

  restoreCursor
  putStr " end sentence, uninterrupted."
  pause3
  -- Start sentence ... end sentence, uninterrupted
  --
  --    SPLASH!

titleExample :: IO ()
titleExample = do
  putStr "Title Demo"
  pause3
  setTitle "New Title"
  pause3

cursorVisibilityExample :: IO ()
cursorVisibilityExample = do
  putStrLn "Cursor Demo"
  pause3

  hideCursor
  pause3

  showCursor
  pause3

scrollExample :: IO ()
scrollExample = do
  putStrLn "Line One"
  putStrLn "Line Two"
  putStrLn "Line Three"
  replicateM_ 3 pause

  scrollPageDown 2
  replicateM_ 3 pause

  scrollPageUp 3
  replicateM_ 3 pause

clearExample :: IO ()
clearExample = do
  putStrLn "Line One"
  putStrLn "Line Two"
  pause3
  -- Line One
  -- Line Two
  
  setCursorPosition 0 4
  clearFromCursorToScreenEnd
  pause3
  -- Line 

  resetScreen
  putStrLn "Line One"
  putStrLn "Line Two"
  pause3
  -- Line One
  -- Line Two

  setCursorPosition 1 4
  clearFromCursorToScreenBeginning
  pause3
  --
  --    Two

  resetScreen
  putStrLn "Line One"
  putStrLn "Line Two"
  pause3
  -- Line One
  -- Line Two

  setCursorPosition 0 4
  clearFromCursorToLineEnd
  pause3
  -- Line
  -- Line Two
  
  setCursorPosition 1 4
  clearFromCursorToLineBeginning
  pause3
  -- Line
  --      Two

  clearLine
  pause3
  -- Line

  clearScreen
  pause3
  --resetScreen
  --

cursorMovementExample :: IO ()
cursorMovementExample = do
  putStrLn "Line One"
  putStr "Line Two"
  pause3
  -- Line One
  -- Line Two

  cursorUp 1
  putStr " - Extras"
  pause3
  -- Line One - Extras
  -- Line Two

  cursorBackward 2
  putStr "zz"
  pause3
  -- Line One - Extrzz
  -- Line Two

  cursorForward 2
  putStr "- And More"
  pause3
  -- Line One - Extrzz  - And More
  -- Line Two

  cursorDown 1
  putStr "Disconnected"
  pause3
  -- Line One - Extrzz  - And More
  -- Line Two                     Disconnected

  resetScreen

lineChangeExample :: IO ()
lineChangeExample = do
  putStrLn "Line One"
  putStr "Line Two"
  pause3
  -- Line One
  -- Line Two

  cursorUpLine 1
  putStr "New Line One"
  pause3
  -- New Line One
  -- Line Two

  cursorDownLine 1
  putStr "New Line Two"
  pause3
  -- New Line One
  -- New Line Two

  resetScreen

hyperlinkExample :: IO ()
hyperlinkExample = do
  putStr "Hyperlink demo: "
  hyperlink "https://example.com" "Example hyperlink\n"
  putStrLn ""
  putStrLn "Linked hyperlinks demo:"
  hyperlinkWithId "ref" "https://example.com" "Example linked hyperlink one\n"
  hyperlinkWithId "ref" "https://example.com" "Example linked hyperlink two\n"

  replicateM_ 5 pause
  -- Hyperlink demo: Example hyperlink
  --
  -- Linked hyperlinks demo:
  -- Example linked hyperlink one
  -- Example linked hyperlink two
