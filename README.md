# ASCII Ball 

## Overview 

An ASCII text "bounce" animation rendered in the terminal.
Implemented in Haskell.

### `src/Animation.hs`

This default version reads data from an INI (initialisation)
file to set a default position and velocity for the
ball.

Edit the INI file to set the program's starting values 
in `data/start.ini`.

### `src/AnimationV1.hs`

This version is the simplest implementation. The configuration
is hard-coded in the source file, and the animation frames
are individually rendered one after the other in the terminal.

### `src/AnimationV2.hs`

This version reads command-line arguments to set the 
configuration. 

```bash
# Syntax:
stack run posX posY velX velY

# Example:
stack run 3 4 1 1

# With GHCI:
ghci> :set args 3 4 1 1
ghci> run
```

### Running Another Version

Import the module's `run` function into `app/Main.hs` and call it 
in `main`. For example:

```haskell
module Main where 

import AnimationV2 ( run )

main :: IO ()
main = run
```

## Building and Running 

Build and run the application using Stack. See the requested 
GHC resolve in `stack.yaml`.

```bash 
stack build 
stack run
```

## Screenshot

![ASCII Ball - Screenshot](/doc/screenshots/screenshot-01.png)
