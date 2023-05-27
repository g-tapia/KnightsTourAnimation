module Main where

import MP5b
import MP5a
import KnightsTourAnimation (animateKnightsTour)
import Data.Maybe (fromJust)
import Text.Read (readMaybe)
import System.IO (stdout, hFlush, hSetBuffering, BufferMode(..), stdin, hGetBuffering)
import System.Environment (getArgs)



main :: IO ()
main = do
  let dimensions = (5,5)
      startPosition = (2, 2)
      initialBoard = MP5a.Board dimensions [startPosition]
  animateKnightsTour [initialBoard] []
  runAnimationWithInputs
  runAnimationWithoutInputs
  args <- getArgs
  if null args
    then runAnimationWithInputs
    else case args of
      ["--no-input"] -> runAnimationWithoutInputs
      _            -> putStrLn "Invalid argument. Use --no-input to run without user input."
runAnimationWithInputs :: IO ()
runAnimationWithInputs = do
  putStr "Enter the dimensions of the board (width height): "
  hFlush stdout
  dimensionsInput <- getLine
  let parsedDimensions = readMaybeTuple dimensionsInput :: Maybe (Int, Int)
  case parsedDimensions of
    Nothing -> putStrLn "Invalid input for dimensions"
    Just dimensions -> do
      putStr "Enter the starting position (x y): "
      hFlush stdout
      startPositionInput <- getLine
      let parsedStartPosition = readMaybeTuple startPositionInput :: Maybe (Int, Int)
      case parsedStartPosition of
        Nothing -> putStrLn "Invalid input for starting position"
        Just startPosition -> runAnimation dimensions startPosition

runAnimationWithoutInputs :: IO ()
runAnimationWithoutInputs = do
  let dimensions = (5,5)
      startPosition = (0, 0)
      initialBoard = MP5a.Board dimensions [startPosition]
  animateKnightsTour [initialBoard] []

runAnimation :: (Int, Int) -> (Int, Int) -> IO ()
runAnimation dimensions startPosition = do
  let initialBoard = MP5a.Board dimensions [startPosition]
  animateKnightsTour [initialBoard] []

-- utility function to parse a tuple of integers
readMaybeTuple :: String -> Maybe (Int, Int)
readMaybeTuple input = case words input of
  [x, y] -> do
    x' <- readMaybe x
    y' <- readMaybe y
    return (x', y')
  _ -> Nothing

  