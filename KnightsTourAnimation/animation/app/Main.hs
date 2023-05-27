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
