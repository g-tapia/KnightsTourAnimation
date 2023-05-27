module MP5a where


import Data.Maybe
import Data.Ord
import Data.List
import Data.List
import Data.Tree
import Data.Map (Map, empty, fromList, (!), keys, elems, assocs,
                 findWithDefault, member, insert, insertWith)
import System.Random
import System.Random.Shuffle
import Control.Concurrent
import Control.Monad.State
import System.IO
import System.Console.ANSI
import GHC.IO
import Debug.Trace


-- a board stores its dimensions and all visited nodes, with the knight
-- standing on the first node, thus new nodes always get appended at the
-- front;  always have to have at least one position in the path for it
-- to be valid;  coordinates go from bottom left to top right
data Board = Board (Int, Int) [(Int, Int)] deriving (Eq)


-- to determine whether a calculated position is in bounds of the board
validPosition :: (Int, Int) -> (Int, Int) -> Bool
validPosition (w, h) (x, y) =
  x >= 0 && x < w && y >= 0 && y < h


-- move set of a knight piece
moveSet :: [(Int, Int)]
moveSet = [(1, 2), (2, 1), (2, -1), (1, -2), (-1, -2), (-2, -1), (-2, 1), (-1, 2)]


-- search function from lecture notes
search :: (Eq a, Show a) =>
          (a -> Bool) -- is the tour finished?
          -> (a -> [a]) -- neighbors == possibleMoves == adj
          -> ([a] -> [a] -> [a]) -- comb == numberOfPossibleMoves
          -> [a] -> [a] -- [start]
          -> Maybe a
search goal adj comb unvisited visited
  | null unvisited = Nothing
  | goal (head unvisited) = Just (head unvisited)
  | otherwise = let (n:ns) = unvisited
                in  debug n $ -- uncomment to "debug"
                   search goal adj comb (comb (removeDups (adj n)) ns) (n:visited)
  where removeDups = filter (not . (`elem` (unvisited ++ visited)))



debug :: Show a => a -> b -> b
debug x y = unsafePerformIO clearScreen `seq`
            unsafePerformIO (setCursorPosition 0 0) `seq`
            unsafePerformIO (putStrLn $ show x) `seq`
            unsafePerformIO (threadDelay $ 3*10^5) `seq`
            y


debugAnimation :: Show a => a -> Int -> b -> b
debugAnimation current frameNum y = unsafePerformIO clearScreen `seq`
                                    unsafePerformIO (setCursorPosition 0 0) `seq`
                                    unsafePerformIO (putStrLn $ "frame number: " ++ show frameNum ++ ", current: " ++ show current) `seq`
                                    unsafePerformIO (threadDelay $ 3*10^5) `seq`
                                    y


-- Call with an admissible heuristic as the cost function to carry out A* search
bestFirstSearch :: (Eq a, Show a, Ord b) =>
                   (a -> Bool)
                   -> (a -> [a])
                   -> (a -> b)
                   -> a -> Maybe a
bestFirstSearch goal succ cost start = search goal succ comb [start] []
  where comb new old = sortOn cost (new ++ old)


-- just making sure we can print boards readably
instance Show Board where
  show (Board (w, h) path) =
    "\n" ++
    unlines (map (\y -> unwords 
    (map (\x -> 
      let pos = (x, h-y-1) in
      if elem pos path then
        if pos == (head path) && (length path == 1) then "K"
        else if pos == (head path) && (length path > 1)  then "o"
        else if pos == path !! 1 then "K"
        else if pos == (last path) then "S"
        else "." 
        else "_") [0..w-1])) [0..h-1])


-- goal predicate; we're done if we've visited all possible fields on the board
isFinishedTour :: Board -> Bool
isFinishedTour (Board (w, h) path) = length path == w * h


-- generate possible valid moves from the last position in the path
generatePossibleMoves :: Board -> [(Int, Int)]
generatePossibleMoves (Board dimensions ((x, y) : path)) =
  filter (\position -> validPosition dimensions position && not (elem position path)) (map (\(xoff, yoff) -> (x+xoff, y+yoff)) moveSet)


-- calculates possible moves from the knight's current position
possibleMoves :: Board -> [Board]
possibleMoves board@(Board dimensions path) =
  map (\move -> Board dimensions (move : path)) (generatePossibleMoves board)


-- calculate number of valid moves possible at a certain position
numberOfPossibleMoves :: Board -> Int
numberOfPossibleMoves board = length (possibleMoves board)


-- combining all the parts to find a knight's tour for a given board
-- size and starting position
knightsTour :: (Int, Int) -> (Int, Int) -> Maybe Board
knightsTour dimensions start =
  bestFirstSearch isFinishedTour possibleMoves numberOfPossibleMoves (Board dimensions [start])