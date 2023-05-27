module KnightsTourAnimation (animateKnightsTour) where

import MP5a
import Data.List
import Data.Colour
import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Data.Picture
import Control.Monad (guard, when, void)
import qualified Data.Colour.Names as SelectColor
import Graphics.Gloss.Data.Color (Color, makeColor)
import Data.Colour.SRGB (toSRGB, channelBlue, channelGreen, channelRed)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import System.IO.Unsafe (unsafePerformIO)
import Debug.Trace (trace)
import Graphics.Gloss.Juicy
import System.IO.Unsafe
import Codec.Picture (DynamicImage(..), Image(..), PixelRGBA8, readImage)
import Codec.Picture.Extra (crop, scaleBilinear)
import Control.DeepSeq (deepseq)

import Graphics.Gloss.Data.Picture( Picture( Bitmap ) )
import Codec.Picture (convertRGBA8, dynamicMap)
import Foreign.ForeignPtr
import Foreign.Ptr
import Data.Vector.Storable (unsafeWith)



cellSize :: Float
cellSize = fromIntegral 70

-- animate knights tour function passes an initial state to the update function
animateKnightsTour :: [Board] -> [Board] -> IO ()
animateKnightsTour openList visited = simulate window (toGlossColor SelectColor.wheat) 4 (0, [], [], openList, visited) (animateFrame) update
  where
    window = FullScreen


-- update function builds off the initial state and continues the search
-- (0,0) represents a buffer value before the search begins
update :: ViewPort -> Float -> (Int, [(Int,Int)], [(Int,Int)], [Board], [Board]) -> (Int,[(Int,Int)], [(Int,Int)], [Board], [Board])
update _ _ (_, _, _, [], visitedList) = (0, [], [], [], visitedList)
update _ _ (frameNum, currentNeighbor, remainingNeighbors, openList, visitedList)

  | isFinishedTour (head openList) = (0, [], [], openList, [])

  | frameNum == 1 =
    let remainingNeighbors = generatePossibleMoves (head openList)
    in (incrementedFrameNum, currentNeighbor, remainingNeighbors, openList, visitedList)

  | frameNum == 2 && length remainingNeighbors > 0 =
                                let neighbor = if not (null remainingNeighbors) then (head remainingNeighbors) else (-1,-1)
                                in (2, (currentNeighbor ++ [neighbor]), (tail remainingNeighbors), openList, visitedList)

  | frameNum == 4 = let current:ns = openList 
                    in (1,[], [],(comb (removeDups (possibleMoves current)) ns), current:visitedList)

  | otherwise = debugAnimation (head openList) frameNum $ 
                (incrementedFrameNum, currentNeighbor, remainingNeighbors, openList, visitedList)
  where
    incrementedFrameNum = frameNum + 1
    removeDups = filter (not . (`elem` (openList ++ visitedList)))
    comb new old = sortOn numberOfPossibleMoves (new ++ old)
    

-- animate the model by converting it to a picture
animateFrame :: (Int, [(Int,Int)], [(Int,Int)], [Board], [Board]) -> Picture
animateFrame (frameNum, currentNeighbor, remainingNeighbors, currentBoard, _) = draw frameNum (head currentBoard) currentNeighbor


draw :: Int -> Board -> [(Int, Int)] -> Picture
draw frameNum board@(Board dimensions@(w , h) path) currentNeighbor = pictures $ concatMap drawColumn [0 .. h - 1] ++ [drawKnight dimensions $ head path]
  where
    drawColumn y = map (\x -> cellPositionToDraw frameNum (x, y) board neighbors currentNeighbor) [0 .. w - 1]
    neighbors = generatePossibleMoves board
    knightPosition = if (frameNum /= 3) then (head path) else (head neighbors)


drawKnight :: (Int, Int) -> (Int, Int) -> Picture
drawKnight dimensions knightPosition =
  uncurry translate (boardToGloss dimensions knightPosition) knightImage

-- taking the regular coordinates and converting them to gloss coordinates
boardToGloss :: (Int, Int) -> (Int, Int) -> (Float, Float)
boardToGloss (w, h) (x, y) = (fromIntegral x * cellSize - halfBoardWidth + cellSize/2,
                              (fromIntegral y * cellSize - halfBoardHeight + cellSize/2))
  where
    halfBoardWidth = fromIntegral w * cellSize / 2
    halfBoardHeight = fromIntegral h * cellSize / 2


-- function application; applies right function to left input
(|>) :: a -> (a -> b) -> b
x |> f = f x

cellPositionToDraw :: Int -> (Int, Int) -> Board -> [(Int, Int)] -> [(Int, Int)] -> Picture
cellPositionToDraw frameNumber pos (Board dimensions path) neighbors currentNeighbor = pictures [cellContent, circleColor]
  where
    visitedPosition = pos `elem` path && pos /= head path
    (x,y) = pos
    cellContent = if visitedPosition
                  then uncurry translate (boardToGloss dimensions pos) $ scaleImageToFitCell $ cropImageToCellPosition chessBoardImage pos dimensions
                  else let emptyColor = if (fst pos + snd pos) `mod` 2 == 0
                                      then toGlossColor SelectColor.darkgreen
                                      else toGlossColor SelectColor.white
                       in color emptyColor $ uncurry translate (boardToGloss dimensions pos) $ rectangleSolid cellSize cellSize

    circleColor = if not visitedPosition && pos `elem` neighbors && frameNumber == 2 && pos `elem` currentNeighbor                 
                  then createNeighborShape
                  else blank
    string = if calculatedMoves > 1 then " moves" else " move"


    createNeighborShape =
      pictures [color (toGlossColor SelectColor.red) (circleSolid (cellSize/4))
              , color (toGlossColor SelectColor.white) (circleSolid (cellSize/5))
              , color (toGlossColor SelectColor.red) (circleSolid (cellSize/8))
              , color (toGlossColor SelectColor.red) (rectangleSolid (cellSize/16) (cellSize/2))
              , color (toGlossColor SelectColor.red) (rectangleSolid (cellSize /2) (cellSize/16))
              , color textColor (text (show neighborMoves <> string))
                  |> scale 0.1 0.1
                  |> translate (-28) 20
              ]
        |> translate (fst (boardToGloss dimensions pos)) (snd (boardToGloss dimensions pos))

    calculatedMoves = let moves = numberOfPossibleMoves (Board dimensions (pos:path))
                      in if moves == 0 then 1 else moves
    textColor = if (fst pos + snd pos) `mod` 2 == 0
                                      then toGlossColor SelectColor.white
                                      else toGlossColor SelectColor.black
    neighborMoves = calculatedMoves


--
--                                    Color Resolvers
-- ###########################################################################################

-- convert a color from Data.Colour library to a Gloss color
toGlossColor :: Colour Double -> Color
toGlossColor c = let (r, g, b, a) = colorToRGBA c
                 in makeColor r g b a


-- convert RGBA components to a Gloss color
colorToRGBA :: Colour Double -> (Float, Float, Float, Float)
colorToRGBA c = (toFloat r, toFloat g, toFloat b, toFloat a)
    where sRGB = toSRGB c
          r = channelRed sRGB
          g = channelGreen sRGB
          b = channelBlue sRGB
          a = 1.0 
          toFloat = realToFrac


--
--                                    Image Resolvers (Knight Image)
-- ###########################################################################################

knightImage :: Picture
knightImage = unsafePerformIO $ do
  maybePicture <- loadJuicyPNG "C:\\Users\\"  -- add your path here (<-) for the knight image
  case maybePicture of
    Nothing -> error "Failed to load the image."
    Just picture -> return $ scaleImageToFitCell picture

scaleImageToFitCell :: Picture -> Picture
scaleImageToFitCell pic@(Bitmap img) =
  let (imgWidth, imgHeight) = bitmapSize img
      scaleX    = cellSize/ fromIntegral imgWidth
      scaleY    = cellSize / fromIntegral imgHeight
  in scale scaleX scaleY pic


--
--                                    Image Resolvers (Painting)
-- ###########################################################################################

chessBoardImage :: Image PixelRGBA8
chessBoardImage = unsafePerformIO $ do
  maybePicture <- readImage "C:\\Users\\" -- add your path here (<-) for the background image
  case maybePicture of
    Left _ -> error "Failed to load the image."
    Right img -> return $ convertRGBA8 img


cropImageToCellPosition :: Image PixelRGBA8 -> (Int, Int) -> (Int, Int) -> Picture
cropImageToCellPosition img (x, y) (w, h) =
  let
    cellWidth = fromIntegral (imageWidth img) / fromIntegral w
    cellHeight = fromIntegral (imageHeight img) / fromIntegral h
    cropX = fromIntegral x * cellWidth
    cropY = fromIntegral (h - 1 - y) * cellHeight
    croppedImg = crop (round cropX) (round cropY) (round cellWidth) (round cellHeight) img
  in fromImageRGBA8 croppedImg
