module Draw where

import Brick
import qualified Graphics.Vty as V

attrTable :: [(AttrName, V.Attr)]
attrTable = [
             (whiteAttr, fg V.white),
             (redAttr, fg V.red), 
             (greenAttr, fg V.green), 
             (yellowAttr, fg V.yellow), 
             (pinkAttr, fg V.magenta),
             (cyanAttr, fg V.cyan `V.withStyle` V.bold)
            ]

whiteAttr :: AttrName
whiteAttr = attrName "white"

redAttr :: AttrName
redAttr = attrName "red"

greenAttr :: AttrName
greenAttr = attrName "green"

pinkAttr :: AttrName
pinkAttr = attrName "pink"

yellowAttr :: AttrName
yellowAttr = attrName "yellow"

cyanAttr :: AttrName
cyanAttr = attrName "cyan"

type Color = AttrName
type ColorBoard = [[Color]]
type Canvas = ([String], ColorBoard)

menu :: [String]
menu = ["                                                                               ", "                                                                               ", "                                                                               ", "                                                                               ", "                                                                               ", "                                                                               ", "                ■ ■ ■ ■ ■           ■ ■ ■ ■ ■ ■             ■ ■ ■              ", "            ■ ■ ■     ■ ■ ■       ■ ■ ■     ■ ■             ■ ■ ■ ■            ", "            ■ ■         ■ ■     ■ ■ ■       ■ ■ ■         ■ ■ ■ ■ ■            ", "          ■ ■ ■                 ■ ■                       ■ ■   ■ ■            ", "          ■ ■                   ■ ■                     ■ ■ ■   ■ ■            ", "          ■ ■                 ■ ■ ■     ■ ■ ■ ■         ■ ■     ■ ■            ", "          ■ ■                 ■ ■ ■         ■ ■       ■ ■ ■     ■ ■            ", "          ■ ■ ■       ■ ■       ■ ■         ■ ■       ■ ■ ■ ■ ■ ■ ■            ", "            ■ ■     ■ ■ ■       ■ ■ ■     ■ ■ ■     ■ ■ ■       ■ ■ ■          ", "            ■ ■ ■ ■ ■ ■           ■ ■ ■ ■ ■ ■ ■     ■ ■         ■ ■ ■          ", "                                                                               ", "                                                                               ", "                                                                               ", "                                                                               ", "                                                                               ", "                                                                               ", "                                                                               ", "                                                                               ", "                                                                               ", "                                                                               ", "                                                                               ", "                                                                               ", "                                                                               ", "                                                                               "]

menuColorBoard :: ColorBoard
menuColorBoard = (replicate 20 $ replicate 80 redAttr) ++ (replicate 10 $ replicate 80 whiteAttr)

whiteBoard :: [String]
whiteBoard = ["                                                                               ", "                                                                               ", "                                                                               ", "                                                                               ", "                                                                               ", "                                                                               ", "                                                                               ", "                                                                               ", "                                                                               ", "                                                                               ", "                                                                               ", "                                                                               ", "                                                                               ", "                                                                               ", "                                                                               ", "                                                                               ", "                                                                               ", "                                                                               ", "                                                                               ", "                                                                               ", "                                                                               ", "                                                                               ", "                                                                               ", "                                                                               ", "                                                                               ", "                                                                               ", "                                                                               ", "                                                                               ", "                                                                               ", "                                                                               "]

whiteColorBoard :: ColorBoard
whiteColorBoard = replicate 30 $ replicate 80 whiteAttr

makeWidget :: Canvas -> Widget ()
makeWidget (strs, cb) = vBox [makeRows i | i <- [0..length strs - 1]]
    where
        makeRows i = hBox [makeCols j | j <- [0..length (strs !! i) - 1]]
            where
                makeCols j = withAttr (cb !! i !! j) $ str [strs !! i !! j]

drawText :: Int -> Int -> String -> Color -> Canvas -> Canvas
drawText x y text color (strs, cb) = (strs', cb')
    where
    strs' = 
        take x strs ++ 
        [take y (strs !! x) ++ text ++ drop (y + length text) (strs !! x)] ++ 
        drop (x + 1) strs
    cb' =
        take x cb ++
        [take y (cb !! x) ++ replicate (length text) color ++ drop (y + length text) (cb !! x)] ++
        drop (x + 1) cb

drawBoxBorder :: Int -> Int -> Int -> Int -> Color -> Canvas -> Canvas
drawBoxBorder curX endX y w color canvas
    | curX < endX =
        drawText curX y ("║" ++ (replicate (w - 2) ' ') ++ "║") color
      $ drawBoxBorder (curX + 1) endX y w color canvas
    | otherwise = canvas

drawBox :: Int -> Int -> Int -> Int -> Color -> Canvas -> Canvas
drawBox x y h w color canvas 
    | w < 3 || h < 3 = canvas
    | otherwise =
        drawText x y ("╔" ++ (replicate (w - 2) '═') ++ "╗") color 
      $ drawBoxBorder (x + 1) (x + h - 1) y w color
      $ drawText (x + h - 1) y ("╚" ++ (replicate (w - 2) '═') ++ "╝") color canvas

drawWhiteBox :: Int -> Int -> Int -> Int -> Canvas -> Canvas
drawWhiteBox x y h w canvas 
    | w < 1 || h < 1 = canvas
    | otherwise =
        drawText x y (replicate w ' ') whiteAttr
      $ drawWhiteBox (x + 1) y (h - 1) w canvas

changeColor :: Int -> Int -> Int -> Int -> Color -> Canvas -> Canvas
changeColor x1 y1 x2 y2 color (strs, cb)
    | x1 > x2 || y1 > y2 = (strs, cb) 
    | x1 == x2 = (strs, take x1 cb ++ [take y1 (cb !! x1) ++ replicate (y2 - y1 + 1) color ++ drop (y2 + 1) (cb !! x1)] ++ drop (x1 + 1) cb)
    | otherwise =
        changeColor x1 y1 x1 y2 color
      $ changeColor (x1 + 1) y1 x2 y2 color (strs, cb)
       