{-# LANGUAGE TemplateHaskell #-}

module Reversi where

import Brick
import Brick.BChan
import Brick.Widgets.Center
import Brick.Widgets.Border
import Brick.Widgets.Border.Style

import qualified Graphics.Vty as V
import qualified Graphics.Vty.CrossPlatform as VCP
import Graphics.Vty.Config (defaultConfig)
import Data.List (intersperse)
import Control.Monad (void, when, unless)
import Control.Lens

data GameState = GameState {
    _board :: [Int],
    _cursor :: Int,
    _turn :: Int,
    _start :: Bool
}
makeLenses ''GameState

app :: App GameState e ()
app =
    App { appDraw = (:[]) . drawUI
        , appChooseCursor = neverShowCursor
        , appHandleEvent = handleEvent
        , appAttrMap = const $ attrMap V.defAttr []
        , appStartEvent = return ()
        }

initialGameState :: GameState
initialGameState = GameState {_board = replicate 64 (-1),
                              _cursor = 0, 
                              _turn = 0,
                              _start = False}

updateBoard :: Int -> Int -> [Int] -> [Int]
updateBoard idx newValue xs = take idx xs ++ [newValue] ++ drop (idx + 1) xs

updateBoardInc :: Int -> Int -> [Int] -> [Int]
updateBoardInc idx newValue xs = take idx xs ++ [(xs !! idx) + newValue] ++ drop (idx + 1) xs

startGame :: EventM n GameState ()
startGame = do
    gameState <- get
    unless (_start gameState) $ do
        board %= updateBoardInc (_cursor gameState) (10)
        start .= True

moveCursor :: Int -> EventM n GameState ()
moveCursor offset = do
    gameState <- get
    let newCursorRaw = (_cursor gameState) + offset
    let newCursor = if newCursorRaw < 0 
                        then newCursorRaw + 64
                    else if newCursorRaw > 63 
                        then newCursorRaw - 64
                    else newCursorRaw
    when (_start gameState) $ do
        board %= updateBoardInc (_cursor gameState) (-10)
        board %= updateBoardInc newCursor (10)
        cursor .= newCursor

move :: EventM n GameState ()
move = do
    gameState <- get
    when (_start gameState && (((_board gameState) !! (_cursor gameState)) < 10)) $ do
        board %= updateBoard (_cursor gameState) (10 + (_turn gameState))
        turn .= 1 - (_turn gameState)

drawCell :: Int -> Widget ()
drawCell (-1) = str ("   ")
drawCell 0    = str (" ○ ")
drawCell 1    = str (" ● ")
drawCell 9    = str (" X ")
drawCell 10   = str (" X ")
drawCell 11   = str (" X ")
drawCell _    = str ("   ")

drawRow :: Int -> GameState -> Widget ()
drawRow row boardState = hBox $ intersperse (str "║") $ map drawCell targetList
    where
        targetList = slice $ _board boardState
        slice xs = take 8 $ drop (8 * row) xs
        
drawBoard :: GameState -> Widget ()
drawBoard boardState = vBox $ intersperse (str "═══ ═══ ═══ ═══ ═══ ═══ ═══ ═══") [drawRow i boardState | i <- [0..7]]

handleEvent :: BrickEvent n e -> EventM n GameState ()
handleEvent (VtyEvent (V.EvKey (V.KEsc) [])) = halt
handleEvent (VtyEvent (V.EvKey (V.KChar 's') [])) = startGame
handleEvent (VtyEvent (V.EvKey (V.KChar 'S') [])) = startGame
handleEvent (VtyEvent (V.EvKey (V.KUp) [])) = moveCursor (-8)
handleEvent (VtyEvent (V.EvKey (V.KDown) [])) = moveCursor 8
handleEvent (VtyEvent (V.EvKey (V.KLeft) [])) = moveCursor (-1)
handleEvent (VtyEvent (V.EvKey (V.KRight) [])) = moveCursor 1
handleEvent (VtyEvent (V.EvKey (V.KEnter) [])) = move
handleEvent _ = return ()

drawUI :: GameState -> Widget ()
drawUI state = center 
    $ vLimit 64
    $ hLimit 64
    $ withBorderStyle unicode
    $ borderWithLabel (str "Reversi") 
    $ drawBoard state

main :: IO ()
main = do
    eventChan <- newBChan 10
    let buildVty = VCP.mkVty Graphics.Vty.Config.defaultConfig
    initialVty <- buildVty
    void $ customMain initialVty buildVty (Just eventChan) app initialGameState
