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
import Control.Monad (void, unless)
import Control.Lens

data GameState = GameState {
    _board :: [Int],
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
initialGameState = GameState {_board = replicate 64 0, _start = False}

startGame :: EventM n GameState ()
startGame = do
    gameState <- get 
    unless (_start gameState) $ do
        board %= updateBoard 0 0 3
        start .= True

updateBoard :: Int -> Int -> Int -> [Int] -> [Int]
updateBoard row col newValue xs = take idx xs ++ [newValue] ++ drop (idx + 1) xs
    where
        idx = row * 8 + col

drawCell :: Int -> Widget ()
drawCell 1 = str (" ○ ")
drawCell 2 = str (" ● ")
drawCell 3 = str (" X ")
drawCell _ = str ("   ")

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
handleEvent _ = return ()

drawUI :: GameState -> Widget ()
drawUI state = center 
    $ vLimit 64
    $ hLimit 64
    $ withBorderStyle unicode
    $ borderWithLabel (str "Reversi") 
    $ drawBoard 
    $ state

main :: IO ()
main = do
    eventChan <- newBChan 10
    let buildVty = VCP.mkVty Graphics.Vty.Config.defaultConfig
    initialVty <- buildVty
    void $ customMain initialVty buildVty (Just eventChan) app initialGameState
