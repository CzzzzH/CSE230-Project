{-# LANGUAGE TemplateHaskell #-}

module Main where

import qualified CardGame.CardGameMain as C

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

import Constant

data AppState = AppState {
    _cursor :: Int,
    _currentApp :: Int
}
makeLenses ''AppState

app :: App AppState e ()
app =
    App { appDraw = (:[]) . drawUI
        , appChooseCursor = neverShowCursor
        , appHandleEvent = handleEvent
        , appAttrMap = const $ attrMap V.defAttr 
            [ (redAttr, fg V.red)
            , (greenAttr, fg V.green)
            , (yellowAttr, fg V.yellow)
            , (cyanAttr, fg V.cyan `V.withStyle` V.bold)
            ]
        , appStartEvent = return ()
        }

initialAppState :: AppState
initialAppState = AppState { _cursor = 0, _currentApp = 0}

drawText :: Int -> Int -> String-> [String] -> [String]
drawText row col text strs = 
    take row strs ++ [replace $ strs !! row] ++ drop (row + 1) strs
        where
            replace oriText = take col oriText ++ text ++ drop (col + length text) oriText

drawMenu :: AppState -> [String]
drawMenu s = drawText 18 25 "Welcome to Casual Games Arena!"
    $ drawText 20 25 "You can choose a game to play!"
    $ drawText 23 15 "╔═════════════╗                   ╔═════════════╗"
    $ drawText 24 15 "║             ║                   ║             ║"
    $ drawText 25 15 "║   Othello   ║                   ║  High  Low  ║"
    $ drawText 26 15 "║             ║                   ║             ║"
    $ drawText 27 15 "╚═════════════╝                   ╚═════════════╝"
    $ menu

setColoredStr :: AppState -> Int -> Int -> String -> Widget ()
setColoredStr s row col rawStr
    | row < 18 = withAttr redAttr $ str rawStr
    | row < 21 = withAttr yellowAttr $ str rawStr
    | col `div` 40 == _cursor s = withAttr cyanAttr $ str rawStr
    | otherwise = str rawStr

makeWidget :: AppState -> [String] -> Widget ()
makeWidget s strs = vBox [makeRows i | i <- [0..length strs - 1]]
    where
        makeRows i = hBox [makeCols j | j <- [0..length (strs !! i) - 1]]
            where
                makeCols j = setColoredStr s i j [strs !! i !! j]

drawUI :: AppState -> Widget ()
drawUI s = center 
    $ vLimit 200
    $ hLimit 200
    $ withBorderStyle unicode
    $ border
    $ makeWidget s
    $ drawMenu s

changeCursor :: EventM n AppState ()
changeCursor = do
    appState <- get
    cursor .= 1 - _cursor appState

startGame :: EventM n AppState ()
startGame = do
    appState <- get
    currentApp .= _cursor appState + 1
    when (_cursor appState == 1)
        halt

handleEvent :: BrickEvent n e -> EventM n AppState ()
handleEvent (VtyEvent (V.EvKey V.KEsc [])) = halt
handleEvent (VtyEvent (V.EvKey V.KLeft [])) = changeCursor
handleEvent (VtyEvent (V.EvKey V.KRight [])) = changeCursor
handleEvent (VtyEvent (V.EvKey V.KEnter [])) = startGame
handleEvent _ = return ()

main :: IO ()
main = do
    eventChan <- newBChan 10
    let buildVty = VCP.mkVty Graphics.Vty.Config.defaultConfig
    initialVty <- buildVty
    finalState <- customMain initialVty buildVty (Just eventChan) app initialAppState
    when (_currentApp finalState == 2) $ C.main

