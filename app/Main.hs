{-# LANGUAGE TemplateHaskell #-}

module Main where

import qualified CardGame.CardGameUI as C

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

import qualified Draw as D

data AppState = AppState {
    _cursor :: Int,
    _currentApp :: Int,
    _canvas :: D.Canvas,
    _quit :: Bool
}
makeLenses ''AppState

app :: App AppState e ()
app =
    App { appDraw = (:[]) . drawUI
        , appChooseCursor = neverShowCursor
        , appHandleEvent = handleEvent
        , appAttrMap = const $ attrMap V.defAttr D.attrTable
        , appStartEvent = return ()
        }

initialAppState :: AppState
initialAppState = AppState { _cursor = 0, _currentApp = 0, _canvas = drawMenu,  _quit = False}

drawMenu :: D.Canvas
drawMenu = 
      D.drawText 18 25 "Welcome to Casual Games Arena!" D.yellowAttr
    $ D.drawText 20 25 "You can choose a game to play!" D.yellowAttr
    $ D.drawText 25 19 "Othello"   D.cyanAttr
    $ D.drawText 25 52 "High  Low" D.whiteAttr
    $ D.drawBox 23 15 5 15 D.cyanAttr
    $ D.drawBox 23 49 5 15 D.whiteAttr
    $ (D.menu, D.menuColorBoard)

drawUI :: AppState -> Widget ()
drawUI s = center 
    $ vLimit 200
    $ hLimit 200
    $ withBorderStyle unicode
    $ border
    $ D.makeWidget
    $ _canvas s

changeCursor :: EventM n AppState ()
changeCursor = do
    appState <- get
    canvas %= D.changeColor 23 15 27 63 D.whiteAttr
    if _cursor appState == 1 then do
        canvas %= D.changeColor 23 15 27 29 D.cyanAttr
    else do
        canvas %= D.changeColor 23 49 27 63 D.cyanAttr
    cursor .= 1 - _cursor appState

startGame :: EventM n AppState ()
startGame = do
    appState <- get
    currentApp .= _cursor appState + 1
    when (_cursor appState == 1)
        halt

quitApp :: EventM n AppState ()
quitApp = do
    currentApp .= -1
    halt

handleEvent :: BrickEvent n e -> EventM n AppState ()
handleEvent (VtyEvent (V.EvKey V.KLeft [])) = changeCursor
handleEvent (VtyEvent (V.EvKey V.KRight [])) = changeCursor
handleEvent (VtyEvent (V.EvKey V.KEnter [])) = startGame
handleEvent (VtyEvent (V.EvKey V.KEsc [])) = quitApp
handleEvent _ = return ()

runMenu :: AppState -> IO ()
runMenu currentState = do
    if _currentApp currentState == -1 then do
        return ()
    else if _currentApp currentState == 2 then do
        C.main
        runMenu currentState {_currentApp = 0}
    else do 
        eventChan <- newBChan 10
        let buildVty = VCP.mkVty Graphics.Vty.Config.defaultConfig
        initialVty <- buildVty
        nextState <- customMain initialVty buildVty (Just eventChan) app currentState
        runMenu nextState

main :: IO ()
main = do
    runMenu initialAppState


