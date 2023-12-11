{-# LANGUAGE TemplateHaskell #-}

module Main where

import qualified CardGame.CardGameUI as C
import qualified Othello.OthelloUI as O

import Brick
    ( App(..),
      EventM,
      Widget,
      BrickEvent(VtyEvent),
      get,
      neverShowCursor,
      attrMap,
      vLimit,
      hLimit,
      withBorderStyle,
      halt,
      customMain )
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

import Network.Socket
import Network.Socket.ByteString (send, recv)
import Control.Concurrent (forkIO)
import Control.Exception (bracket)
import Data.ByteString.Char8 as BS (pack, unpack)

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

runMenu :: Socket -> AppState -> IO ()
runMenu conn currentState = do
    if _currentApp currentState == -1 then do
        return ()
    else if _currentApp currentState == 1 then do
        send conn (pack "1")
        O.main
        runMenu conn currentState {_currentApp = 0}
    else if _currentApp currentState == 2 then do
        C.main
        runMenu conn currentState {_currentApp = 0}
    else do 
        eventChan <- newBChan 10
        let buildVty = VCP.mkVty Graphics.Vty.Config.defaultConfig
        initialVty <- buildVty
        nextState <- customMain initialVty buildVty (Just eventChan) app currentState
        runMenu conn nextState

startServer :: IO ()
startServer = withSocketsDo $ do
    addr <- resolve "127.0.0.1" "8080"
    sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
    bind sock (addrAddress addr)
    listen sock 5
    putStrLn "Server listening on port 8080"

    -- only serve one connection
    (conn, clientAddr) <- accept sock
    putStrLn $ "Established connection with " ++ show clientAddr
    handleClient conn

resolve :: String -> String -> IO AddrInfo
resolve host port = do
    let hints = defaultHints { addrSocketType = Stream }
    addr:_ <- getAddrInfo (Just hints) (Just host) (Just port)
    return addr

handleClient :: Socket -> IO ()
handleClient conn = do
    -- recv the gameID
    chaosMsg <- recv conn 4096
    let msg = unpack chaosMsg
    if msg /= "1" && msg /= "2"
        then do
            -- putStrLn $ "Received unexpected message from client: " ++ msg 
            handleClient conn 
        else do
            let gameID = read msg
            putStrLn $ "Client chose" ++ if gameID == 1 then "Othello" else "Card Game"
            let initialAppState = AppState { _cursor = 0, _currentApp = gameID, _canvas = drawMenu,  _quit = False}
            runMenu conn initialAppState

startClient :: IO ()
startClient = do
    putStrLn "Please input Server IP address:"
    host <- getLine
    putStrLn "Please input Server port number:"
    port <- getLine

    addr <- resolve host port
    conn <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
    connect conn (addrAddress addr)  

    let initialAppState = AppState { _cursor = 0, _currentApp = 0, _canvas = drawMenu,  _quit = False}
    runMenu conn initialAppState

main :: IO ()
main = do
    putStrLn "Choose mode: (1) Server, (2) Client"
    mode <- getLine
    case mode of
        "1" -> startServer
        "2" -> startClient
        _  -> do { 
            putStrLn "Invalid mode. Please choose again: (1) Server, (2) Client";
            main
        }
