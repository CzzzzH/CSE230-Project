{-# LANGUAGE TemplateHaskell #-}

module Main where

import qualified CardGame.CardGameUI as C
import qualified Othello.OthelloUI as O

import Brick
import Brick.BChan
import Brick.Widgets.Center
import Brick.Widgets.Border
import Brick.Widgets.Border.Style

import qualified Graphics.Vty as V
import qualified Graphics.Vty.CrossPlatform as VCP
import Graphics.Vty.Config (defaultConfig)
import Data.List (intersperse, init)
import Control.Monad (void, when, unless, forever)
import Control.Lens

import Network.Socket
import Network.Socket.ByteString (send, recv)
import Control.Concurrent (forkIO)
import Control.Exception (bracket)
import Control.Monad.IO.Class (liftIO)

import Data.ByteString.Char8 as BS (pack, unpack)

import qualified Draw as D
import qualified Text.Parsec as P
import Data.Bool (Bool(False))
import Data.IntMap (update)
import qualified Draw as D

data AppState = AppState {
    _cursor :: Int,
    _currentApp :: Int,
    _canvas :: D.Canvas,
    _appConnect :: Socket,
    _appConnectHint :: String,
    _single :: Bool,
    _quit :: Bool
}
makeLenses ''AppState

data MenuEvent = StartEvent (Int)

app :: App AppState MenuEvent ()
app =
    App { appDraw = (:[]) . drawUI
        , appChooseCursor = neverShowCursor
        , appHandleEvent = handleEvent
        , appAttrMap = const $ attrMap V.defAttr D.attrTable
        , appStartEvent = updateCanvas
        }

drawMenu :: D.Canvas
drawMenu =
      D.drawText 19 25 "Welcome to Casual Games Arena!" D.yellowAttr
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

updateCanvas :: EventM n AppState ()
updateCanvas = do
    appState <- get

    canvas %= D.drawText 1 3 (_appConnectHint appState) D.yellowAttr
    if _single appState then do
        canvas %= D.drawText 2 3 "Please choose a game to play" D.yellowAttr
    else do
        canvas %= D.drawText 2 3 "Please choose a game to play or wait for the other player's choice" D.yellowAttr
    
    -- Update Cursor
    canvas %= D.changeColor 23 15 27 63 D.whiteAttr
    if _cursor appState == 0 then do
        canvas %= D.changeColor 23 15 27 29 D.cyanAttr
    else do
        canvas %= D.changeColor 23 49 27 63 D.cyanAttr
    

changeCursor :: EventM n AppState ()
changeCursor = do
    appState <- get
    if _currentApp appState /= 0 then do
        return ()
    else do
        cursor .= 1 - _cursor appState
        updateCanvas

oppoStart :: Int -> EventM n AppState ()
oppoStart appID = do
    appState <- get
    if _currentApp appState == 0 then do
        currentApp .= appID
        liftIO $ do {
                _ <- send (_appConnect appState) (pack $ show appID);
                return ()
            }
        halt
    else do
        halt
    
startGame :: EventM n AppState ()
startGame = do
    appState <- get
    currentApp .= _cursor appState + 1
    if _single appState then do
        halt
    else do
        liftIO $ do {
                _ <- send (_appConnect appState) (pack $ show $ _cursor appState + 1);
                return ()
            }
    
quitApp :: EventM n AppState ()
quitApp = do
    currentApp .= -1
    halt

handleEvent :: BrickEvent n MenuEvent -> EventM n AppState ()
handleEvent (VtyEvent (V.EvKey V.KLeft [])) = changeCursor
handleEvent (VtyEvent (V.EvKey V.KRight [])) = changeCursor
handleEvent (VtyEvent (V.EvKey V.KEnter [])) = startGame
handleEvent (VtyEvent (V.EvKey V.KEsc [])) = quitApp
handleEvent (AppEvent (StartEvent appID)) = oppoStart appID
handleEvent _ = return ()

runNetwork :: Bool -> Bool -> Socket -> BChan MenuEvent -> IO ()
runNetwork isSingle isServer conn eventChan = do
    unless isSingle $ do
        chaosMsg <- recv conn 4096
        let msg = unpack chaosMsg
        if msg /= "1" && msg /= "2"
            then do
                runNetwork isSingle isServer conn eventChan
            else do
                writeBChan eventChan (StartEvent (read msg))

runMenu :: Socket -> AppState -> Bool -> Bool -> IO ()
runMenu conn currentState isSingle isServer = do
    if _currentApp currentState == -1 then do
        return ()
    else if _currentApp currentState == 1 then do
        errorCode <- O.main isSingle isServer conn
        if errorCode == -1 then do return () else do
            runMenu conn currentState {_currentApp = 0} isSingle isServer 
    else if _currentApp currentState == 2 then do
        errorCode <- C.main isSingle isServer conn
        if errorCode == -1 then do return () else do
            runMenu conn currentState {_currentApp = 0} isSingle isServer
    else do
        eventChan <- newBChan 20
        _ <- forkIO $ runNetwork isSingle isServer conn eventChan
        let buildVty = VCP.mkVty Graphics.Vty.Config.defaultConfig
        initialVty <- buildVty
        nextState <- customMain initialVty buildVty (Just eventChan) app currentState
        runMenu conn nextState isSingle isServer 

myDefaultHost, myDefaultPort :: String
myDefaultHost = "127.0.0.1"
myDefaultPort = "8080"

startServer :: Bool -> IO ()
startServer isSingle = withSocketsDo $ do
    addr <- resolve myDefaultHost myDefaultPort
    sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
    bind sock (addrAddress addr)
    listen sock 5

    if isSingle then do
        conn2 <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
        connect conn2 (addrAddress addr)
    else do
        putStrLn $ "Server listening on port " ++ myDefaultPort
        
    -- only serve one connection
    (conn, clientAddr) <- accept sock
    let connHint
          | isSingle  = "Single-Player Mode"
          | otherwise = "Two-Players Mode (connected with " ++ show clientAddr ++ ")"
    let initialAppState = AppState { _cursor = 0, _currentApp = 0, _canvas = drawMenu,  _appConnect = conn,
                                     _appConnectHint = connHint, _single = isSingle, _quit = False}
    runMenu conn initialAppState isSingle True 

resolve :: String -> String -> IO AddrInfo
resolve host port = do
    let hints = defaultHints { addrSocketType = Stream }
    addr:_ <- getAddrInfo (Just hints) (Just host) (Just port)
    return addr

startClient :: IO ()
startClient = do
    putStrLn "Please input Server IP address: Press [ENTER] to use default config."
    hostOrNull <- getLine
    putStrLn "Please input Server port number: Press [ENTER] to use default config."
    portOrNull <- getLine

    let host = if hostOrNull == "" then myDefaultHost else hostOrNull
    let port = if portOrNull == "" then myDefaultPort else portOrNull

    addr <- resolve host port
    conn <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
    connect conn (addrAddress addr)
    let connHint = "Two Player Mode (connected with " ++ show (addrAddress addr) ++ ")"
    let initialAppState = AppState { _cursor = 0, _currentApp = 0, _canvas = drawMenu,  _appConnect = conn,
                                     _appConnectHint = connHint, _single = False, _quit = False}
    runMenu conn initialAppState False False

main :: IO ()
main = do
    putStrLn "Choose APP mode: (1) Server (2) Client (3) Single Player (Q) Quit APP"
    mode <- getLine
    case mode of
        "1" -> startServer False
        "2" -> startClient
        "3" -> startServer True
        "Q" -> return ()
        "q" -> return ()
        _  -> do {
            putStrLn "Invalid APP mode. Please choose again: (1) Server (2) Client (3) Single Player (Q) Quit APP";
            main
        }

