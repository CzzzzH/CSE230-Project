{-# LANGUAGE TemplateHaskell #-}

module CardGame.CardGameUI where

import CardGame.Definitions
import CardGame.GameLogic

import Brick
import Brick.BChan
import Brick.Widgets.Center
import Brick.Widgets.Border
import Brick.Widgets.Border.Style

import qualified Graphics.Vty as V
import qualified Graphics.Vty.CrossPlatform as VCP
import Graphics.Vty.Config (defaultConfig)
import Control.Lens

import qualified Draw as D
import Data.List ( delete, intersperse)

import Network.Socket
import Network.Socket.ByteString (send, recv)
import Control.Concurrent (forkIO)
import Control.Exception (bracket)
import Data.ByteString.Char8 as BS (pack, unpack)
import qualified Text.Parsec as P
import Text.Parsec.String (Parser)
import Control.Monad (forever)
import Control.Monad.IO.Class (liftIO)

data AppState = AppState {
    _self:: Player,
    _oppo :: Player,
    _cursor :: Int,
    _isMyTurn :: Bool,
    _winner :: Int,
    _gameRound :: Int,
    _gameOver :: Bool,
    _restart :: Bool,
    _canvas :: D.Canvas,
    _isServer :: Bool,
    _conn :: Socket,
    _exit :: Bool
}
makeLenses ''AppState

genInitAppState :: Bool -> Socket -> IO AppState
genInitAppState isServer conn = do
    (playerA, playerB) <- initGame
    return AppState { _self = playerA, 
                      _oppo = playerB, 
                      _cursor = 0, 
                      _isMyTurn = if isServer then False else True, 
                      _winner = -1, 
                      _gameRound = 0, 
                      _gameOver = False, 
                      _restart = False,
                      _canvas = drawGame,
                      _isServer = isServer,
                      _conn = conn,
                      _exit = False
                      }

app :: App AppState MyEvent ()
app =
    App { appDraw = (:[]) . drawUI
        , appChooseCursor = neverShowCursor
        , appHandleEvent = handleEvent
        , appAttrMap = const $ attrMap V.defAttr D.attrTable
        , appStartEvent = updateCanvas
        }

drawCard :: Card -> String
drawCard card = 
    case card of
        Two -> "2"
        Three -> "3"
        Four -> "4"
        Five -> "5"
        Six -> "6"
        Seven -> "7"
        Eight -> "8"
        Nine -> "9"
        Ten -> "10"
        Jack -> "J"
        Queen -> "Q"
        King -> "K"
        Ace -> "A"

data MyEvent = SocketEvent Card | RestartEvent | ExitEvent

drawGame :: D.Canvas
drawGame =
      D.drawText 1 3  "Game: High Low" D.yellowAttr
    $ D.drawText 2 3  "Hint: [Enter] Choose a card [R] Restart [Esc] Quit the game" D.yellowAttr
    $ D.drawText 4 3  "You => Hand Cards: 3   Deck Cards: 10" D.redAttr
    $ D.drawText 5 3  "      Card Wons: " D.redAttr
    $ D.drawText 16 3 "Oppo => Hand Cards: 3   Deck Cards: 10" D.greenAttr
    $ D.drawText 17 3 "      Card Wons: " D.greenAttr
    $ D.drawText 28 3 "Game Start!" D.yellowAttr
    $ D.drawBox 8 15 5 9 D.cyanAttr
    $ D.drawBox 8 35 5 9 D.redAttr
    $ D.drawBox 8 55 5 9 D.redAttr
    $ D.drawBox 20 15 5 9 D.whiteAttr
    $ D.drawBox 20 35 5 9 D.whiteAttr
    $ D.drawBox 20 55 5 9 D.whiteAttr
    $ (D.whiteBoard, D.whiteColorBoard)

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
    let isServer = _isServer appState
    let endCol = 14 + 20 * (length $ hand $ _self appState)
    canvas %= D.changeColor 8 5 12 75 D.whiteAttr
    canvas %= D.changeColor 20 5 24 75 D.whiteAttr

    -- Updates Player Cards
    canvas %= D.drawText 4 3 (replicate 50 ' ') D.redAttr
    canvas %= D.drawText 4 3 ("You => Hand Cards: " ++
                                    show (length $ hand $ _self appState) ++
                                    "   Deck Cards: " ++
                                    show (length $ deck $ _self appState))
                                    D.redAttr
    
    canvas %= D.drawText 10 19 (replicate 2 ' ') D.yellowAttr
    canvas %= D.drawText 10 39 (replicate 2 ' ') D.yellowAttr
    canvas %= D.drawText 10 59 (replicate 2 ' ') D.yellowAttr

    let handLen = length $ hand $ _self appState
    if handLen >= 1 then canvas %= D.drawText 10 19 (drawCard (hand (_self appState) !! 0)) D.cyanAttr else do return ()
    if handLen >= 2 then canvas %= D.drawText 10 39 (drawCard (hand (_self appState) !! 1)) D.redAttr else do return ()
    if handLen >= 3 then canvas %= D.drawText 10 59 (drawCard (hand (_self appState) !! 2)) D.redAttr else do return ()

    canvas %= D.drawText 16 3 (replicate 50 ' ') D.greenAttr
    canvas %= D.drawText 16 3 ("Oppo => Hand Cards: " ++
                                show (length $ hand $ _oppo appState) ++
                                "   Deck Cards: " ++
                                show (length $ deck $ _oppo appState))
                                D.greenAttr

    let won1Str 
            | null (wonCards $ _self appState)  = " "
            | otherwise = foldr1 (++) $ intersperse " " $ map drawCard (wonCards $ _self appState) 
    let won2Str 
            | null (wonCards $ _oppo appState)  = " "
            | otherwise = foldr1 (++) $ intersperse " " $ map drawCard (wonCards $ _oppo appState) 
    canvas %= D.drawText 5 3  ("      Card Wons:" ++ won1Str) D.redAttr
    canvas %= D.drawText 17 3 ("      Card Wons:" ++ won2Str) D.greenAttr

    -- Updates Round Result
    if _winner appState /= -1 then do
        let (roundWinnerStr, roundWinnerColor) 
                | _winner appState == 1 && not isServer = (" You win!", D.redAttr)
                | _winner appState == 1 && isServer = (" You lose!", D.redAttr)
                | _winner appState == 2 && isServer = (" You win!", D.redAttr)
                | _winner appState == 2 && not isServer = (" You lose!", D.redAttr)
                | otherwise = (" Draw!", D.pinkAttr)
        let y1 = 3 + length ("Round " ++ show (_gameRound appState) ++ ": ")
        let y2 = y1 + length ("You " ++ show (head $ chosenCard $ _self appState))
        let y3 = y2 + 4
        let y4 = y3 + length ("Oppo " ++ show (head $ chosenCard $ _oppo appState))
        canvas %= D.drawText 28 3 (replicate 50 ' ') D.yellowAttr
        canvas %= D.drawText 28 3 ("Round " ++ show (_gameRound appState) ++ ": ") D.yellowAttr
        canvas %= D.drawText 28 y1 ("You " ++ show (head $ chosenCard $ _self appState)) D.redAttr
        canvas %= D.drawText 28 y2 " vs " D.yellowAttr
        canvas %= D.drawText 28 y3 ("Oppo " ++ show (head $ chosenCard $ _oppo appState)) D.greenAttr
        canvas %= D.drawText 28 y4 ("  " ++ roundWinnerStr) roundWinnerColor
    else do
        return ()

    if _gameOver appState then do
        let total1 = length (wonCards $ _self appState)
        let total2 = length (wonCards $ _oppo appState)
        canvas %= D.drawText 29 3 (replicate 50 ' ') D.yellowAttr
        canvas %= D.drawText 29 3 "Game Over! " D.yellowAttr
        if total1 > total2 then do
            canvas %= D.drawText 29 14 "You " D.redAttr 
            canvas %= D.drawText 29 22 "win! Press [R] to start a new game" D.yellowAttr
        else if total2 > total1 then do
            canvas %= D.drawText 29 14 "You " D.greenAttr 
            canvas %= D.drawText 29 22 "lose! Press [R] to start a new game" D.yellowAttr
        else do
            canvas %= D.drawText 29 14 "Draw! " D.pinkAttr
            canvas %= D.drawText 29 20 "Press [R] to start a new game" D.yellowAttr
    else do
        -- Update Cursor
        if _isMyTurn appState then do
            canvas %= D.changeColor 8 5 12 endCol D.redAttr
            let startRowCursor = 8
            let startColCursor = 15 + 20 * _cursor appState
            canvas %= D.changeColor startRowCursor startColCursor (startRowCursor + 4) (startColCursor + 8) D.cyanAttr
        else do
            return ()

changeCursor :: Int -> EventM n AppState ()
changeCursor offset = do
    appState <- get
    if _gameOver appState then do
        return ()
    else do
        let currentPlayer = _self appState
        cursor .= (_cursor appState + offset) `mod` (length $ hand currentPlayer)
        updateCanvas

kRESTART, kEXIT :: String
kRESTART = "RESTART"
kEXIT = "EXIT"

runGame :: Card -> EventM n AppState ()
runGame card = do
    appState <- get

    let isMyTurn = _isMyTurn appState
    let isServer = _isServer appState
    let player = if isMyTurn then _self appState else _oppo appState

    -- 只更新自己的手牌
    if isMyTurn then do
        let newHand = delete card (hand player)
        let newPlayer = player { hand = newHand, chosenCard = [card] }
        self .= getCard newPlayer
        cursor .= 0
        if isServer then finishRound else do return ()
    else do 
        oppo .= player { chosenCard = [card] }
        if not isServer then finishRound else do return ()

finishRound :: EventM n AppState ()
finishRound = do
    appState <- get
    let isServer = _isServer appState
    let playerA = if isServer then _oppo appState else _self appState
    let playerB = if isServer then _self appState else _oppo appState
    let cardA = head $ chosenCard playerA
    let cardB = head $ chosenCard playerB

    let (win1, win2) = compareCards cardA cardB
    let (newPlayerA, newPlayerB) = updateState playerA playerB cardA cardB win1 win2
    self .= if isServer then newPlayerB else newPlayerA
    oppo .= if not isServer then newPlayerB else newPlayerA

    if win1 then do winner .= 1
    else if win2 then do winner .= 2
    else do winner .= 0

    gameRound .= _gameRound appState + 1
    if null (hand $ _self appState) then do gameOver .= True
    else do return ()
    updateCanvas
    winner .= -1


runSelfGame :: EventM n AppState ()
runSelfGame = do
    appState <- get
    
    if not (_isMyTurn appState) then do
        return ()
    else if _gameOver appState then do
        return ()
    else do
        let player = _self appState
        let card = hand player !! _cursor appState
        -- turn .= _iAm appState

        runGame card
        -- tell the other side about your move
        liftIO $ do {
            _ <- send (_conn appState) (pack (drawCard card));
            return ()
        }
        isMyTurn .= False
    updateCanvas

runOppoGame :: Card -> EventM n AppState ()
runOppoGame card = do
    runGame card
    isMyTurn .= True
    updateCanvas

endGame :: EventM n AppState ()
endGame = do
    exit .= True
    appState <- get
    liftIO $ do {
        _ <- send (_conn appState) (pack (show kEXIT));
        return ()
    }
    halt

oppoExit :: EventM n AppState ()
oppoExit = do
    exit .= True
    halt

restartGame :: EventM n AppState ()
restartGame = do
    appState <- get
    liftIO $ do {
        _ <- send (_conn appState) (pack (show kRESTART));
        return ()
    }
    restart .= True
    halt

oppoRestart :: EventM n AppState ()
oppoRestart = do
    restart .= True
    -- to trigger updateCanvas
    changeCursor 0
    halt

handleEvent :: BrickEvent n MyEvent -> EventM n AppState ()
handleEvent (VtyEvent (V.EvKey V.KLeft [])) = changeCursor (-1)
handleEvent (VtyEvent (V.EvKey V.KRight [])) = changeCursor 1
handleEvent (VtyEvent (V.EvKey V.KEnter [])) = runSelfGame
handleEvent (VtyEvent (V.EvKey V.KEsc [])) = endGame
handleEvent (VtyEvent (V.EvKey (V.KChar 'R') [])) = restartGame
handleEvent (VtyEvent (V.EvKey (V.KChar 'r') [])) = restartGame
handleEvent (AppEvent (SocketEvent card)) = runOppoGame card
handleEvent (AppEvent (RestartEvent)) = oppoRestart
handleEvent (AppEvent (ExitEvent)) = oppoExit
handleEvent _ = return ()

twoParser :: Parser Card
twoParser = do { P.string "2"; return Two }

cardParser :: Parser Card
cardParser = P.try
    (do { P.string "2"; return Two })  P.<|>
    (do { P.string "3"; return Three }) P.<|>
    (do { P.string "4"; return Four } ) P.<|>
    (do { P.string "5"; return Five } ) P.<|>
    (do { P.string "6"; return Six }  ) P.<|>
    (do { P.string "7"; return Seven }) P.<|>
    (do { P.string "8"; return Eight }) P.<|>
    (do { P.string "9"; return Nine } ) P.<|>
    (do { P.string "10"; return Ten } ) P.<|>
    (do { P.string "J"; return Jack } ) P.<|>
    (do { P.string "K"; return King } ) P.<|>
    (do { P.string "Q"; return Queen }) P.<|>
    (do { P.string "A"; return Ace })

runNetwork :: Socket -> BChan MyEvent -> IO ()
runNetwork conn eventChan = do
  -- Read from the socket, parse messages, and send events to the UI
  forever $ do
    chaosMsg <- recv conn 1024
    let msg = unpack chaosMsg
    if msg == show kRESTART then do
        writeBChan eventChan (RestartEvent)
    else if msg == show kEXIT then do
        writeBChan eventChan (ExitEvent)
    else if msg /= "" then do
        case P.parse cardParser "" msg of
            Left _        -> return ()
            Right card -> do {
                writeBChan eventChan (SocketEvent card) }
    else return ()

main :: Bool -> Socket -> IO Int
main isServer conn = do
    eventChan <- newBChan 10
    forkIO $ runNetwork conn eventChan
    let buildVty = VCP.mkVty Graphics.Vty.Config.defaultConfig
    initialVty <- buildVty
    initAppState <- genInitAppState isServer conn
    nextState <- customMain initialVty buildVty (Just eventChan) app initAppState
    if _restart nextState then do
        main isServer conn
    else if _exit nextState then do
        return (-1)
    else do
        return 0
