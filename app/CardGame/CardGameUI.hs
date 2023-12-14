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
import Data.ByteString.Char8 as BS (pack, unpack)
import qualified Text.Parsec as P
import Text.Parsec.String (Parser)
import Control.Monad (when, unless)
import Control.Monad.IO.Class (liftIO)

import System.Random (randomRIO)    

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
    _isP1 :: Bool,
    _single :: Bool,
    _appConnect :: Socket,
    _exit :: Bool
}
makeLenses ''AppState

genInitAppState :: Bool -> Bool -> Socket -> IO AppState
genInitAppState isSingle isServer conn = do
    (playerA, playerB) <- initGame
    return AppState { _self = if isServer then playerA else playerB,
                      _oppo = if isServer then playerB else playerA,
                      _cursor = 0,
                      _isMyTurn = isServer,
                      _winner = -1,
                      _gameRound = 0,
                      _gameOver = False,
                      _restart = False,
                      _canvas = drawGame,
                      _isP1 = isServer,
                      _single = isSingle,
                      _appConnect = conn,
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
    $ D.drawText 29 3  "Game Start!" D.yellowAttr
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
    let p1 = _isP1 appState
    let myColor = if p1 then D.redAttr else D.greenAttr
    let myRow = if p1 then 8 else 20
    let oppoRow = if p1 then 20 else 8
    let oppoColor = if p1 then D.greenAttr else D.redAttr
    let cursorPos = _cursor appState

    -- Clear Cards
    canvas %= D.drawBox 8 15 5 9 D.whiteAttr
    canvas %= D.drawBox 8 35 5 9 D.whiteAttr
    canvas %= D.drawBox 8 55 5 9 D.whiteAttr
    canvas %= D.drawBox 20 15 5 9 D.whiteAttr
    canvas %= D.drawBox 20 35 5 9 D.whiteAttr
    canvas %= D.drawBox 20 55 5 9 D.whiteAttr

    -- Updates Player Cards
    let handLen = length $ hand $ _self appState
    when (handLen >= 1) $ do
        canvas %= D.drawBox myRow 15 5 9 (if cursorPos == 0 then D.cyanAttr else myColor)
        canvas %= D.drawText (myRow + 2) 19 (drawCard (hand (_self appState) !! 0)) (if cursorPos == 0 then D.cyanAttr else myColor)

    when (handLen >= 2) $ do
        canvas %= D.drawBox myRow 35 5 9 (if cursorPos == 1 then D.cyanAttr else myColor)
        canvas %= D.drawText (myRow + 2) 39 (drawCard (hand (_self appState) !! 1)) (if cursorPos == 1 then D.cyanAttr else myColor)

    when (handLen >= 3) $ do
        canvas %= D.drawBox myRow 55 5 9 (if cursorPos == 2 then D.cyanAttr else myColor)
        canvas %= D.drawText (myRow + 2) 59 (drawCard (hand (_self appState) !! 2)) (if cursorPos == 2 then D.cyanAttr else myColor)

    canvas %= D.drawText (myRow - 4) 3 (replicate 50 ' ') myColor
    canvas %= D.drawText (myRow - 4) 3 ("You => Hand Cards: " ++
                                    show (length $ hand $ _self appState) ++
                                    "   Deck Cards: " ++
                                    show (length $ deck $ _self appState))
                                    myColor

    canvas %= D.drawText (oppoRow - 4) 3 "Oppo => Hand Cards: ??? Deck Cards: ???" oppoColor

    -- Updates Won cards
    let won1Str
            | null (wonCards $ _self appState)  = " "
            | otherwise = foldr1 (++) $ intersperse " " $ map drawCard (wonCards $ _self appState)
    let won2Str
            | null (wonCards $ _oppo appState)  = " "
            | otherwise = foldr1 (++) $ intersperse " " $ map drawCard (wonCards $ _oppo appState)
    canvas %= D.drawText (myRow - 3) 3 ("      Card Wons:" ++ won1Str) myColor
    canvas %= D.drawText (oppoRow - 3) 3 ("      Card Wons:" ++ won2Str) oppoColor

    -- Updates Round Result
    when (_winner appState /= -1) $ do
        canvas %= D.drawText 28 3  (replicate 70 ' ') D.yellowAttr
        let (roundWinnerStr, roundWinnerColor)
                | _winner appState == 1 && p1 = (" You won this round!", D.yellowAttr)
                | _winner appState == 1 && not p1 = (" You lost this round!", D.yellowAttr)
                | _winner appState == 2 && not p1 = (" You won this round!", D.yellowAttr)
                | _winner appState == 2 && p1 = (" You lost this round!", D.yellowAttr)
                | otherwise = (" Draw!", D.yellowAttr)
        let y1 = 3 + length ("Round " ++ show (_gameRound appState) ++ ": ")
        let y2 = y1 + length ("You " ++ show (head $ chosenCard $ _self appState))
        let y3 = y2 + 4
        let y4 = y3 + length ("Oppo " ++ show (head $ chosenCard $ _oppo appState))
        canvas %= D.drawText 28 3 (replicate 50 ' ') D.yellowAttr
        canvas %= D.drawText 28 3 ("Round " ++ show (_gameRound appState) ++ ": ") D.yellowAttr
        canvas %= D.drawText 28 y1 ("You " ++ show (head $ chosenCard $ _self appState)) myColor
        canvas %= D.drawText 28 y2 " vs " D.yellowAttr
        canvas %= D.drawText 28 y3 ("Oppo " ++ show (head $ chosenCard $ _oppo appState)) oppoColor
        canvas %= D.drawText 28 y4 ("  " ++ roundWinnerStr) roundWinnerColor

    if _gameOver appState then do
        let total1 = length (wonCards $ _self appState)
        let total2 = length (wonCards $ _oppo appState)
        canvas %= D.drawText 29 3 (replicate 70 ' ') D.yellowAttr
        canvas %= D.drawText 29 3 "Game Over! " D.yellowAttr
        if total1 > total2 then do
            canvas %= D.drawText 29 14 "You win! Press [R] to start a new game" D.yellowAttr
        else if total2 > total1 then do
            canvas %= D.drawText 29 14 "You lose! Press [R] to start a new game" D.yellowAttr
        else do
            canvas %= D.drawText 29 14 "Draw! Press [R] to start a new game" D.yellowAttr
    else do
        -- Update Cursor (make everything white if it is not your turn)
        canvas %= D.drawText 29 3 "It's your turn! Choose one of your hand cards" D.yellowAttr
        unless (_isMyTurn appState) $ do
            canvas %= D.drawText 29 3 (replicate 70 ' ') D.yellowAttr
            canvas %= D.drawText 29 3 "Waiting for opponent's choice..." D.yellowAttr
            canvas %= D.changeColor myRow 5 (myRow + 5) 75 D.whiteAttr

changeCursor :: Int -> EventM n AppState ()
changeCursor offset = do
    appState <- get
    unless (_gameOver appState) $ do
        let currentPlayer = _self appState
        cursor .= (_cursor appState + offset) `mod` (length $ hand currentPlayer)
        updateCanvas

kRESTART, kEXIT :: String
kRESTART = "RESTART"
kEXIT = "EXIT"

runGame :: Card -> EventM n AppState ()
runGame card = do
    appState <- get

    let myTurn = _isMyTurn appState
    let p1 = _isP1 appState
    let player = if myTurn then _self appState else _oppo appState

    -- Only update my own cards
    if myTurn then do
        let newHand = delete card (hand player)
        let newPlayer = player { hand = newHand, chosenCard = [card] }
        self .= getCard newPlayer
        cursor .= 0
        if not p1 then finishRound else do return ()
    else do
        if _single appState then do
            let newHand = delete card (hand player)
            let newPlayer = player { hand = newHand, chosenCard = [card] }
            oppo .= getCard newPlayer
        else do
            oppo .= player { chosenCard = [card] }
        if p1 then finishRound else do return ()

finishRound :: EventM n AppState ()
finishRound = do
    appState <- get
    let p1 = _isP1 appState
    let playerA = if p1 then _self appState else _oppo appState
    let playerB = if p1 then _oppo appState else _self appState
    let cardA = head $ chosenCard playerA
    let cardB = head $ chosenCard playerB

    let (win1, win2) = compareCards cardA cardB
    let (newPlayerA, newPlayerB) = updateState playerA playerB cardA cardB win1 win2
    self .= if p1 then newPlayerA else newPlayerB
    oppo .= if not p1 then newPlayerA else newPlayerB

    if win1 then do winner .= 1
    else if win2 then do winner .= 2
    else do winner .= 0

    gameRound .= _gameRound appState + 1
    when (null (hand $ _self appState)) $ do 
        gameOver .= True
    updateCanvas
    winner .= -1

runSelfGame :: EventM n AppState ()
runSelfGame = do
    appState <- get
    if not (_isMyTurn appState) || _gameOver appState then do
        return ()
    else do
        let player = _self appState
        let card = hand player !! _cursor appState
        runGame card

        if _single appState then do
            let botPlayer = _oppo appState
            randomNum <- liftIO $ randomRIO (0, length (hand botPlayer) - 1)
            let botCard = hand botPlayer !! randomNum
            isMyTurn .= False
            runGame botCard
            isMyTurn .= True
        else do
            -- Tell the other side about your move
            liftIO $ do {
                _ <- send (_appConnect appState) (pack (drawCard card));
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
    appState <- get
    exit .= True
    if _single appState then do
        halt
    else do
        liftIO $ do {
            _ <- send (_appConnect appState) (pack (show kEXIT));
            return ()
        }

oppoExit :: EventM n AppState ()
oppoExit = do
    appState <- get
    if _exit appState then do
        halt
    else do
        liftIO $ do {
            _ <- send (_appConnect appState) (pack (show kEXIT));
            return ()
        }
        exit .= True
        halt

restartGame :: EventM n AppState ()
restartGame = do
    appState <- get
    restart .= True
    if _single appState then do
        halt
    else do
        liftIO $ do {
            _ <- send (_appConnect appState) (pack (show kRESTART));
            return ()
        }

oppoRestart :: EventM n AppState ()
oppoRestart = do
    appState <- get
    if _restart appState then do
        halt
    else do
        liftIO $ do {
            _ <- send (_appConnect appState) (pack (show kRESTART));
            return ()
        }
        restart .= True
        halt

handleEvent :: BrickEvent n MyEvent -> EventM n AppState ()
handleEvent (VtyEvent (V.EvKey V.KLeft [])) = changeCursor (-1)
handleEvent (VtyEvent (V.EvKey V.KRight [])) = changeCursor 1
handleEvent (VtyEvent (V.EvKey V.KEnter [])) = runSelfGame
handleEvent (VtyEvent (V.EvKey V.KEsc [])) = endGame
handleEvent (VtyEvent (V.EvKey (V.KChar 'R') [])) = restartGame
handleEvent (VtyEvent (V.EvKey (V.KChar 'r') [])) = restartGame
handleEvent (AppEvent (SocketEvent card)) = runOppoGame card
handleEvent (AppEvent RestartEvent) = oppoRestart
handleEvent (AppEvent ExitEvent) = oppoExit
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

runNetwork :: Bool -> Socket -> BChan MyEvent -> IO ()
runNetwork isSingle conn eventChan = do
  -- Read from the socket, parse messages, and send events to the UI
    unless isSingle $ do
        chaosMsg <- recv conn 1024
        let msg = unpack chaosMsg
        if msg == show kRESTART then do
            writeBChan eventChan RestartEvent
        else if msg == show kEXIT then do
            writeBChan eventChan ExitEvent
        else if msg /= "" then do
            case P.parse cardParser "" msg of
                Left _        -> return ()
                Right card -> do {
                    writeBChan eventChan (SocketEvent card) }
            runNetwork isSingle conn eventChan
        else
            runNetwork isSingle conn eventChan

main :: Bool -> Bool -> Socket -> IO Int
main isSingle isServer conn = do
    eventChan <- newBChan 10
    _ <- forkIO $ runNetwork isSingle conn eventChan
    let buildVty = VCP.mkVty Graphics.Vty.Config.defaultConfig
    initialVty <- buildVty
    initAppState <- genInitAppState isSingle isServer conn
    nextState <- customMain initialVty buildVty (Just eventChan) app initAppState
    if _restart nextState then do
        main isSingle isServer conn
    else
        return 0
