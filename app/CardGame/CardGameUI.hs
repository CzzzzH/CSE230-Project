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

data AppState = AppState {
    _player1 :: Player,
    _player2 :: Player,
    _cursor :: Int,
    _turn :: Int,
    _winner :: Int,
    _gameRound :: Int,
    _gameOver :: Bool,
    _restart :: Bool,
    _canvas :: D.Canvas
}
makeLenses ''AppState

genInitAppState :: IO AppState
genInitAppState = do
    (playerA, playerB) <- initGame
    return AppState { _player1 = playerA, 
                      _player2 = playerB, 
                      _cursor = 0, 
                      _turn = 1, 
                      _winner = -1, 
                      _gameRound = 0, 
                      _gameOver = False, 
                      _restart = False,
                      _canvas = drawGame}

app :: App AppState e ()
app =
    App { appDraw = (:[]) . drawUI
        , appChooseCursor = neverShowCursor
        , appHandleEvent = handleEvent
        , appAttrMap = const $ attrMap V.defAttr D.attrTable
        , appStartEvent = return ()
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

drawGame :: D.Canvas
drawGame =
      D.drawText 1 3  "Game: High Low" D.yellowAttr
    $ D.drawText 2 3  "Hint: [Enter] Choose a card [R] Restart [Esc] Quit the game" D.yellowAttr
    $ D.drawText 4 3  "P1 => Hand Cards: 3   Deck Cards: 10" D.redAttr
    $ D.drawText 5 3  "      Card Wons: " D.redAttr
    $ D.drawText 16 3 "P2 => Hand Cards: 3   Deck Cards: 10" D.greenAttr
    $ D.drawText 17 3 "      Card Wons: " D.greenAttr
    $ D.drawText 28 3 "Game Start!" D.yellowAttr
    $ D.drawText 10 19 "I" D.cyanAttr
    $ D.drawText 10 38 "I I" D.redAttr
    $ D.drawText 10 57 "I I I" D.redAttr
    $ D.drawText 22 19 "I" D.whiteAttr
    $ D.drawText 22 38 "I I" D.whiteAttr
    $ D.drawText 22 57 "I I I" D.whiteAttr
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
    let endCol1 = 14 + 20 * (length $ hand $ _player1 appState)
    let endCol2 = 14 + 20 * (length $ hand $ _player2 appState)
    canvas %= D.changeColor 8 5 12 75 D.whiteAttr
    canvas %= D.changeColor 20 5 24 75 D.whiteAttr

    -- Updates Player Cards
    canvas %= D.drawText 4 3 (replicate 50 ' ') D.redAttr
    canvas %= D.drawText 4 3 ("P1 => Hand Cards: " ++
                                    show (length $ hand $ _player1 appState) ++
                                    "   Deck Cards: " ++
                                    show (length $ deck $ _player1 appState))
                                    D.redAttr
    canvas %= D.drawText 16 3 (replicate 50 ' ') D.greenAttr
    canvas %= D.drawText 16 3 ("P2 => Hand Cards: " ++
                                show (length $ hand $ _player2 appState) ++
                                "   Deck Cards: " ++
                                show (length $ deck $ _player2 appState))
                                D.greenAttr
    let won1Str 
            | null (wonCards $ _player1 appState)  = " "
            | otherwise = foldr1 (++) $ intersperse " " $ map drawCard (wonCards $ _player1 appState) 
    let won2Str 
            | null (wonCards $ _player2 appState)  = " "
            | otherwise = foldr1 (++) $ intersperse " " $ map drawCard (wonCards $ _player2 appState) 
    canvas %= D.drawText 5 3  ("      Card Wons:" ++ won1Str) D.redAttr
    canvas %= D.drawText 17 3 ("      Card Wons:" ++ won2Str) D.greenAttr

    -- Updates Round Result
    if _winner appState /= -1 then do
        let (roundWinnerStr, roundWinnerColor) 
                | _winner appState == 1 = (" P1 win!", D.redAttr)
                | _winner appState == 2 = (" P2 win!", D.greenAttr)
                | otherwise = (" Draw!", D.pinkAttr)
        let y1 = 3 + length ("Round " ++ show (_gameRound appState) ++ ": ")
        let y2 = y1 + length ("P1 " ++ show (head $ chosenCard $ _player1 appState))
        let y3 = y2 + 4
        let y4 = y3 + length ("P2 " ++ show (head $ chosenCard $ _player2 appState))
        canvas %= D.drawText 28 3 (replicate 50 ' ') D.yellowAttr
        canvas %= D.drawText 28 3 ("Round " ++ show (_gameRound appState) ++ ": ") D.yellowAttr
        canvas %= D.drawText 28 y1 ("P1 " ++ show (head $ chosenCard $ _player1 appState)) D.redAttr
        canvas %= D.drawText 28 y2 " vs " D.yellowAttr
        canvas %= D.drawText 28 y3 ("P2 " ++ show (head $ chosenCard $ _player2 appState)) D.greenAttr
        canvas %= D.drawText 28 y4 ("  " ++ roundWinnerStr) roundWinnerColor
    else do
        return ()

    if _gameOver appState then do
        let total1 = length (wonCards $ _player1 appState)
        let total2 = length (wonCards $ _player2 appState)
        canvas %= D.drawText 29 3 (replicate 50 ' ') D.yellowAttr
        canvas %= D.drawText 29 3 "Game Over! " D.yellowAttr
        if total1 > total2 then do
            canvas %= D.drawText 29 14 "Player1 " D.redAttr 
            canvas %= D.drawText 29 22 "win! Press [R] to start a new game" D.yellowAttr
        else if total2 > total1 then do
            canvas %= D.drawText 29 14 "Player2 " D.greenAttr 
            canvas %= D.drawText 29 22 "win! Press [R] to start a new game" D.yellowAttr
        else do
            canvas %= D.drawText 29 14 "Draw! " D.pinkAttr
            canvas %= D.drawText 29 20 "Press [R] to start a new game" D.yellowAttr
    else do
        -- Update Cursor
        if _turn appState == 1 then do
            canvas %= D.changeColor 8 5 12 endCol1 D.redAttr
            let startRowCursor = 8
            let startColCursor = 15 + 20 * _cursor appState
            canvas %= D.changeColor startRowCursor startColCursor (startRowCursor + 4) (startColCursor + 8) D.cyanAttr
        else do
            canvas %= D.changeColor 20 5 24 endCol2 D.greenAttr
            let startRowCursor = 20
            let startColCursor = 15 + 20 * _cursor appState
            canvas %= D.changeColor startRowCursor startColCursor (startRowCursor + 4) (startColCursor + 8) D.cyanAttr

changeCursor :: Int -> EventM n AppState ()
changeCursor offset = do
    appState <- get
    if _gameOver appState then do
        return ()
    else do
        let currentPlayer = if _turn appState == 1 then _player1 appState else _player2 appState
        cursor .= (_cursor appState + offset) `mod` (length $ hand currentPlayer)
        updateCanvas

runGame :: EventM n AppState ()
runGame = do
    appState <- get
    if _gameOver appState then do
        return ()
    else do 
        if _turn appState == 1 then do
            let playerA = _player1 appState
            let cardA = hand playerA !! _cursor appState
            let newHandA = delete cardA (hand playerA)
            player1 .= playerA { hand = newHandA, chosenCard = [cardA] }
            turn .= 2
            cursor .= 0
            updateCanvas
        else do
            let playerA = _player1 appState
            let playerB = _player2 appState
            let cardA = head $ chosenCard playerA
            let cardB = hand playerB !! _cursor appState
            let newHandB = delete cardB (hand playerB)
            let (win1, win2) = compareCards cardA cardB
            let (newPlayerA, newPlayerB) = updateState playerA playerB { hand = newHandB, chosenCard = [cardB] }
                                                    cardA cardB win1 win2

            if win1 then do winner .= 1
            else if win2 then do winner .= 2
            else do winner .= 0

            player1 .= getCard newPlayerA
            player2 .= getCard newPlayerB
            gameRound .= _gameRound appState + 1
            turn .= 1
            cursor .= 0
            if null (hand $ _player1 appState) && null (deck $ _player1 appState) then do gameOver .= True
            else do return ()
            updateCanvas
            winner .= -1

endGame :: EventM n AppState ()
endGame = do
    halt

restartGame :: EventM n AppState ()
restartGame = do
    restart .= True
    halt

handleEvent :: BrickEvent n e -> EventM n AppState ()
handleEvent (VtyEvent (V.EvKey V.KLeft [])) = changeCursor (-1)
handleEvent (VtyEvent (V.EvKey V.KRight [])) = changeCursor 1
handleEvent (VtyEvent (V.EvKey V.KEnter [])) = runGame
handleEvent (VtyEvent (V.EvKey V.KEsc [])) = endGame
handleEvent (VtyEvent (V.EvKey (V.KChar 'R') [])) = restartGame
handleEvent (VtyEvent (V.EvKey (V.KChar 'r') [])) = restartGame
handleEvent _ = return ()

main :: IO ()
main = do
    eventChan <- newBChan 10
    let buildVty = VCP.mkVty Graphics.Vty.Config.defaultConfig
    initialVty <- buildVty
    initAppState <- genInitAppState
    nextState <- customMain initialVty buildVty (Just eventChan) app initAppState
    if _restart nextState then do
        main
    else do
        return ()
