{-# LANGUAGE TemplateHaskell #-}

module Othello.OthelloUI where
import Othello.GameLogic

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

data AppState = AppState {
    _gameState :: GameState,
    _choice :: Position,
    _cursor :: Int,
    _winner :: Int,
    _noMove :: Bool,
    _gameOver :: Bool,
    _restart :: Bool,
    _canvas :: D.Canvas
}
makeLenses ''AppState

genInitAppState :: IO AppState
genInitAppState = do
    return AppState { _gameState = initState,
                      _choice = (-1, -1),
                      _cursor = -1, 
                      _winner = -1, 
                      _noMove = False,
                      _gameOver = False, 
                      _restart = False,
                      _canvas = drawGame}

app :: App AppState e ()
app =
    App { appDraw = (:[]) . drawUI
        , appChooseCursor = neverShowCursor
        , appHandleEvent = handleEvent
        , appAttrMap = const $ attrMap V.defAttr D.attrTable
        , appStartEvent = updateCanvas
        }

drawGame :: D.Canvas
drawGame =
      D.drawText 1 3  "Game: Othello" D.yellowAttr
    $ D.drawText 2 3  "Hint: [Enter] Choose a move [R] Restart [Esc] Quit the game" D.yellowAttr
    $ D.drawText 28 3 "Select a move from above options!" D.yellowAttr
    $ D.drawBoard
    $ (D.whiteBoard, D.whiteColorBoard)
    
drawUI :: AppState -> Widget ()
drawUI s = center
    $ vLimit 200
    $ hLimit 200
    $ withBorderStyle unicode
    $ border
    $ D.makeWidget
    $ _canvas s

drawOptions :: [Position] -> Int -> Int -> Disc -> D.Canvas -> D.Canvas
drawOptions [] _ _ _ c = c
drawOptions (p:ps) idx currentCursor player c = 
    let
        x = 24 + idx `div` 6
        y = 3 + 13 * (idx `mod` 6)
        (px, py) = p
        xBoard = 5 + px * 2
        yBoard = 26 + py * 4
        color = if idx == currentCursor then D.cyanAttr else D.pinkAttr
        colorBoard = if idx == currentCursor then D.cyanAttr else D.redAttr
        cell = "×"
    in
        D.drawText x y ("[" ++ show p ++ "]") color
      $ D.drawText xBoard yBoard cell colorBoard
      $ drawOptions ps (idx + 1) currentCursor player c
 
drawDiscs :: Board -> Int -> D.Canvas -> D.Canvas
drawDiscs b idx c 
    | idx >= boardSize * boardSize = c
    | otherwise =
        let
            (px, py) = (idx `div` boardSize, idx `mod` boardSize)
            xBoard = 5 + px * 2
            yBoard = 26 + py * 4
            cell = if (b !! px !! py == White) then "○"
                   else if (b !! px !! py == Black) then "●"
                   else " "
        in
            D.drawText xBoard yBoard cell D.whiteAttr
            $ drawDiscs b (idx + 1) c

updateCanvas :: EventM n AppState ()
updateCanvas = do
    appState <- get
    let p = currentPlayer $ _gameState appState
    let b = board $ _gameState appState
    if _gameOver appState then do
        let winnerStr = if _winner appState == 1 then "Black win  " else if _winner appState == 2 then "White win  " else "Draw  "
        canvas %= D.drawWhiteBox 24 3 6 75 
        canvas %= D.drawBoard
        canvas %= drawDiscs (board $ _gameState appState) 0
        canvas %= D.drawText 28 3 "Game Over!" D.yellowAttr
        canvas %= D.drawText 28 15 (winnerStr ++ "Press [R] to start a new game") D.yellowAttr
    else do
        let playerName = if p == Black then "Black" else "White"
        canvas %= D.drawText 22 3 (replicate 70 ' ') D.yellowAttr
        canvas %= D.drawText 22 3 ("Current Player: " ++ playerName ++ "!  Valid moves:") D.yellowAttr
        
        let allPositions = [(x, y) | x <- [0..boardSize - 1], y <- [0..boardSize - 1]]
        let possibles = filter (\pos -> isPlayablePos p pos b) allPositions
        if null possibles
        then do
            cursor .= 0
            choice .= (-1, -1)
            canvas %= D.drawText 24 3 (replicate 70 ' ') D.yellowAttr
            canvas %= D.drawText 24 3 "No valid moves! Press [Enter] to skip your move" D.yellowAttr
            noMove .= True
        else do
            let newCursor = _cursor appState `mod` (length $ possibles)
            choice .= (possibles !! newCursor)
            cursor .= newCursor
            canvas %= D.drawWhiteBox 24 3 4 75 
            canvas %= D.drawBoard
            canvas %= drawDiscs (board $ _gameState appState) 0
            canvas %= drawOptions possibles 0 newCursor p
            noMove .= False

changeCursor :: Int -> EventM n AppState ()
changeCursor offset = do
    appState <- get
    cursor .= (_cursor appState) + offset
    updateCanvas
    return ()

runGame :: EventM n AppState ()
runGame = do
    appState <- get
    let state = _gameState appState
    let cp = currentPlayer state
    let newPlayer = if cp == Black then White else Black
    if _gameOver appState then do
        return ()
    else if _noMove appState then do
        let newGameState = state { currentPlayer = newPlayer }
        gameState .= newGameState
        if anyMovesPossible newPlayer (board state) then do
            return ()
        else do
            winner .= 0
            gameOver .= True
        updateCanvas
    else do
        let (x, y) = _choice appState
        let newBoard = flipDiscs cp (x, y) (board state)
        let newGameState = state { board = newBoard, currentPlayer = newPlayer }
        gameState .= newGameState
        case checkGameOver newBoard of
            Just (gameWinner, _) -> do
                gameOver .= True
                if gameWinner == Just Black then winner .= 1
                else if gameWinner == Just White then winner .= 2
                else winner .= 0
            Nothing -> do
                return ()
        updateCanvas

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
handleEvent (VtyEvent (V.EvKey V.KUp [])) = changeCursor (-6)
handleEvent (VtyEvent (V.EvKey V.KDown [])) = changeCursor 6
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
