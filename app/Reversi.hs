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

import Reversi_logic.Board as Board
import Reversi_logic.Disc as Disc
import System.Exit (exitSuccess)
import Reversi_logic (move, initBoard, getPlayerMove, flipDiscs, checkGameOver, anyMovesPossible, currentPlayer)

-- data GameState = GameState {
--     _board :: [Int],
--     _cursor :: Int,
--     _turn :: Int,
--     _start :: Bool
-- }
-- makeLenses ''GameState

-- Data definition for GameState
data GameState = GameState {
    _board :: Board, 
    _cursor :: Int,
    _turn :: Int,     
    _start :: Bool
}
makeLenses ''GameState

-- New initial game state using the new board definition
initialGameState :: GameState
initialGameState = GameState {
    _board = initBoard, -- Initialize the board with initBoard
    _cursor = 0, 
    _turn = Black,      -- Starting turn can be Black
    _start = False
}


app :: App GameState e ()
app =
    App { appDraw = (:[]) . drawUI
        , appChooseCursor = neverShowCursor
        , appHandleEvent = handleEvent
        , appAttrMap = const $ attrMap V.defAttr []
        , appStartEvent = return ()
        }

-- initialGameState :: GameState
-- initialGameState = GameState {_board = replicate 64 (-1),
--                               _cursor = 0, 
--                               _turn = 0,
--                               _start = False}


-- move :: EventM n GameState ()
-- move = do
--     gameState <- get
--     when (_start gameState && (((_board gameState) !! (_cursor gameState)) < 10)) $ do
--         board %= updateBoard (_cursor gameState) (10 + (_turn gameState))
        

runGame :: EventM n GameState ()
runGame gameState = forever $ do
    -- 显示当前棋盘
    -- putStrLn $ drawUI gameState
    -- putStrLn $ boardToStr $ board gameState

    -- 获取玩家移动
    gameState <- get
    moveMaybe <- getPlayerMove (currentPlayer gameState) (board gameState)

    case moveMaybe of
        Just move -> do
            -- 玩家有有效的移动

            let newBoard = flipDiscs (currentPlayer gameState) move (board gameState)
            -- let nextPlayer = if currentPlayer gameState == Black then White else Black

            -- 检查游戏是否结束
            case checkGameOver newBoard of
                Just (winner, message) -> do
                    putStrLn message
                    if winner == Just Black then putStrLn "Black wins!"
                    else if winner == Just White then putStrLn "White wins!"
                    else putStrLn "It's a draw!"
                    putStrLn "The game is over! Please press ESC to exit to main manu!" 
                    -- exitSuccess  -- 结束游戏
                Nothing -> do
                    let newGameState = gameState { 
                        _board = newBoard, 
                        _turn =  1 - (_turn gameState),
                    }
                    -- assign newGameState to system gameState ? 
                    Error!!!!!!!!!!!!!!
                    -- runGame newGameState  -- 继续下一个回合

        Nothing -> do
            -- 当前玩家没有有效的移动
            let nextPlayer = if currentPlayer gameState == Black then White else Black
            if anyMovesPossible nextPlayer (board gameState) then do
                putStrLn $ "Player " ++ show (currentPlayer gameState) ++ " has no valid moves. Skipping turn."
                -- let newGameState = gameState { currentPlayer = nextPlayer }
                let newGameState = gameState { 
                    _board = newBoard, 
                    _turn =  1 - (_turn gameState),    
                }
                -- runGame newGameState
            else do
                -- 如果双方都没有有效的移动，游戏结束
                putStrLn "Neither player has valid moves. Game over."
                -- 可以在这里添加计算分数并宣布胜者的逻辑
                -- exitSuccess
                putStrLn "The game is over! Please press ESC to exit to main manu!" 

convert2Dto1D :: Int -> Int -> Int
convert2Dto1D x y = x * 8 + y

convertDiscToInt :: Disc -> Int
convertDiscToInt disc = case disc of
    Black -> 1
    White -> 0
    Empty -> -1


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
handleEvent (VtyEvent (V.EvKey (V.KEnter) [])) = runGame
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

