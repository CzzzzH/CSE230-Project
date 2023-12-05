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
import Control.Monad.IO.Class (liftIO)
import Control.Lens

import Reversi_logic
-- import Reversi_logic.Board as Board
-- import Reversi_logic.Disc as Disc
import System.Exit (exitSuccess)
import Reversi_logic (move, initBoard, flipDiscs, checkGameOver, anyMovesPossible, parseInput, isValidMove, isPlayablePos, boardSize, Disc(..), Board, Position)

import Debug.Trace

-- data GameState = GameState {
--     _board :: [Int],
--     _cursor :: Int,
--     _turn :: Int,
--     _start :: Bool
-- }
-- makeLenses ''GameState

-- Data definition for GameState

cvrtTurnMovableCursor :: [Int] -> [Int] -> [Int]
cvrtTurnMovableCursor init1Didx cursorArr = [if idx `elem` init1Didx then 1 else cell | (cell, idx) <- zip cursorArr [0..]]

-- New initial game state using the new board definition
initialGameState :: GameState
initialGameState = GameState {
    _board = initBoard, -- Initialize the board with initBoard
    _cursor = 0, 
    _turn = 0,      -- Starting turn can be Black
    _start = False,
    _turn_possible_cursor = [(2,3),(3,2),(4,5),(5,4)]
}
-- makeLenses ''GameState


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
        
getPlayerMovablePos :: Disc -> EventM n GameState ()
getPlayerMovablePos player = do
    gameState <- get
    
    let possibleMoves = filter (\pos -> isPlayablePos player pos (_board gameState)) allPositions
        allPositions = [(x, y) | x <- [0..boardSize - 1], y <- [0..boardSize - 1]]

    if null possibleMoves
    then do
        liftIO $ putStrLn $ "Next Player " ++ show player ++ " has no valid moves."
        -- return Nothing
    else do
        liftIO $ putStrLn $ "Next Player " ++ show player ++ ", possible moves: " ++ show possibleMoves
        -- let possibleMoves1D = convert2DCoordsTo1DIndexes possibleMoves
        --  cvrtTurnMovableCursor possibleMoves1D (replicate 64 (-1))
        turn_possible_cursor .= possibleMoves
        liftIO $ putStrLn "Move your cursor to that position! Then press Enter to confirm."
        -- input <- getLine


parseActualMove :: EventM n GameState (Maybe Position)
parseActualMove = do
    gameState <- get
    let player = if (_turn gameState) == 0 then Black else White 
    let pos = convert1Dto2D (_cursor gameState)
    liftIO $ print pos
    -- case posMaybe of
        -- Just pos -> 
    -- liftIO $ putStrLn "Invalid move. Please try again."
    if isValidMove player pos (_board gameState)
    then return $ Just pos
    else do
        liftIO $ putStrLn "Invalid move. Please try again."
        return Nothing
        -- Nothing -> invalidMove player

runGame :: EventM n GameState ()
runGame = do
    -- 显示当前棋盘
    -- putStrLn $ drawUI gameState
    -- putStrLn $ boardToStr $ board gameState

    gameState <- get
    let currentPlayer = if (_turn gameState) == 0 then Black else White 
    
    -- _turn_possible_cursor 是否为空？
    if null (_turn_possible_cursor gameState)
    then do
        -- 当前玩家没有有效的移动
        let nextPlayer = if currentPlayer == Black then White else Black
        if anyMovesPossible nextPlayer (_board gameState) then do
            liftIO $ putStrLn $ "Player " ++ show currentPlayer ++ " has no valid moves. Skipping turn."
            turn .= 1 - (_turn gameState)

            -- 打印下一个玩家可以移动的所有位置
            let nextPlayer = if currentPlayer == Black then White else Black
            getPlayerMovablePos nextPlayer
            -- return Nothing
        else do
            -- 如果双方都没有有效的移动，游戏结束
            liftIO $ putStrLn "Neither player has valid moves. Game over."
            -- 可以在这里添加计算分数并宣布胜者的逻辑
            liftIO $ putStrLn "The game is over! Please press ESC to exit to main manu!" 
            -- return Nothing
    
    else do
        -- 当前玩家有可以落子位置，已经按下回车键，光标位置为尝试的落子位置
        moveMaybe <- parseActualMove 
        -- 光标位置转换为棋盘上的坐标，检查是否为有效落子，包括是否越界，否的话，是否在_turn_possible_cursor里面？

        case moveMaybe of
            Just move -> do
            -- if move != (-1, -1)
            -- then do
                -- 玩家有有效的移动，更新棋盘
                let newBoard = flipDiscs currentPlayer move (_board gameState)
            
                -- 检查游戏是否结束
                case checkGameOver newBoard of
                    Just (winner, message) -> do
                        liftIO $ putStrLn message
                        if winner == Just Black then liftIO $ putStrLn "Black wins!"
                        else if winner == Just White then liftIO $ putStrLn "White wins!"
                        else liftIO $ putStrLn "It's a draw!"
                        liftIO $ putStrLn "The game is over! Please press ESC to exit to main manu!" 
                        -- return Nothing
                        -- exitSuccess  -- 结束游戏
                    Nothing -> do
                        board .= newBoard
                        turn .= 1 - (_turn gameState)
                        
                        -- 打印下一个玩家可以移动的所有位置
                        let nextPlayer = if currentPlayer == Black then White else Black
                        getPlayerMovablePos nextPlayer
                        -- return Nothing
            Nothing -> return ()
        -- else return ()
        -- 玩家有可落子位置，但是输入了非法位置，等待下一次合法光标和回车
            -- Nothing -> return Nothing

-- convert2Dto1D :: Int -> Int -> Int
-- convert2Dto1D x y = x * 8 + y

convert1Dto2D :: Int -> (Int, Int)
convert1Dto2D idx =
    if idx == -1 then (-1, -1)
    else (idx `div` 8, idx `mod` 8)

convert2DMatrixTo1DArray :: [[a]] -> [a]
convert2DMatrixTo1DArray twoDArray = [item | row <- twoDArray, item <- row]

convert2DCoordsTo1DIndexes :: [(Int, Int)] -> [Int]
convert2DCoordsTo1DIndexes coords = [x * 8 + y | (x, y) <- coords]

convertDiscToInt :: Disc -> Int
convertDiscToInt disc = case disc of
    Black -> 1
    White -> 0
    Reversi_logic.Empty -> -1
    CursorPos -> 10

convertIntToDisc :: Int -> Disc
convertIntToDisc int = case int of
    1 -> Black
    0 -> White
    -1 -> Reversi_logic.Empty
    10 -> CursorPos
    9 -> CursorPos
    11 -> CursorPos
    _ -> Reversi_logic.Empty

-- updateBoard :: Int -> Int -> [Int] -> [Int]
-- updateBoard idx newValue xs = take idx xs ++ [newValue] ++ drop (idx + 1) xs

updateBoardInc :: Int -> Int -> Board -> Board
updateBoardInc idx newValue board =
    let (row, col) = convert1Dto2D idx
        newDisc = convertIntToDisc newValue
        updatedRow = take col (board !! row) ++ [newDisc] ++ drop (col + 1) (board !! row)
    in take row board ++ [updatedRow] ++ drop (row + 1) board


startGame :: EventM n GameState ()
startGame = do
    gameState <- get
    unless (_start gameState) $ do
        board %= updateBoardInc (_cursor gameState) (10)
        start .= True
        turn .= 0
        getPlayerMovablePos Black

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

-- drawRow :: Int -> GameState -> Widget ()
-- drawRow row boardState = hBox $ intersperse (str "║") $ map drawCell targetList
--     where
--         targetList = slice $ _board boardState
--         slice xs = take 8 $ drop (8 * row) xs

-- drawRow :: Int -> GameState -> Widget ()
-- drawRow row boardState = hBox $ intersperse (str "║") $ map drawCell (boardState ^. board !! row)
drawRow :: Int -> GameState -> Widget ()
drawRow row gameState = hBox $ intersperse (str "║") $ map (drawCell . convertDiscToInt) ((_board gameState) !! row)

        
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

