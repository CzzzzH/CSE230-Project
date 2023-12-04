{-# LANGUAGE TemplateHaskell #-}

module Reversi_logic where

import qualified Brick.Main as M
-- import Brick
-- import Brick.BChan
-- import Brick.Widgets.Center
-- import Brick.Widgets.Border
-- import Brick.Widgets.Border.Style
import Brick.Types (Widget, EventM)
import Brick.Widgets.Core (str, vBox)
import Brick.AttrMap (attrMap)
import Brick.Util (on)
import Text.Read (readMaybe)
import Control.Monad (forever)
import Control.Lens (makeLenses)
import Control.Monad.IO.Class (liftIO)
import Debug.Trace
import System.Exit (exitSuccess)


-- 定义棋盘大小
boardSize :: Int
boardSize = 8

-- 定义棋子类型
data Disc = Black | White | Empty | CursorPos  -- 添加一个 Empty 选项作为占位符
    deriving (Eq, Show)

-- 定义棋盘格
type Cell = Disc 

-- 定义棋盘
type Board = [[Cell]]


-- 初始化棋盘
initBoard :: Board
initBoard = 
    let emptyRow = replicate boardSize Empty  -- 使用 Empty 填充整行
        middleRow1 = replicate 3 Empty ++ [White, Black] ++ replicate 3 Empty
        middleRow2 = replicate 3 Empty ++ [Black, White] ++ replicate 3 Empty
    in replicate 3 emptyRow ++ [middleRow1, middleRow2] ++ replicate 3 emptyRow


data GameState = GameState {
    _board :: Board, 
    _cursor :: Int,
    _turn :: Int,     
    _start :: Bool
}
makeLenses ''GameState

-- 游戏状态
-- data GameState = GameState { board :: Board }
-- data GameState = GameState { board :: Board, currentPlayer :: Disc }

-- 初始化游戏状态
-- initState :: GameState
-- -- initState = GameState initBoard
-- initState = GameState initBoard Black  -- 假设黑方先行


-- 渲染游戏界面
-- drawUI :: GameState -> Widget ()
-- drawUI gs = vBox([str $ boardToStr $ board gs])

-- 将棋盘转换为字符串以显示
-- boardToStr :: Board -> String
-- boardToStr = concatMap ((++ "\n") . concatMap cellToStr)
--   where
--     cellToStr Empty = ". "
--     cellToStr Black = "O "
--     cellToStr White = "X "

-- 主程序
-- main :: IO ()
-- main = M.simpleMain $ drawUI initState

-- main :: IO ()
-- main = runGame initState



-- 核心逻辑

type Position = (Int, Int)
-- 检查位置是否有效（在棋盘范围内）
isValidPos :: Position -> Board -> Bool
isValidPos (x, y) _ = x >= 0 && y >= 0 && x < boardSize && y < boardSize


-- 翻转棋盘上的棋子
flipDiscs :: Disc -> Position -> Board -> Board
flipDiscs player pos board = 
    let boardWithNewDisc = setDiscAt pos player board  -- 首先放置新棋子
    in foldl (flipLine player pos) boardWithNewDisc directions
    where directions = [(dx, dy) | dx <- [-1, 0, 1], dy <- [-1, 0, 1], not (dx == 0 && dy == 0)]

-- 沿一个方向翻转棋子
flipLine :: Disc -> Position -> Board -> (Int, Int) -> Board
flipLine player pos board dir
    | canCapture player pos dir board = flipLineHelper player (move pos dir) dir board
    | otherwise = board

-- 辅助函数，实际执行翻转操作
flipLineHelper :: Disc -> Position -> (Int, Int) -> Board -> Board
flipLineHelper player pos dir board =
    case getDiscAt pos board of
        disc
            | disc == player -> board -- 遇到了自己的棋子，停止翻转
            | disc == Empty -> board -- 遇到空位，停止翻转
            | otherwise -> 
                -- 翻转当前位置的棋子，并继续沿线翻转
                flipLineHelper player (move pos dir) dir (setDiscAt pos player board)


-- 检查位置是否可以落子-- 检查位置是否可以落子
isPlayablePos :: Disc -> Position -> Board -> Bool
isPlayablePos player (x, y) board =
    case getDiscAt (x, y) board of
        disc -> 
            disc == Empty && -- 确保选择的位置是空的
            any (\dir -> canCapture player (x, y) dir board) directions
    where directions = [(dx, dy) | dx <- [-1, 0, 1], dy <- [-1, 0, 1], not (dx == 0 && dy == 0)]



-- 检查指定方向上是否可以夹住对手的棋子
canCapture :: Disc -> Position -> (Int, Int) -> Board -> Bool
canCapture player pos dir board =
    let nextPos = move pos dir
        nextDisc = getDiscAt nextPos board
    in case nextDisc of
        oppDisc ->
            if oppDisc /= player && oppDisc /= Empty then
                checkLine player nextPos dir board
            else
                False

-- 沿着方向移动，检查是否最终遇到玩家自己的棋子
checkLine :: Disc -> Position -> (Int, Int) -> Board -> Bool
checkLine player pos dir board =
    case getDiscAt pos board of
        disc ->
            if disc == player then True
            else if disc == Empty then False
            else checkLine player (move pos dir) dir board



-- 检查指定方向上是否可以翻转对手的棋子
canFlip :: Disc -> Position -> Board -> (Int, Int) -> Bool
canFlip player pos board dir = 
    let nextDisc = getDiscAt (move pos dir) board
    in
    case nextDisc of
        disc -> 
            if disc /= player then
                canContinueFlipping player (move pos dir) dir board
            else
                False

-- 沿着方向移动，继续检查是否可以翻转
canContinueFlipping :: Disc -> Position -> (Int, Int) -> Board -> Bool
canContinueFlipping player pos dir board =
    let newPos = move pos dir
    in if not (isValidPos newPos board) then False  -- 防止移动到棋盘外
       else case getDiscAt newPos board of
            Empty -> False
            disc -> 
                if disc == player then True 
                else canContinueFlipping player newPos dir board

-- 获取指定位置的棋子
getDiscAt :: Position -> Board -> Disc
getDiscAt (x, y) board
    | isValidPos (x, y) board = (board !! x) !! y
    | otherwise = Empty



-- 设置指定位置的棋子
setDiscAt :: Position -> Disc -> Board -> Board
setDiscAt (x, y) disc board =
    take x board ++ [take y (board !! x) ++ [disc] ++ drop (y + 1) (board !! x)] ++ drop (x + 1) board

-- 移动到新位置
move :: Position -> (Int, Int) -> Position
move (x, y) (dx, dy) = (x + dx, y + dy)



-- 判断游戏是否结束以及胜负情况
checkGameOver :: Board -> Maybe (Maybe Disc, String)
checkGameOver board
    | not (anyMovesPossible Black board) && not (anyMovesPossible White board) = Just (winner, resultMessage)
    | otherwise = Nothing
  where
    blackCount = countDiscs Black board
    whiteCount = countDiscs White board
    winner
      | blackCount > whiteCount = Just Black
      | whiteCount > blackCount = Just White
      | otherwise = Nothing
    resultMessage = "Black: " ++ show blackCount ++ ", White: " ++ show whiteCount

-- 计算棋盘上特定棋子的数量
countDiscs :: Disc -> Board -> Int
countDiscs disc = sum . map (length . filter (== disc))

-- 检查特定玩家是否有可能的移动
anyMovesPossible :: Disc -> Board -> Bool
-- anyMovesPossible player board = any (isPlayablePos player) allPositions
anyMovesPossible player board = any (\pos -> isPlayablePos player pos board) allPositions
  where
    allPositions = [(x, y) | x <- [0..boardSize - 1], y <- [0..boardSize - 1]]

-- 解析输入为坐标
parseInput :: String -> EventM n GameState (Maybe Position)
parseInput input = do
    liftIO $ putStrLn $ "You entered: " ++ input -- 打印玩家的输入
    return $ case mapM readMaybe (words input) of
        Just [x, y] -> Just (x, y)
        _ -> Nothing

-- 检查落子是否有效
isValidMove :: Disc -> Position -> Board -> Bool
isValidMove player pos board = isValidPos pos board && isPlayablePos player pos board

-- 无效落子处理
invalidMove :: Disc -> Board -> EventM n GameState (Maybe Position)
invalidMove player board = do
    liftIO $ putStrLn "Invalid move. Please try again."
    getPlayerMove player board
