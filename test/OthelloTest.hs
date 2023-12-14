{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use null" #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module OthelloTest where
import System.Random (newStdGen, randomR, StdGen,mkStdGen)
import Data.List ( (\\), delete , nub,subsequences,intersect)
import Data.Maybe (isJust, isNothing)
import Test.QuickCheck
import Test.QuickCheck.Gen (oneof, listOf1)
import Control.Monad (replicateM_)

import Othello.GameLogic
import Test.HUnit

-- 测试检查有效位置
testIsValidPos = let
  _board = initBoard
  in TestList [
    TestCase (assertBool "Position (0,0) should be valid." (isValidPos (0, 0) _board)),
    TestCase (assertBool "Position (7,7) should be valid." (isValidPos (7, 7) _board)),
    TestCase (assertBool "Position (-1,0) should not be valid." (not $ isValidPos (-1, 0) _board)),
    TestCase (assertBool "Position (8,8) should not be valid." (not $ isValidPos (8, 8) _board))
  ]

-- 生成测试棋盘布局
generateTestBoardFlipLine :: Board
generateTestBoardFlipLine = 
    let board1 = flipDiscs Black (2, 3) initBoard  -- 模拟第一步
        board2 = flipDiscs White (2, 2) board1      -- 模拟第二步
    in board2

-- 测试 flipLine 函数
testFlipLine :: Test
testFlipLine =
    let testBoard = setDiscAt (3, 2) Black generateTestBoardFlipLine
        expectedBoard = flipLine Black (3, 2) testBoard (0, 1) -- 假设翻转方向为 (0, 1)
        expectedBoardAfterFlipLine = 
            let emptyRow = replicate boardSize Empty  -- 使用 Empty 填充整行
                middleRow1 = replicate 2 Empty ++ [White, Black] ++ replicate 4 Empty
                middleRow2 = replicate 2 Empty ++ [Black, Black, Black] ++ replicate  3 Empty
                middleRow3 = replicate 3 Empty ++ [Black, White] ++ replicate 3 Empty
            in replicate 2 emptyRow ++ [middleRow1, middleRow2, middleRow3] ++ replicate 3 emptyRow
    in TestCase (assertEqual "flipLine should correctly flip a line." expectedBoardAfterFlipLine expectedBoard)


-- 生成测试棋盘布局
generateTestBoardFlipDisc :: Board
generateTestBoardFlipDisc = 
    let board1 = flipDiscs Black (2, 3) initBoard  -- 模拟第一步
        board2 = flipDiscs White (2, 2) board1      -- 模拟第二步
        board3 = flipDiscs Black (3, 2) board2      -- 模拟第三步
        board4 = flipDiscs White (2, 4) board3      -- 模拟第四步
    in board4
  
-- 测试 flipDiscs 函数
testFlipDisc :: Test
testFlipDisc = 
    let testBoard = setDiscAt (3, 5) Black generateTestBoardFlipDisc
        expectedBoard = flipDiscs Black (3, 5) testBoard
        emptyRow = replicate boardSize Empty  -- 使用 Empty 填充整行
        middleRow1 = replicate 2 Empty ++ [White, White, White] ++ replicate 3 Empty
        middleRow2 = replicate 2 Empty ++ [Black, Black, Black, Black] ++ replicate 2 Empty
        middleRow3 = replicate 3 Empty ++ [Black, White] ++ replicate 3 Empty
        expectedBoardAfterFlipDisc = replicate 2 emptyRow ++ [middleRow1, middleRow2, middleRow3] ++ replicate 3 emptyRow
    in TestCase (assertEqual "flipDiscs should correctly flip discs." expectedBoardAfterFlipDisc expectedBoard)

-- -- 测试落子翻转棋子
-- testFlipDiscs = TestCase (assertEqual "Flipping discs should result in expected board state." expectedBoardAfterFlipLine (flipDiscs Black (3, 2) initBoard))

-- 测试检查可玩位置
testIsPlayablePos = TestCase (assertBool "Position (3,2) should be playable for Black." (isPlayablePos Black (3, 2) initBoard))

gameOverBoard :: Board
gameOverBoard =
    [ [Black, Black, Black, Black, Black, Black, Black, White]
    , [Black, Black, Black, Black, Black, Black, White, White]
    , [Black, Black, Black, Black, Black, White, Black, White]
    , [Black, Black, Black, Black, White, Black, White, White]
    , [Black, Black, Black, White, Black, Black, White, White]
    , [Black, Black, White, White, Black, Black, White, White]
    , [Black, Black, Black, White, White, White, Black, White]
    , [White, White, White, White, White, Black, White, Black]
    ]

-- 测试游戏结束判断（游戏应该结束）
testCheckGameOverTrue :: Test
testCheckGameOverTrue = TestCase $ do
    let result = checkGameOver gameOverBoard
    assertBool "Game should be over" $ isJust result
    let Just (winner, _) = result
    assertBool "There should be a winner or a draw" $ isJust winner

-- 测试游戏结束判断（游戏不应该结束）
testCheckGameOverFalse :: Test
testCheckGameOverFalse = TestCase $ do
    let result = checkGameOver initBoard
    assertBool "Game should not be over" $ isNothing result


-- -- 生成随机的 Othello 对局
generateRandomGame :: Test
generateRandomGame = TestCase $ do
    replicateM_ 1000 (runGameRandom initState 0)

-- -- 生成随机位置序列
-- randomPositions :: StdGen -> [Position]
-- randomPositions gen = zip (randomRs (0, 7) gen) (randomRs (0, 7) gen)


-- 你需要自己定义 expectedInitBoard, expectedBoardAfterFlip 和 expectedGameOver
-- 他们应该是你期望的棋盘状态，根据 initBoard 和 flipDiscs 的逻辑。

-- tests = TestList [testIsValidPos, testFlipLine, testFlipDisc, testIsPlayablePos, 
--                   testCheckGameOverTrue, testCheckGameOverFalse]