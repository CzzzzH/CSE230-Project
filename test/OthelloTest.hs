{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use null" #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module OthelloTest where
import System.Random (newStdGen, randomR, StdGen,mkStdGen)
import Data.List ( (\\), delete , nub,subsequences,intersect)
import Test.QuickCheck
import Test.QuickCheck.Gen (oneof, listOf1)

import Othello.GameLogic (Disc(..), Cell, Board, GameState(..), Position, boardSize, initBoard, initState, isValidPos, flipDiscs, setDiscAt, getValidMoves, getScore, isGameOver)


import Test.HUnit

-- 测试初始化棋盘
testInitBoard = TestCase (assertEqual "Should initialize board with correct layout." expectedInitBoard initBoard)

-- 测试检查有效位置
testIsValidPos = TestList [
  TestCase (assertBool "Position (0,0) should be valid." (isValidPos (0, 0) initBoard)),
  TestCase (assertBool "Position (7,7) should be valid." (isValidPos (7, 7) initBoard)),
  TestCase (assertBool "Position (-1,0) should not be valid." (not $ isValidPos (-1, 0) initBoard)),
  TestCase (assertBool "Position (8,8) should not be valid." (not $ isValidPos (8, 8) initBoard))
  ]

-- 测试落子翻转棋子
testFlipDiscs = TestCase (assertEqual "Flipping discs should result in expected board state." expectedBoardAfterFlip (flipDiscs Black (3, 2) initBoard))

-- 测试检查可玩位置
testIsPlayablePos = TestCase (assertBool "Position (3,2) should be playable for Black." (isPlayablePos Black (3, 2) initBoard))

-- 测试游戏结束判断
testCheckGameOver = TestCase (assertEqual "Should determine game over correctly." expectedGameOver (checkGameOver initBoard))

-- 生成随机的 Othello 对局
generateRandomGame :: Int -> IO [Move]
generateRandomGame numberOfMoves = do
  gen <- newStdGen
  let moves = take numberOfMoves $ zip (cycle ['B', 'W']) (randomPositions gen)
  return moves

-- 生成随机位置序列
randomPositions :: StdGen -> [Position]
randomPositions gen = zip (randomRs (0, 7) gen) (randomRs (0, 7) gen)


-- 你需要自己定义 expectedInitBoard, expectedBoardAfterFlip 和 expectedGameOver
-- 他们应该是你期望的棋盘状态，根据 initBoard 和 flipDiscs 的逻辑。

tests = TestList [testInitBoard, testIsValidPos, testFlipDiscs, testIsPlayablePos, testCheckGameOver]
