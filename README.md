# CSE230-Project: Reversi

# Group Member
+ Xiaoda Li (xil204@ucsd.edu)

# Proposal
## Overview of Reversi
In this project, we will implement the classic two-player strategy board game [Reversi](https://en.wikipedia.org/wiki/Reversi) that works on a $8\times 8$ grid. The two players are marked with color discs, typically black on one side and white on the other. The game always starts with a fixed initial grid. The players take turns place one disc of their color on the board, resulting in some of the opponent's discs being trapped between two of the player's discs and flipped to the capturing player's color. Whoever has the majority of their color discs on the board wins when the game ends with a fully occupied board.

## Goals
The project consists of the following goals organized as milestones.
#### M1
+ Basic setup with starter code (support `stack run` and `stack test`)
+ TUI support including parsing disc coordinates input and printing board 
+ Game logic in a standalone mode
#### M2
+ Refactor user interface with [Brick](https://github.com/jtdaugherty/brick/) library
+ Networking support for two-player mode
#### M3
+ Unit testing support either manually or using [quickcheck](https://hackage.haskell.org/package/QuickCheck)
+ GUI support for better interaction experience
