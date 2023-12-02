module CardGame.CardGameMain where

import CardGame.Definitions
import CardGame.GameLogic

main :: IO ()
main = do
    (player1, player2) <- initGame
    runGame player1 player2
