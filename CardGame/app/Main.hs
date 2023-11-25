import Definitions
import GameLogic

main :: IO ()
main = do
    (player1, player2) <- initGame
    runGame player1 player2
