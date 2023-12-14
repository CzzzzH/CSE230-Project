module CardGame.GameLogic where

import CardGame.Definitions ( Player(..), Card, fullDeck )
import System.Random (newStdGen, randomR, StdGen)
import Data.List ( delete )

-- 从牌组选择一张牌并返回选择的牌和剩下的牌组
select :: [Card] -> StdGen -> (Card, [Card], StdGen)
select deck gen =  (card,new_deck,newGen) 
                    where
                        (i,newGen) = randomR (0, length deck - 1) gen
                        card = deck !! i
                        new_deck = delete card deck


-- 初始化手牌3张
initCard :: StdGen -> ([Card],[Card],StdGen)
initCard gen = ([card1, card2, card3],deck3, gen3) 
                 where   
                   (card1, deck1, gen1) = select fullDeck gen
                   (card2, deck2, gen2) = select deck1 gen1
                   (card3, deck3, gen3) = select deck2 gen2

-- 初始化两位玩家
initGame :: IO (Player, Player)
initGame = do
    gen <- newStdGen
    let (hand1, deck1,gen1) = initCard gen
    let (hand2, deck2,gen2) = initCard gen1
    let player1 = Player hand1 deck1 [][] [] gen1  
    let player2 = Player hand2 deck2 [][] [] gen2  
    return (player1, player2)

-- 比较卡牌大小
compareCards :: Card -> Card -> (Bool, Bool)
compareCards card1 card2
    | card1 > card2 = (True, False)
    | card2 > card1 = (False, True)
    | otherwise     = (False, False)


-- 补充玩家手牌
getCard :: Player -> Player
getCard player = 
    if length (hand player) < 3 && not (null (deck player))
    then player { hand = newCard : hand player, deck = newDeck, randomGen = newGen } 
    else player
    where
        (newCard, newDeck, newGen) = select(deck player) (randomGen player)

-- 更新两位玩家的状态
updateState :: Player -> Player -> Card -> Card -> Bool -> Bool -> (Player, Player)
updateState player1 player2 chosenCard1 chosenCard2 win1 win2 = 
    do
    let currentCard = [chosenCard1 ,chosenCard2]
        equalCard = pendingCards player1 ++ pendingCards player2
        allWonCards = currentCard ++ equalCard
    case (win1, win2) of
        (True, False) -> (player1 { wonCards = allWonCards ++ wonCards player1, pendingCards = [] },
                          player2 { pendingCards = [] })

        (False, True) ->(player1 { pendingCards = [] },
                        player2 { wonCards = allWonCards ++ wonCards player2, pendingCards = [] })

        (False, False)->(player1 { pendingCards = chosenCard1 : pendingCards player1 },
                        player2 { pendingCards = chosenCard2 : pendingCards player2 })

--输入提示与检查
-- playerChooseCard :: Player -> IO Card
-- playerChooseCard player = do
--     let handSize = length (hand player)
--     putStrLn $ "Please choose one card in your hand（from 1 to " ++ show handSize ++")："
--     input <- getLine
--     case readMaybe input of
--         Just n | n >= 1 && n <= handSize -> return (hand player !! (n - 1))
--         _ -> do
--             putStrLn $ "invalid input，please choose number from 1 to " ++ show handSize ++ "!"
--             playerChooseCard player

--打印状态变化，如果有GUI的话应该就不需要了。
-- 0.初始状态 ->1.手牌减少1张，桌面显示选择的牌-> 2.比大小，若不同，则到winner的赢牌里，若相同，先放在一边->3.手牌补充一张（也是第二轮的初始状态））
-- printPlayer :: Player ->Player -> IO ()
-- printPlayer player1 player2 = do
--     let handSize = length (hand player1)
--     putStrLn "------------------------"
--     putStrLn $ "player1" ++ "  won cards:"++show(wonCards player1)
--     putStrLn $  concat $ replicate handSize "[*] "
--     if null (chosenCard player1)
--         then do putStrLn "[]"
--                 putStrLn $ "---"++show(pendingCards player1) ++ show(pendingCards player2)
--                 putStrLn "[]"
--         else do putStrLn $ show (chosenCard player1)
--                 putStrLn $ "---"++show(pendingCards player1) ++ show(pendingCards player2)
--                 putStrLn $ show (chosenCard player2)
--     putStrLn $  concat $ replicate handSize "[*] "
--     putStrLn $ "player2" ++ "  won cards:"++show(wonCards player2)
--     putStrLn "------------------------"

-- runGame :: Player -> Player -> IO ()
-- runGame player1 player2 = do

--     -- 检查游戏是否结束
--     if null (hand player1) && null (deck player1) 
--     then do
--         -- 游戏结束，显示结果
--         putStrLn "game ends........"
--         let total1 = length (wonCards player1)
--         let total2 = length (wonCards player2)
--         putStrLn $ "Number of cards won by player1 : " ++ show total1
--         putStrLn $ "Number of cards won by player2 : " ++ show total2
--         if total1 > total2
--         then putStrLn "Player1 wins!"
--         else if total2 > total1
--              then putStrLn "Player2 wins!"
--              else putStrLn "draw！"
--     else do
--         printPlayer player1 player2  --打印初始状态，同时也是上一轮末到状态
--         -- 玩家1选择一张牌
--         putStrLn "Player1's turn"
--         chosenCard1 <- playerChooseCard player1
--         let newHand1 = delete chosenCard1 (hand player1)

--         -- 玩家2选择一张牌
--         putStrLn "Player2's turn"
--         chosenCard2 <- playerChooseCard player2
--         let newHand2 = delete chosenCard2 (hand player2)

--         --putStrLn $ "player1 choose：" ++ show chosenCard1
--         --putStrLn $ "player2 choose：" ++ show chosenCard2
--         -- 比较两张牌
--         let (win1, win2) = compareCards chosenCard1 chosenCard2
        
--         --第一次状态改变：手牌减少一张，选择的卡牌显示
--         let newPlayer11 = player1 { hand = newHand1,chosenCard = [chosenCard1] }
--         let newPlayer21 = player2 { hand = newHand2,chosenCard = [chosenCard2] }
--         printPlayer newPlayer11 newPlayer21

--         -- 第二次状态改变：选择的卡牌要么进入winner的赢牌，要么变成pending状态等待下一轮
--         let (newPlayer12, newPlayer22) = updateState newPlayer11 { chosenCard = [] } 
--                                                      newPlayer21 { chosenCard = [] } 
--                                                      chosenCard1 chosenCard2 win1 win2
--         printPlayer newPlayer12 newPlayer22  

--         -- 通知玩家本轮比较结果
--         if win1
--         then putStrLn "Player1 wins the round!"
--         else if win2
--              then putStrLn "Player2 wins the round!"
--              else putStrLn "This round is a draw."
        
--         -- 第三次状态改变：卡组有牌的时候，手牌从2张补充到3张
--         let newPlayer13 = getCard newPlayer12
--         let newPlayer23= getCard newPlayer22
        
--         -- 使用更新后的玩家状态进行下一轮
--         runGame newPlayer13 newPlayer23

