{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use null" #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module CardTest where
import System.Random (newStdGen, randomR, StdGen,mkStdGen)
import Data.List ( (\\), delete , nub,subsequences,intersect)
import Test.QuickCheck
import Test.QuickCheck.Gen (oneof, listOf1)


-- 定义牌的类型
data Card = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace deriving (Eq, Ord, Enum, Show)

data Player = Player {
    hand :: [Card],       -- 手牌
    deck :: [Card],       -- 牌组
    chosenCard::[Card],   --当前局选择的牌
    wonCards :: [Card],   -- 赢得的牌
    pendingCards :: [Card], -- 平局时积累的牌
    randomGen :: StdGen    -- 随机生成器
} deriving (Eq,Show)

-- 初始化一个完整的牌组
fullDeck :: [Card]
fullDeck = [Two .. Ace]


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
        (newCard, newDeck, newGen) = select (deck player) (randomGen player)

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



-- Card的Arbitrary 实例
instance Arbitrary Card where
    arbitrary = elements [Two .. Ace]

-- 生成deck子集
subDeck :: Gen [Card]
subDeck = elements . subsequences $ [Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten, Jack, Queen, King, Ace]


-- 测试select。其他函数中有判断为空的情况，因此这里不考虑。
prop_select :: Int -> Property
prop_select seed = forAll subDeck $ \deck ->
  not (null deck) ==>
    let gen = mkStdGen seed
        (card, new_deck, _) = select deck gen
        expect = delete card deck
    in expect== new_deck && 
       length new_deck == length deck - 1 &&
       elem card deck

--测试初始化卡牌,是否抽出3张，新卡组比旧卡组少这三张卡牌
prop_initCard :: Int -> Bool
prop_initCard seed =
  let gen = mkStdGen seed
      (hand, new_deck, _) = initCard gen
      expect = fullDeck \\ hand
  in length (nub hand) == length hand &&                            
     all (`elem` fullDeck) hand &&     
     expect == new_deck &&
     length hand == 3  && 
     length new_deck == length fullDeck - 3

--测试比较卡牌大小是否正确
prop_compareCards :: Card -> Card -> Bool
prop_compareCards card1 card2
  | card1 > card2  = compareCards card1 card2 == (True, False)
  | card1 < card2  = compareCards card1 card2 == (False, True)
  | otherwise      = compareCards card1 card2 == (False, False)

--随机生成player，和实际情况略微有区别，但不影响测试
instance Arbitrary Player where
    arbitrary = do
        handCards <- sublistOf fullDeck `suchThat` (\x -> length x >= 1 && length x <= 3)
        deckCards <- sublistOf fullDeck
        chosenCards <- sublistOf fullDeck `suchThat` (\x -> length x == 1)
        wonCards <- sublistOf fullDeck
        pendingCards <- sublistOf fullDeck
        seed <- arbitrary
        let gen = mkStdGen seed
        return $ Player handCards deckCards chosenCards wonCards pendingCards gen

--测试补充卡牌，手牌是否加1，卡组是否少1 ，增加的牌来自旧卡组      
prop_getCard :: Player -> Bool
prop_getCard player =
  let player1 = getCard player
      oldHand = hand player
      oldDeck = deck player
      newHand = hand player1
      newDeck = deck player1
  in if length (oldHand) < 3 && not (null oldDeck)
     then length (newHand) == length (oldHand) + 1 &&
          length (newDeck) == length (oldDeck) - 1 &&
          (newHand \\ oldHand) `intersect` oldDeck == (newHand \\ oldHand) &&
          notElem (head (newHand \\ oldHand)) newDeck
     else player == player1


-- 生成 win1 和 win2 的值，
winGen :: Gen (Bool, Bool)
winGen = oneof [return (True, False), return (False, True), return (False, False)]

-- 测试update
prop_updateState :: Player -> Player -> Card -> Card -> Property
prop_updateState player1 player2 chosenCard1 chosenCard2 =
  forAll winGen $ \(win1, win2) ->
    let (player11, player22) = updateState player1 player2 chosenCard1 chosenCard2 win1 win2
        pend1 = pendingCards player1
        pend2 = pendingCards player2
        pend11 = pendingCards player11
        pend22 = pendingCards player22
        alls = [chosenCard1, chosenCard2] ++ pend1 ++ pend2
        unchange = hand player11 == hand player1 && deck player11 == deck player1 && randomGen player11 == randomGen player1 &&
                   hand player22 == hand player2 && deck player22 == deck player2 && randomGen player22 == randomGen player2
    --woncards，pending是否正确改变
    in case (win1, win2) of
         (True, False) -> wonCards player11 == alls ++ wonCards player1 &&
                          null (pendingCards player11)&&
                          null (pendingCards player22)
         (False, True) -> wonCards player22 == alls ++ wonCards player2 &&
                          null (pendingCards player11)&&
                          null (pendingCards player22)
         (False, False) -> pend11 == chosenCard1 : pend1 &&
                           pend22 == chosenCard2 : pend2 &&
                           wonCards player1 == wonCards player11 &&
                           wonCards player2 == wonCards player22
       && unchange
       
