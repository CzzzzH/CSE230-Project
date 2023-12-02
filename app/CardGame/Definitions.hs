module CardGame.Definitions where
import System.Random (StdGen)

-- 定义牌的类型
data Card = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace deriving (Eq, Ord, Enum, Show)

data Player = Player {
    hand :: [Card],       -- 手牌
    deck :: [Card],       -- 牌组
    chosenCard::[Card],   --当前局选择的牌
    wonCards :: [Card],   -- 赢得的牌
    pendingCards :: [Card], -- 平局时积累的牌
    randomGen :: StdGen    -- 随机生成器
} deriving (Show)

-- 初始化一个完整的牌组
fullDeck :: [Card]
fullDeck = [Two .. Ace]
