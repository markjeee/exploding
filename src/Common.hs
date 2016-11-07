-- MORE Info on rules here:
-- https://www.explodingkittens.com/how

module Common where
import System.Console.ANSI (Color(Black, Red, Green, Yellow, Blue, Magenta, Cyan, White))

data Card = DefuseCard
          | ExplodingCard
          | NopeCard
          | AttackCard
          | SkipCard
          | FavorCard
          | ShuffleCard
          | FutureCard
          | TacoCat
          | MelonCat
          | PotatoCat
          | BeardCat
          | RainbowCat deriving (Eq, Show, Read)

type Hand = [ Card ]
type Deck = [ Card ]
type D_Stack = [ Card ]

data Player = HPlayer { name :: String,
                        hand :: Hand }
            | AiPlayer { name :: String,
                         hand :: Hand }
            deriving (Show, Eq)

data State = State { players :: [ Player ],
                     deck :: Deck,
                     d_stack :: D_Stack }

explodingCards = take 4 $ repeat ExplodingCard
defuseCards = take 6 $ repeat DefuseCard
nopeCards = take 5 $ repeat NopeCard
attackCards = take 4 $ repeat AttackCard
skipCards = take 4 $ repeat SkipCard
favorCards = take 4 $ repeat FavorCard
shuffleCards = take 4 $ repeat ShuffleCard
futureCards = take 5 $ repeat FutureCard
tacoCats = take 4 $ repeat TacoCat
melonCats = take 4 $ repeat MelonCat
potatoCats = take 4 $ repeat PotatoCat
beardCats = take 4 $ repeat BeardCat
rainbowCats = take 4 $ repeat RainbowCat

actionCards = nopeCards
              ++ attackCards
              ++ skipCards
              ++ favorCards
              ++ shuffleCards
              ++ futureCards

catCards = tacoCats
           ++ melonCats
           ++ potatoCats
           ++ beardCats
           ++ rainbowCats

initDeck :: Deck
initDeck = actionCards ++ catCards

fullDeck :: Deck
fullDeck = explodingCards ++ defuseCards ++ initDeck

isCatCard :: Card -> Bool
isCatCard c = c `elem` [ TacoCat, MelonCat, PotatoCat, BeardCat, RainbowCat ]

getCards :: Deck -> Card -> [ Card ]
getCards d c = [ c' | c' <- d, c' == c ]
