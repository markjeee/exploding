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
          | RainbowCat
          | NoCard deriving (Eq, Show, Read)

type Hand = [ Card ]
type Deck = [ Card ]
type D_Stack = [ Card ]

data Player = HPlayer { name :: String,
                        hand :: Hand }
            | AiPlayer { name :: String,
                         hand :: Hand }
            | NoPlayer { name :: String } deriving (Show, Eq)

data Action = UseCard
            | TakeFromDeck
            | ContinuePlay
            | EndTurn
            | Exploded
            | AttackNextPlayer
            | Skip
            | Favor
            | Shuffle
            | SeeTheFuture
            | TwoOfAKind
            | ThreeOfAKind
            | FiveRandom deriving (Eq, Show, Read)

endTurnActions = [ EndTurn, AttackNextPlayer ]

noPlayer :: Player
noPlayer = NoPlayer { name = "No Player" }

data State = State { players :: [ Player ],
                     e_players :: [ Player ],
                     deck :: Deck,
                     d_stack :: D_Stack,
                     cur_player :: Player }

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

takeCards :: [ Card ] -> Hand -> (Hand, Hand)
takeCards [ card ] from = ([ card ], f1 ++ (drop 1 f2)) where
  (f1, f2) = break (== card) from
takeCards (card:rest) from = (t1 ++ t2, remaining) where
  (t1, from') = takeCards [ card ] from
  (t2, remaining) = takeCards rest from'

curHand :: State -> Hand
curHand gs = hand $ cur_player gs
