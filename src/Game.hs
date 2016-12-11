module Game where

import Data.List (elemIndex)
import Data.Maybe (fromJust, fromMaybe)

import Common
import Shuffler

initialCardCount :: Int
initialCardCount = 5

initGame :: Int -> State

-- TODO: Implement a method to initialize a new game given n players
initGame n | (n < 6 && n > 0) = State { players = generateHumanPlayers n,
                                        e_players = [ ],
                                        deck = initDeck,
                                        d_stack = [ ],
                                        cur_player = noPlayer }
           | otherwise = error "Invalid number of players"

-- TODO: Implement a method to setup the game
setupGame :: State -> IO State
setupGame gs = do
  gs' <- shuffleDeck gs
  gs' <- setupExplodingCards gs'
  gs' <- setupDefuseCards gs'
  gs' <- dealToAllPlayers (initialCardCount - 1) gs' $ players gs'
  shuffleDeck gs'

startGame :: State -> IO State
startGame gs = pickNextAndPlay gs

pickNextAndPlay :: State -> IO State
pickNextAndPlay gs = do
  gs' <- pickNextPlayer gs
  playLoop gs' False

playLoop :: State -> Bool -> IO State
playLoop gs under_attack = do
  if playerHasWon gs
    then return (gs)
    else do
      (next_action, gs') <- playPlayer gs under_attack
      playLoopNext gs' next_action

playLoopNext :: State -> Action -> IO State
playLoopNext gs next_action
  | next_action == Exploded = explodeAndPlay gs
  | next_action == AttackNextPlayer = attackAndPlay gs
  | otherwise = pickNextAndPlay gs

playerHasWon :: State -> Bool
playerHasWon gs = (length $ players gs) == 1

explodeAndPlay :: State -> IO State
explodeAndPlay gs = do
  gs' <- explodePlayer gs
  playLoop gs' False

attackAndPlay :: State -> IO State
attackAndPlay gs = do
  gs' <- pickNextPlayer gs
  playLoop gs' True

explodePlayer :: State -> IO State
explodePlayer gs = do
  let next_p = getNextPlayer gs (cur_player gs)
  gs' <- discardExplodedHand gs
  gs' <- moveToExploded gs'
  updateCurPlayer gs' next_p

discardExplodedHand :: State -> IO State
discardExplodedHand gs = do
  gs' <- discardCards (curHand gs) gs
  updateCurHand gs' [ ]

playPlayer :: State -> Bool -> IO (Action, State)
playPlayer gs under_attack = do
  (next_action, gs') <- playTurn gs
  takeNextAction next_action gs' under_attack

takeNextAction :: Action -> State -> Bool -> IO (Action, State)
takeNextAction next_action gs under_attack
  | next_action == EndTurn && under_attack = playPlayer gs False
  | next_action `elem` [ Exploded, AttackNextPlayer, EndTurn ] = return (next_action, gs)
  | otherwise = error "Action not allowed"

playTurn :: State -> IO (Action, State)
playTurn gs = do
  (next_action, gs') <- playOneCard gs
  takeTurnNextAction next_action gs'

takeTurnNextAction :: Action -> State -> IO (Action, State)
takeTurnNextAction next_action gs
  | next_action == ContinuePlay = playTurn gs
  | next_action `elem` [ Exploded, AttackNextPlayer, EndTurn ] = return (next_action, gs)
  | otherwise = error "Action not allowed"

playOneCard :: State -> IO (Action, State)
playOneCard gs = takeAction action cards gs where
  (action, cards) = playCurrentPlayer gs [ ]

takeAction :: Action -> [ Card ] -> State -> IO (Action, State)
takeAction action cards gs
  | action == TakeFromDeck = takeFromDeck gs
  | action == UseCard && not (allCardsInCurHand cards gs) = error ("Cards " ++ show cards ++ " not in hand: " ++ show (curHand gs))
  | action == UseCard = playCard cards gs
  | otherwise = error "Action not allowed"

playCard :: [ Card ] -> State -> IO (Action, State)
playCard cards gs = takeFromHandWithAction cards EndTurn gs

takeFromHandWithAction :: [ Card ] -> Action -> State -> IO (Action, State)
takeFromHandWithAction cards next_action gs = do
  gs' <- takeFromHand cards gs
  return (next_action, gs')

takeFromHand :: [ Card ] -> State -> IO State
takeFromHand cards gs = do
  gs' <- removeFromCurHand cards gs
  discardCards cards gs'

takeFromDeck :: State -> IO (Action, State)
takeFromDeck gs = do
  gs' <- drawNCards 1 gs $ cur_player gs
  explodeOrEndTurn gs'

explodeOrEndTurn :: State -> IO (Action, State)
explodeOrEndTurn gs = do
  if willExplode gs
    then if canDefuse gs
      then defuseAndEndTurn gs
      else return (Exploded, gs)
    else return (EndTurn, gs)

willExplode :: State -> Bool
willExplode gs = ExplodingCard `elem` (hand $ cur_player gs)

canDefuse :: State -> Bool
canDefuse gs = DefuseCard `elem` (hand $ cur_player gs)

defuseAndEndTurn :: State -> IO (Action, State)
defuseAndEndTurn gs = do
  gs' <- defuse gs
  return (EndTurn, gs')

defuse :: State -> IO State
defuse gs = do
  (dcards, gs') <- takeFromCurHands [ ExplodingCard, DefuseCard ] gs
  gs' <- discardCards [ c | c <- dcards, c == DefuseCard ] gs'
  insertInDeck [ c | c <- dcards, c == ExplodingCard ] gs'

takeFromCurHands :: [ Card ] -> State -> IO ([ Card ], State)
takeFromCurHands cards gs = do
  let (dcards, hand') = takeCards [ ExplodingCard, DefuseCard ] $ curHand gs
  gs' <- updateCurHand gs hand'
  return (dcards, gs')

insertInDeck :: [ Card ] -> State -> IO State
insertInDeck cards gs = do
  let deck' = (deck gs) ++ cards
  gs' <- updateDeck gs deck'
  shuffleDeck gs'

injectCards :: [ Card ] -> State -> IO State
injectCards cards gs = updateCurHand gs hand' where
  hand' = (curHand gs) ++ cards

setupExplodingCards :: State -> IO State
setupExplodingCards gs = do
  let nc = (length $ players gs) - 1
  let (for_deck, for_discard)  = splitAt nc explodingCards
  let deck' = (deck gs) ++ for_deck
  let d_stack' = (d_stack gs) ++ for_discard
  return (gs { deck = deck',
               d_stack = d_stack' })

setupDefuseCards :: State -> IO State
setupDefuseCards gs = do
  let nc = length $ players gs
  let (for_distro, for_deck) = splitAt nc defuseCards
  let deck' = (deck gs) ++ for_deck
  gs' <- updateDeck gs deck'
  dealCardToAllPlayers (head for_distro) gs' $ players gs'

drawNCards :: Int -> State -> Player -> IO State
drawNCards n gs player = do
  let (hand', deck') = splitAt n $ deck gs
  player' <- updateHand player (hand player ++ hand')
  gs' <- updatePlayer gs player player'
  updateDeck gs' deck'

dealCardToAllPlayers :: Card -> State -> [ Player ] -> IO State
dealCardToAllPlayers card gs [] = return (gs)
dealCardToAllPlayers card gs ps = do
  let (ps', ps'') = splitAt 1 ps
  gs' <- dealCard card gs $ head ps'
  dealCardToAllPlayers card gs' ps''

dealCard :: Card -> State -> Player -> IO State
dealCard card gs player = do
  let hand' = (hand player) ++ [ card ]
  player' <- updateHand player (hand player ++ hand')
  updatePlayer gs player player'

moveToExploded :: State -> IO State
moveToExploded gs = do
  let cp = cur_player gs
  gs' <- removePlayer cp gs
  return (gs' { e_players = (e_players gs') ++ [ cp ] })

removePlayer :: Player -> State -> IO State
removePlayer player gs = return (gs { players = new_players, cur_player = new_cp }) where
  new_players = [ p | p <- (players gs), p /= player ]
  new_cp = if (cur_player gs) == player then noPlayer else (cur_player gs)

updatePlayer :: State -> Player -> Player -> IO State
updatePlayer gs p new_p = do
  return (gs { players = map (\p' -> if p' == p then new_p else p') $ players gs,
               cur_player = if (cur_player gs) == p then new_p else (cur_player gs) })

discardCards :: [ Card ] -> State -> IO State
discardCards cards gs = do
  let d_stack' = (d_stack gs) ++ cards
  updateDiscardS gs d_stack'

allCardsInCurHand :: [ Card ] -> State -> Bool
allCardsInCurHand cards gs = all (\c -> cardInCurHand c gs) cards

cardInCurHand :: Card -> State -> Bool
cardInCurHand card gs = cardInHand card (cur_player gs)

cardInHand :: Card -> Player -> Bool
cardInHand card player = card `elem` (hand player)

curHand :: State -> Hand
curHand gs = hand $ cur_player gs

updateCurHand :: State -> Hand -> IO State
updateCurHand gs h = do
  let cp = cur_player gs
  player' <- updateHand cp h
  updatePlayer gs cp player'

updateHand :: Player -> Hand -> IO Player
updateHand player h = return (player { hand = h })

updateDeck :: State -> Deck -> IO State
updateDeck gs deck' = return (gs { deck = deck' })

updateDiscardS :: State -> Deck -> IO State
updateDiscardS gs d_stack' = return (gs { d_stack = d_stack' })

updateCurPlayer :: State -> Player -> IO State
updateCurPlayer gs player = return (gs { cur_player = player })

getNextPlayer :: State -> Player -> Player
getNextPlayer gs player
  | (player == noPlayer) = head $ players gs
  | otherwise = do
      let i = fromJust $ elemIndex player (players gs)
      if (i + 1) >= (length $ players gs)
        then head $ players gs
        else (players gs) !! (i + 1)

pickNextPlayer :: State -> IO State
pickNextPlayer gs = updateCurPlayer gs $ getNextPlayer gs (cur_player gs)

playCurrentPlayer :: State -> [ Card ] -> (Action, [ Card ])
playCurrentPlayer gs future_cards = useSimpleStrategy gs future_cards (curHand gs)

useSimpleStrategy :: State -> [ Card ] -> Hand -> (Action, [ Card ])
useSimpleStrategy gs future_cards hand
  | otherwise = (TakeFromDeck, [ ])

-- ADD extra codes after this line, so it's easy to rebase or merge code changes in Git --

generateHumanPlayers :: Int -> [ Player ]
generateHumanPlayers n | (n > 0) = [ HPlayer {name = "Human " ++ show n, hand = [ ]} ] ++ generateHumanPlayers (n-1)
                       | otherwise = [ ]

dealToAllPlayers :: Int -> State -> [ Player ] -> IO State
dealToAllPlayers n gs [] = return (gs)
dealToAllPlayers n gs ps = do
  let (ps', ps'') = splitAt 1 ps
  gs' <- drawNCards n gs $ head ps'
  dealToAllPlayers n gs' ps''

removeFromCurHand :: [ Card ] -> State -> IO State
removeFromCurHand cards gs = updateCurHand gs hand' where
  (d_cards, hand') = takeCards cards (curHand gs)
