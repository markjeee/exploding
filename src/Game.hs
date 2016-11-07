module Game where

import Common
import Shuffler

initialCardCount :: Int
initialCardCount = 5

initGame :: Int -> State

-- TODO: Implement a method to initialize a new game given n players
initGame n | (n < 6 && n > 0) = State { players = generateHumanPlayers n,
                                        deck = initDeck,
                                        d_stack = [ ] }
           | otherwise = error "Invalid number of players"

generateHumanPlayers :: Int -> [Player]
generateHumanPlayers n | (n > 0) = [ HPlayer {name = "Human " ++ show n, hand = [ ]} ] ++ generateHumanPlayers (n-1)
                       | otherwise = [ ]

-- TODO: Implement a method to setup the game
setupGame :: State -> IO State
setupGame gs = do
  gs' <- shuffleDeck gs
  gs' <- setupExplodingCards gs'
  gs' <- setupDefuseCards gs'
  gs' <- dealToAllPlayers (initialCardCount - 1) gs' $ players gs'
  shuffleDeck gs'

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

dealToAllPlayers :: Int -> State -> [ Player ] -> IO State
dealToAllPlayers n gs [] = return (gs)
dealToAllPlayers n gs ps = do
  let (ps', ps'') = splitAt 1 ps
  gs' <- dealNCards n gs $ head ps'
  dealToAllPlayers n gs' ps''

dealNCards :: Int -> State -> Player -> IO State
dealNCards n gs player = do
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

updatePlayer :: State -> Player -> Player -> IO State
updatePlayer gs p new_p = do
  return (gs { players = map (\p' -> if p' == p then new_p else p') $ players gs })

updateHand :: Player -> Hand -> IO Player
updateHand player h = return (player { hand = h })

updateDeck :: State -> Deck -> IO State
updateDeck gs deck' = return (gs { deck = deck' })

updateDiscardS :: State -> Deck -> IO State
updateDiscardS gs d_stack' = return (gs { d_stack = d_stack' })
