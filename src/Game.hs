module Game where

import Common

initialCardCount :: Int
initialCardCount = 5

initGame :: Int -> State

-- TODO: Implement a method to initialize a new game given n players
initGame n = State { players = [ ],
                     deck = initDeck,
                     d_stack = [ ] }

-- TODO: Implement a method to setup the game
setupGame :: State -> IO State
setupGame gs = return (gs)
