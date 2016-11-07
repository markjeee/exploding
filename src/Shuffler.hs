module Shuffler where

import System.Random
import System.Random.Shuffle
import Data.List

import Common

shuffleDeck :: State -> IO State
shuffleDeck state =
  newStdGen >>= \gen -> return (state { deck = shuffle' (deck state) (length $ deck state) gen })
