module Strategies (Strategy, strategyFor) where

import Data.Set (Set)

import Types
import Strategies.NaiveBestLauncher (bestLauncherNaive)
import Strategies.AlphaBetaPruning  (bestLauncherAlphaBeta)

-- A strategy is a function that picks a launcher for the given player.
type Strategy = Dims -> Board -> Set Coord -> Player -> Maybe Launcher

strategyFor :: Difficulty -> Strategy
strategyFor Easy = bestLauncherNaive
strategyFor Hard = bestLauncherAlphaBeta
