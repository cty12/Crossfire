module Strategies.NaiveBestLauncher (bestLauncherNaive) where

import qualified Data.Map.Strict as Map
import           Data.Set        (Set)
import           Data.List       (sortBy)
import           Data.Maybe      (listToMaybe)
import           Data.Ord        (comparing, Down (..))

import Types
import Logic (nextPlayer, uniqueLaunchTargets, hasWon, consec)

-- Choose the best launcher for player 'p'.
-- Priority: (1) immediate win, (2) block opponent win, (3) maximise streak.
bestLauncherNaive :: Dims -> Board -> Set Coord -> Player -> Maybe Launcher
bestLauncherNaive dims b vs p =
  case filter winsNow valid of
    ((launcher, _):_) -> Just launcher
    []    -> case filter blocksOpp valid of
               ((launcher, _):_) -> Just launcher
               []                -> fmap fst $ listToMaybe
                                      (sortBy (comparing (Down . myStreak)) valid)
  where
    opp   = nextPlayer p
    valid = uniqueLaunchTargets dims b vs

    winsNow (_, pos) = hasWon dims (Map.insert pos p b) pos p

    blocksOpp (_, pos) = hasWon dims (Map.insert pos opp b) pos opp

    myStreak (_, pos) =
      let b' = Map.insert pos p b
      in  maximum [ consec dims b' p pos (dc, dr)
                  + consec dims b' p pos (-dc, -dr) - 1
                  | (dc, dr) <- [(1,0),(0,1),(1,1),(1,-1)] ]
