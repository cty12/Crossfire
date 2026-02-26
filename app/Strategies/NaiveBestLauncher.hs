module Strategies.NaiveBestLauncher (bestLauncherNaive) where

import qualified Data.Map.Strict as Map
import           Data.Set        (Set)
import           Data.List       (sortBy)
import           Data.Maybe      (isJust, listToMaybe)
import           Data.Ord        (comparing, Down (..))

import Types
import Logic (nextPlayer, allLaunchers, launchStone, hasWon, consec)

-- Choose the best launcher for player 'p'.
-- Priority: (1) immediate win, (2) block opponent win, (3) maximise streak.
bestLauncherNaive :: Dims -> Board -> Set (Int, Int) -> Player -> Maybe Launcher
bestLauncherNaive dims b vs p =
  case filter winsNow valid of
    (l:_) -> Just l
    []    -> case filter blocksOpp valid of
               (l:_) -> Just l
               []    -> listToMaybe $ sortBy (comparing (Down . myStreak)) valid
  where
    opp   = nextPlayer p
    valid = filter (isJust . launchStone dims b vs) (allLaunchers dims)

    winsNow l = case launchStone dims b vs l of
      Nothing  -> False
      Just pos -> hasWon dims (Map.insert pos p b) pos p

    blocksOpp l = case launchStone dims b vs l of
      Nothing  -> False
      Just pos -> hasWon dims (Map.insert pos opp b) pos opp

    myStreak l = case launchStone dims b vs l of
      Nothing  -> 0
      Just pos ->
        let b' = Map.insert pos p b
        in  maximum [ consec dims b' p pos (dc, dr)
                    + consec dims b' p pos (-dc, -dr) - 1
                    | (dc, dr) <- [(1,0),(0,1),(1,1),(1,-1)] ]
