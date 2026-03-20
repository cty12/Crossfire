module Logic where

import qualified Data.Map.Strict as Map
import qualified Data.Set        as Set
import           Data.Set        (Set)
import           System.Random   (randomRIO)

import Types

-- ─── Initial State ────────────────────────────────────────────────────────────

initModel :: Model
initModel = Model
  { board        = Map.empty
  , curPlayer    = P1
  , phase        = Playing
  , hoverL       = Nothing
  , activeDims   = defaultDims
  , selectedDims = defaultDims
  , voids        = Set.empty
  , gameMode     = PvC
  , difficulty   = defaultDifficulty
  , lastPlaced   = Nothing
  }

-- ─── Game Logic ──────────────────────────────────────────────────────────────

nextPlayer :: Player -> Player
nextPlayer P1 = P2
nextPlayer P2 = P1

inBounds :: Dims -> Coord -> Bool
inBounds (Dims numRows numCols) (Coord c r) =
  c >= 0 && c < numCols && r >= 0 && r < numRows

-- Entry position (just outside the board) and travel direction for a launcher.
launcherEntry :: Dims -> Launcher -> Coord
launcherEntry (Dims numRows _      ) (TopL    c) = Coord c numRows
launcherEntry _                       (BottomL c) = Coord c (-1)
launcherEntry _                       (LeftL   r) = Coord (-1) r
launcherEntry (Dims _       numCols) (RightL  r) = Coord numCols r

launcherDir :: Launcher -> (Int, Int)
launcherDir (TopL    _) = ( 0, -1)
launcherDir (BottomL _) = ( 0,  1)
launcherDir (LeftL   _) = ( 1,  0)
launcherDir (RightL  _) = (-1,  0)

-- Compute where a stone lands when fired from a launcher.
-- Travels until hitting an occupied cell, a void, or the far wall.
-- Returns the last empty non-void cell, or Nothing if the path is fully blocked.
launchStone :: Dims -> Board -> Set Coord -> Launcher -> Maybe Coord
launchStone dims b vs launcher = go (Coord (ec + dc) (er + dr)) Nothing
  where
    Coord ec er = launcherEntry dims launcher
    (dc, dr)    = launcherDir launcher
    go pos@(Coord c r) acc
      | not (inBounds dims pos) = acc
      | Map.member pos b        = acc
      | Set.member pos vs       = acc
      | otherwise               = go (Coord (c + dc) (r + dr)) (Just pos)

-- Count consecutive same-colour stones from 'pos' along direction '(dc, dr)'.
consec :: Dims -> Board -> Player -> Coord -> (Int, Int) -> Int
consec dims b p (Coord c r) (dc, dr) =
  length $ takeWhile inLine [Coord (c + dc * i) (r + dr * i) | i <- [0 .. winLength - 1]]
  where
    inLine pos = inBounds dims pos && Map.lookup pos b == Just p

-- Check all four axes through a position for a win.
hasWon :: Dims -> Board -> Coord -> Player -> Bool
hasWon dims b pos p = any checkAxis [(1,0), (0,1), (1,1), (1,-1)]
  where
    checkAxis (dc, dr) =
      consec dims b p pos (dc, dr) + consec dims b p pos (-dc, -dr) - 1 >= winLength

allLaunchers :: Dims -> [Launcher]
allLaunchers (Dims numRows numCols) =
     map TopL    [0 .. numCols - 1]
  ++ map BottomL [0 .. numCols - 1]
  ++ map LeftL   [0 .. numRows - 1]
  ++ map RightL  [0 .. numRows - 1]

-- Launchers paired with their landing positions.
launchTargets :: Dims -> Board -> Set Coord -> [(Launcher, Coord)]
launchTargets dims b vs = go (allLaunchers dims)
  where
    go [] = []
    go (launcher:rest) = case launchStone dims b vs launcher of
      Nothing  -> go rest
      Just pos -> (launcher, pos) : go rest

-- Keep the first launcher seen for each landing position.
uniqueLaunchTargets :: Dims -> Board -> Set Coord -> [(Launcher, Coord)]
uniqueLaunchTargets dims b vs = go Set.empty (launchTargets dims b vs)
  where
    go _    [] = []
    go seen ((launcher, pos):rest)
      | Set.member pos seen = go seen rest
      | otherwise           = (launcher, pos) : go (Set.insert pos seen) rest

hasLegalLaunch :: Dims -> Board -> Set Coord -> Bool
hasLegalLaunch dims b vs = not (null (launchTargets dims b vs))

-- Randomly place ~1/8 of the board's cells as voids.
generateVoids :: Dims -> IO (Set Coord)
generateVoids (Dims numRows numCols) = go Set.empty
  where
    n = numRows * numCols `div` 8
    go acc
      | Set.size acc >= n = return acc
      | otherwise         = do
          c <- randomRIO (0, numCols - 1)
          r <- randomRIO (0, numRows - 1)
          go (Set.insert (Coord c r) acc)

-- True when it is a human player's turn to act.
isHumanTurn :: Model -> Bool
isHumanTurn m = gameMode m == PvP || curPlayer m == P1
