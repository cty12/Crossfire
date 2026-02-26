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

inBounds :: Dims -> Int -> Int -> Bool
inBounds (rows, cols) c r = c >= 0 && c < cols && r >= 0 && r < rows

-- Entry position (just outside the board) and travel direction for a launcher.
launcherEntry :: Dims -> Launcher -> (Int, Int)
launcherEntry (rows, _   ) (TopL    c) = (c,    rows)
launcherEntry _            (BottomL c) = (c,    -1  )
launcherEntry _            (LeftL   r) = (-1,   r   )
launcherEntry (_,    cols) (RightL  r) = (cols, r   )

launcherDir :: Launcher -> (Int, Int)
launcherDir (TopL    _) = ( 0, -1)
launcherDir (BottomL _) = ( 0,  1)
launcherDir (LeftL   _) = ( 1,  0)
launcherDir (RightL  _) = (-1,  0)

-- Compute where a stone lands when fired from a launcher.
-- Travels until hitting an occupied cell, a void, or the far wall.
-- Returns the last empty non-void cell, or Nothing if the path is fully blocked.
launchStone :: Dims -> Board -> Set (Int, Int) -> Launcher -> Maybe (Int, Int)
launchStone dims b vs launcher = go (ec + dc) (er + dr) Nothing
  where
    (ec, er) = launcherEntry dims launcher
    (dc, dr) = launcherDir        launcher
    go c r acc
      | not (inBounds dims c r) = acc
      | Map.member (c, r) b     = acc
      | Set.member (c, r) vs    = acc
      | otherwise               = go (c + dc) (r + dr) (Just (c, r))

-- Count consecutive same-colour stones from 'pos' along direction '(dc, dr)'.
consec :: Dims -> Board -> Player -> (Int, Int) -> (Int, Int) -> Int
consec dims b p (col, row) (dc, dr) =
  length $ takeWhile inLine [(col + dc * i, row + dr * i) | i <- [0 .. winLength - 1]]
  where
    inLine (c, r) = inBounds dims c r && Map.lookup (c, r) b == Just p

-- Check all four axes through a position for a win.
hasWon :: Dims -> Board -> (Int, Int) -> Player -> Bool
hasWon dims b pos p = any checkAxis [(1,0), (0,1), (1,1), (1,-1)]
  where
    checkAxis (dc, dr) =
      consec dims b p pos (dc, dr) + consec dims b p pos (-dc, -dr) - 1 >= winLength

allLaunchers :: Dims -> [Launcher]
allLaunchers (rows, cols) =
     map TopL    [0 .. cols - 1]
  ++ map BottomL [0 .. cols - 1]
  ++ map LeftL   [0 .. rows - 1]
  ++ map RightL  [0 .. rows - 1]

-- Randomly place ~1/8 of the board's cells as voids.
generateVoids :: Dims -> IO (Set (Int, Int))
generateVoids (rows, cols) = go Set.empty
  where
    n = rows * cols `div` 8
    go acc
      | Set.size acc >= n = return acc
      | otherwise         = do
          c <- randomRIO (0, cols - 1)
          r <- randomRIO (0, rows - 1)
          go (Set.insert (c, r) acc)

-- True when it is a human player's turn to act.
isHumanTurn :: Model -> Bool
isHumanTurn m = gameMode m == PvP || curPlayer m == P1

