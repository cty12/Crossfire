module Types where

import Data.Map.Strict (Map)
import Data.Set        (Set)

-- ─── Constants ───────────────────────────────────────────────────────────────

winLength :: Int
winLength = 4

cellSize :: Int
cellSize = 64

-- ─── Types ───────────────────────────────────────────────────────────────────

data Player = P1 | P2 deriving (Eq, Show)

type Coord = (Int, Int)

type Board = Map Coord Player

data Phase = Playing | Won Player | Draw deriving (Eq, Show)

-- Board dimensions as (rows, cols).
type Dims = (Int, Int)

defaultDims :: Dims
defaultDims = (8, 8)

data GameMode = PvP | PvC deriving (Eq, Show)

data Difficulty = Easy | Hard deriving (Eq, Show)

defaultDifficulty :: Difficulty
defaultDifficulty = Hard

-- A launcher sits just outside one edge of the board and fires inward.
data Launcher
  = TopL    Int   -- column, fires downward
  | BottomL Int   -- column, fires upward
  | LeftL   Int   -- row,    fires rightward
  | RightL  Int   -- row,    fires leftward
  deriving (Eq, Show)

data Model = Model
  { board        :: Board
  , curPlayer    :: Player
  , phase        :: Phase
  , hoverL       :: Maybe Launcher
  , activeDims   :: Dims              -- dimensions of the current game
  , selectedDims :: Dims              -- pending selection from the dropdown
  , voids        :: Set Coord         -- interior void cells for the current game
  , gameMode     :: GameMode
  , difficulty   :: Difficulty
  , lastPlaced   :: Maybe Coord         -- most recently placed stone
  } deriving (Eq, Show)

data Msg
  = Launch     Launcher        -- fired by a human player
  | AIMove     Launcher        -- fired by the AI
  | SetHover   (Maybe Launcher)
  | Restart
  | SelectSize Dims
  | SetVoids   (Set Coord)
  | SelectMode GameMode
  | SelectDiff Difficulty
  | NoOp
  deriving (Eq, Show)
