module Strategies.AlphaBetaPruning (bestLauncherAlphaBeta) where

import qualified Data.Map.Strict as Map
import           Data.Set        (Set)
import           Data.List       (maximumBy, partition)
import           Data.Ord        (comparing)

import Types
import Logic (consec, hasWon, nextPlayer, uniqueLaunchTargets)

-- ─── Constants ───────────────────────────────────────────────────────────────

winScore :: Int
winScore = 1000000

searchDepth :: Int
searchDepth = 4

-- ─── Static Evaluation ───────────────────────────────────────────────────────

-- Board score from the root player's perspective.
-- Counts weighted streaks for both players.
evaluate :: Player -> Dims -> Board -> Int
evaluate rootPlayer dims b = sum
  [ sign p * weight len
  | (pos, p) <- Map.toList b
  , (dc, dr) <- [(1,0),(0,1),(1,1),(1,-1)]
  , let len = consec dims b p pos (dc, dr)
            + consec dims b p pos (-dc, -dr) - 1
  , len >= 2
  ]
  where
    sign p
      | p == rootPlayer = 1
      | otherwise       = -1
    weight 2 = 10
    weight 3 = 100
    weight _ = 10000   -- 4+ is terminal; shouldn't be reached here

-- ─── Move Generation ─────────────────────────────────────────────────────────

-- Unique launcher/landing pairs, in move order:
-- winning moves first, then opponent blocks, then the rest.
orderedMoves :: Dims -> Board -> Set Coord -> Player -> [(Launcher, Coord)]
orderedMoves dims b vs p = wins ++ blocks ++ rest
  where
    opp             = nextPlayer p
    all_            = uniqueLaunchTargets dims b vs
    (wins,   noWin) = partition (\(_,pos) -> hasWon dims (Map.insert pos p   b) pos p  ) all_
    (blocks, rest ) = partition (\(_,pos) -> hasWon dims (Map.insert pos opp b) pos opp) noWin

-- ─── Alpha-Beta Search ───────────────────────────────────────────────────────

-- Minimax with alpha-beta pruning.
-- Score is always from the root player's perspective.
-- alpha = lower bound for maximiser, beta = upper bound for minimiser.
alphaBeta :: Player -> Dims -> Board -> Set Coord -> Player -> Int -> Int -> Int -> Int
alphaBeta rootPlayer dims b vs player depth alpha beta
  | depth == 0 = evaluate rootPlayer dims b
  | null moves = 0
  | player == rootPlayer = maximize moves alpha
  | otherwise            = minimize moves beta
  where
    moves = orderedMoves dims b vs player
    terminalScore winner
      | winner == rootPlayer = winScore
      | otherwise            = -winScore

    maximize []            a  = a
    maximize ((_,pos):ls)  a  =
      let b' = Map.insert pos player b
          v  | hasWon dims b' pos player = terminalScore player
             | otherwise                 =
                 alphaBeta rootPlayer dims b' vs (nextPlayer player) (depth-1) a beta
          a' = max a v
      in  if a' >= beta then a' else maximize ls a'

    minimize []            bt = bt
    minimize ((_,pos):ls)  bt =
      let b' = Map.insert pos player b
          v  | hasWon dims b' pos player = terminalScore player
             | otherwise                 =
                 alphaBeta rootPlayer dims b' vs (nextPlayer player) (depth-1) alpha bt
          bt'= min bt v
      in  if alpha >= bt' then bt' else minimize ls bt'

-- ─── Public Interface ────────────────────────────────────────────────────────

-- Pick the best launcher for the given root player using alpha-beta search.
bestLauncherAlphaBeta :: Dims -> Board -> Set Coord -> Player -> Maybe Launcher
bestLauncherAlphaBeta dims b vs rootPlayer =
  case orderedMoves dims b vs rootPlayer of
    []    -> Nothing
    moves -> Just $ fst $ maximumBy (comparing snd) (map scoreMove moves)
  where
    scoreMove (l, pos) =
      let b' = Map.insert pos rootPlayer b
          sc | hasWon dims b' pos rootPlayer = winScore
             | otherwise                     =
                 alphaBeta rootPlayer dims b' vs (nextPlayer rootPlayer)
                   (searchDepth - 1) (-winScore) winScore
      in  (l, sc)
