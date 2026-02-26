{-# LANGUAGE TupleSections #-}
module Strategies.AlphaBetaPruning (bestLauncherAlphaBeta) where

import qualified Data.Map.Strict as Map
import           Data.Set        (Set)
import           Data.List       (maximumBy, partition)
import           Data.Maybe      (mapMaybe)
import           Data.Ord        (comparing)

import Types
import Logic (consec, hasWon, allLaunchers, launchStone, nextPlayer)

-- ─── Constants ───────────────────────────────────────────────────────────────

winScore :: Int
winScore = 1000000

searchDepth :: Int
searchDepth = 4

-- ─── Static Evaluation ───────────────────────────────────────────────────────

-- Board score from P2's (maximiser) perspective.
-- Counts weighted streaks for both players.
evaluate :: Dims -> Board -> Int
evaluate dims b = sum
  [ sign p * weight len
  | (pos, p) <- Map.toList b
  , (dc, dr) <- [(1,0),(0,1),(1,1),(1,-1)]
  , let len = consec dims b p pos (dc, dr)
            + consec dims b p pos (-dc, -dr) - 1
  , len >= 2
  ]
  where
    sign P2 = 1
    sign P1 = -1
    weight 2 = 10
    weight 3 = 100
    weight _ = 10000   -- 4+ is terminal; shouldn't be reached here

-- ─── Move Generation ─────────────────────────────────────────────────────────

-- Launchers paired with their landing positions, in move order:
-- winning moves first, then opponent blocks, then the rest.
orderedMoves :: Dims -> Board -> Set (Int,Int) -> Player -> [(Launcher, (Int,Int))]
orderedMoves dims b vs p = wins ++ blocks ++ rest
  where
    opp             = nextPlayer p
    all_            = mapMaybe (\l -> fmap (l,) (launchStone dims b vs l))
                               (allLaunchers dims)
    (wins,   noWin) = partition (\(_,pos) -> hasWon dims (Map.insert pos p   b) pos p  ) all_
    (blocks, rest ) = partition (\(_,pos) -> hasWon dims (Map.insert pos opp b) pos opp) noWin

-- ─── Alpha-Beta Search ───────────────────────────────────────────────────────

-- Minimax with alpha-beta pruning.
-- Score is always from P2's (maximiser) perspective.
-- alpha = lower bound for maximiser, beta = upper bound for minimiser.
alphaBeta :: Dims -> Board -> Set (Int,Int) -> Player -> Int -> Int -> Int -> Int
alphaBeta dims b vs player depth alpha beta
  | depth == 0 = evaluate dims b
  | null moves = evaluate dims b
  | player == P2 = maximize moves alpha
  | otherwise    = minimize moves beta
  where
    moves = orderedMoves dims b vs player

    maximize []            a  = a
    maximize ((_,pos):ls)  a  =
      let b' = Map.insert pos P2 b
          v  | hasWon dims b' pos P2 = winScore
             | otherwise             = alphaBeta dims b' vs P1 (depth-1) a beta
          a' = max a v
      in  if a' >= beta then a' else maximize ls a'

    minimize []            bt = bt
    minimize ((_,pos):ls)  bt =
      let b' = Map.insert pos P1 b
          v  | hasWon dims b' pos P1 = -winScore
             | otherwise             = alphaBeta dims b' vs P2 (depth-1) alpha bt
          bt'= min bt v
      in  if alpha >= bt' then bt' else minimize ls bt'

-- ─── Public Interface ────────────────────────────────────────────────────────

-- Pick the best launcher for the computer (P2) using alpha-beta search.
bestLauncherAlphaBeta :: Dims -> Board -> Set (Int,Int) -> Player -> Maybe Launcher
bestLauncherAlphaBeta dims b vs p =
  case orderedMoves dims b vs p of
    []    -> Nothing
    moves -> Just $ fst $ maximumBy (comparing snd) (map scoreMove moves)
  where
    scoreMove (l, pos) =
      let b' = Map.insert pos p b
          sc | hasWon dims b' pos p = winScore
             | otherwise            = alphaBeta dims b' vs (nextPlayer p)
                                        (searchDepth - 1) (-winScore) winScore
      in  (l, sc)
