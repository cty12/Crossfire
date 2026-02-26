{-# LANGUAGE CPP #-}
module Main where

import           Miso
import           Control.Monad.IO.Class (liftIO)
import qualified Data.Map.Strict                  as Map
import qualified Data.Set                         as Set
#ifndef ghcjs_HOST_OS
import           Language.Javascript.JSaddle.Warp (run)
#endif

import Logic
import Types
import View

-- ─── Helpers ─────────────────────────────────────────────────────────────────

-- Trigger an AI move if it is the computer's turn.
triggerAI :: Model -> Effect Msg Model
triggerAI m
  | gameMode m == PvC, curPlayer m == P2, phase m == Playing =
      m <# return (case bestLauncher (activeDims m) (board m) (voids m) P2 of
                     Just l  -> AIMove l
                     Nothing -> NoOp)
  | otherwise = noEff m

-- Apply a launcher move for the current player, then trigger AI if needed.
handleLaunch :: Model -> Launcher -> Effect Msg Model
handleLaunch m launcher =
  let dims@(rows, cols) = activeDims m
  in  case launchStone dims (board m) (voids m) launcher of
        Nothing  -> noEff m
        Just pos ->
          let b'       = Map.insert pos (curPlayer m) (board m)
              capacity = rows * cols - Set.size (voids m)
              newPhase
                | hasWon dims b' pos (curPlayer m) = Won (curPlayer m)
                | Map.size b' == capacity          = Draw
                | otherwise                        = Playing
              m' = m { board     = b'
                     , phase     = newPhase
                     , curPlayer = case newPhase of
                                     Playing -> nextPlayer (curPlayer m)
                                     _       -> curPlayer m
                     }
          in  triggerAI m'

-- ─── Update ──────────────────────────────────────────────────────────────────

gameUpdate :: Msg -> Model -> Effect Msg Model
gameUpdate msg m = case msg of

  Restart ->
    let dims = selectedDims m
    in  initModel { activeDims = dims, selectedDims = dims, gameMode = gameMode m }
        <# (SetVoids <$> liftIO (generateVoids dims))

  SetVoids vs ->
    noEff $ m { voids = vs }

  SelectSize dims ->
    noEff $ m { selectedDims = dims }

  SelectMode mode ->
    noEff $ m { gameMode = mode }

  SetHover ml ->
    noEff $ m { hoverL = ml }

  Launch launcher
    | phase m /= Playing  -> noEff m
    | not (isHumanTurn m) -> noEff m
    | otherwise           -> handleLaunch m launcher

  AIMove launcher
    | phase m /= Playing -> noEff m
    | otherwise          -> handleLaunch m launcher

  NoOp -> noEff m

-- ─── Entry Point ─────────────────────────────────────────────────────────────

main :: IO ()
#ifdef ghcjs_HOST_OS
main = startApp App
#else
main = run 8080 $ startApp App
#endif
  { model         = initModel
  , update        = gameUpdate
  , view          = gameView
  , subs          = []
  , events        = defaultEvents
  , initialAction = Restart
  , mountPoint    = Nothing
  , logLevel      = Off
  }
