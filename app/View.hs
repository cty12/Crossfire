{-# LANGUAGE OverloadedStrings #-}

module View where

import           Miso hiding (model)
import           Miso.String        (MisoString, ms, fromMisoString)
import           Miso.Svg           (svg_, circle_, rect_, polygon_)
import           Miso.Svg.Attribute (cx_, cy_, r_, fill_, x_, y_, points_)
import qualified Data.Map.Strict    as Map
import qualified Data.Set           as Set

import Logic
import Types

-- ─── Encode / Decode Helpers ────────────────────────────────────────────────

encodeDims :: Dims -> MisoString
encodeDims (Dims numRows numCols) = ms (show numRows ++ "," ++ show numCols)

decodeDims :: MisoString -> Dims
decodeDims s =
  let (rs, rest) = break (== ',') (fromMisoString s)
  in  Dims (read rs) (read (drop 1 rest))

encodeMode :: GameMode -> MisoString
encodeMode PvP = "pvp"
encodeMode PvC = "pvc"

decodeMode :: MisoString -> GameMode
decodeMode "pvc" = PvC
decodeMode _     = PvP

encodeDiff :: Difficulty -> MisoString
encodeDiff Easy = "easy"
encodeDiff Hard = "hard"

decodeDiff :: MisoString -> Difficulty
decodeDiff "hard" = Hard
decodeDiff _      = Easy

encodeTurnOrder :: TurnOrder -> MisoString
encodeTurnOrder HumanFirst    = "human"
encodeTurnOrder ComputerFirst = "computer"

decodeTurnOrder :: MisoString -> TurnOrder
decodeTurnOrder "computer" = ComputerFirst
decodeTurnOrder _          = HumanFirst

-- ─── Player Helpers ─────────────────────────────────────────────────────────

playerName :: Player -> MisoString
playerName P1 = "Red"
playerName P2 = "Blue"

playerColor :: Player -> MisoString
playerColor P1 = "red"
playerColor P2 = "blue"

statusMsg :: Model -> MisoString
statusMsg model = case phase model of
  Playing -> case gameMode model of
               PvC | not (isHumanTurn model) -> "Computer's turn"
               _                             -> playerName (curPlayer model) <> "'s turn"
  Won p   -> playerName p <> " wins!"
  Draw    -> "It's a draw!"

-- ─── SVG Coordinate Helpers ─────────────────────────────────────────────────
-- The board is offset by one cell in each direction to make room for launchers.

boardX :: Int -> Int
boardX c = (c + 1) * cellSize

boardY :: Int -> Int -> Int
boardY numRows r = (numRows - r) * cellSize

boardCX :: Int -> Int
boardCX c = boardX c + cellSize `div` 2

boardCY :: Int -> Int -> Int
boardCY numRows r = boardY numRows r + cellSize `div` 2

launcherXY :: Dims -> Launcher -> (Int, Int)
launcherXY (Dims numRows numCols) launcher = case launcher of
  TopL    c -> (boardX c,                    0)
  BottomL c -> (boardX c,                    (numRows + 1) * cellSize)
  LeftL   r -> (0,                           boardY numRows r)
  RightL  r -> ((numCols + 1) * cellSize,    boardY numRows r)

launcherArrowDir :: Launcher -> (Int, Int)
launcherArrowDir (TopL    _) = ( 0,  1)
launcherArrowDir (BottomL _) = ( 0, -1)
launcherArrowDir (LeftL   _) = ( 1,  0)
launcherArrowDir (RightL  _) = (-1,  0)

-- ─── SVG Primitives ─────────────────────────────────────────────────────────

ptsStr :: [(Int, Int)] -> MisoString
ptsStr ps = ms $ unwords [show x ++ "," ++ show y | (x, y) <- ps]

-- Bold arrow polygon pointing in SVG direction (adx, ady), centered at (cx, cy).
arrowPoly :: Int -> Int -> (Int, Int) -> MisoString -> View Msg
arrowPoly cx cy (adx, ady) color =
  let ext = cellSize * 5 `div` 16
      sw  = round (fromIntegral (cellSize `div` 13) / (sqrt 2 :: Double))
      hw  = cellSize * 3 `div` 16
      rel = [ ( ext,   0)
            , (   0, -hw)
            , (   0, -sw)
            , (-ext, -sw)
            , (-ext,  sw)
            , (   0,  sw)
            , (   0,  hw)
            ]
      rot (x, y) = (cx + x*adx - y*ady, cy + x*ady + y*adx)
  in  polygon_
        [ fill_   color
        , points_ (ptsStr (map rot rel))
        , style_  $ Map.fromList [("pointerEvents", "none")]
        ]
        []

-- Bold × symbol centered at (cx, cy).
crossPoly :: Int -> Int -> [View Msg]
crossPoly cx cy =
  let s      = cellSize * 5 `div` 16
      w      = cellSize `div` 13
      off (dx, dy) = (cx + dx, cy + dy)
      strip1 = map off [(-s, -s+w), (-s+w, -s), ( s,  s-w), ( s-w,  s)]
      strip2 = map off [( s-w, -s), ( s, -s+w), (-s+w,  s), (-s,  s-w)]
      mkPoly pts = polygon_
                     [ fill_ "black", points_ (ptsStr pts)
                     , style_ $ Map.fromList [("pointerEvents", "none")]
                     ] []
  in  [mkPoly strip1, mkPoly strip2]

-- ─── Board Cells ────────────────────────────────────────────────────────────

cellRect :: Int -> Int -> Int -> View Msg
cellRect numRows c r =
  rect_
    [ x_      (ms (boardX c))
    , y_      (ms (boardY numRows r))
    , width_  (ms cellSize)
    , height_ (ms cellSize)
    , fill_   "white"
    , style_  $ Map.fromList [("stroke", "black"), ("stroke-width", "2")]
    ]
    []

-- White cell with a bold × symbol at SVG position (lx, ly).
voidCellView :: Int -> Int -> [View Msg]
voidCellView lx ly =
  rect_ [ x_ (ms lx), y_ (ms ly)
        , width_ (ms cellSize), height_ (ms cellSize)
        , fill_ "white"
        , style_ $ Map.fromList [("stroke", "black"), ("stroke-width", "2")]
        ] []
  : crossPoly (lx + cellSize `div` 2) (ly + cellSize `div` 2)

-- Void cells at the four corners.
voidCells :: Dims -> [View Msg]
voidCells (Dims numRows numCols) = concatMap (uncurry voidCellView)
  [ (0,                         0)
  , ((numCols + 1) * cellSize,  0)
  , (0,                         (numRows + 1) * cellSize)
  , ((numCols + 1) * cellSize,  (numRows + 1) * cellSize)
  ]

-- P1 → red circle, P2 → blue upward triangle.
stoneDot :: Int -> Player -> Int -> Int -> View Msg
stoneDot numRows p c r =
  let cx     = boardCX c
      cy     = boardCY numRows r
      radius = cellSize `div` 2 - 5
      triPts = ptsStr
        [ (cx,          cy - 3 * radius `div` 4)
        , (cx - radius, cy + 3 * radius `div` 4)
        , (cx + radius, cy + 3 * radius `div` 4)
        ]
  in case p of
       P1 -> circle_  [ cx_ (ms cx), cy_ (ms cy), r_ (ms radius), fill_ "red" ] []
       P2 -> polygon_ [ fill_ "blue", points_ triPts ] []

-- Small black dot in the top-left corner of the last placed stone's cell.
lastPlacedDot :: Model -> [View Msg]
lastPlacedDot model = case lastPlaced model of
  Nothing     -> []
  Just (Coord c r) ->
    let Dims totalRows _ = activeDims model
        x                = boardX c + 6
        y                = boardY totalRows r + 6
    in  [ circle_ [ cx_ (ms x), cy_ (ms y), r_ "4", fill_ "black"
                  , style_ $ Map.fromList [("pointerEvents", "none")]
                  ] [] ]

-- Highlight the predicted landing cell when hovering a launcher.
landingPreview :: Model -> [View Msg]
landingPreview model
  | phase model /= Playing = []
  | otherwise =
      let dims@(Dims totalRows _) = activeDims model
      in  case hoverL model >>= launchStone dims (board model) (voids model) of
            Nothing       -> []
            Just (Coord lc lr) ->
              [ rect_
                  [ x_ (ms (boardX lc)), y_ (ms (boardY totalRows lr))
                  , width_ (ms cellSize), height_ (ms cellSize)
                  , style_ $ Map.fromList
                      [ ("fill",          "rgba(0,0,0,0.15)")
                      , ("pointerEvents", "none")
                      ]
                  ]
                  []
              ]

-- A launcher cell: white clickable background + bold directional arrow.
launcherCell :: Model -> Launcher -> [View Msg]
launcherCell model launcher =
  let dims       = activeDims model
      (lx, ly)   = launcherXY dims launcher
      playable   = phase model == Playing
                && isHumanTurn model
                && launchStone dims (board model) (voids model) launcher /= Nothing
      hovered    = hoverL model == Just launcher
      arrowColor
        | hovered && playable = playerColor (curPlayer model)
        | playable            = "black"
        | otherwise           = "#cccccc"
  in  [ rect_
          [ x_ (ms lx), y_ (ms ly)
          , width_ (ms cellSize), height_ (ms cellSize)
          , fill_ "white"
          , style_ $ Map.fromList
              [ ("stroke",       "black")
              , ("stroke-width", "2")
              , ("cursor",       if playable then "pointer" else "default")
              ]
          , onMouseEnter (SetHover (Just launcher))
          , onMouseLeave (SetHover Nothing)
          , onClick (Launch launcher)
          ]
          []
      , arrowPoly
          (lx + cellSize `div` 2)
          (ly + cellSize `div` 2)
          (launcherArrowDir launcher)
          arrowColor
      ]

-- ─── Board SVG ──────────────────────────────────────────────────────────────

boardSvg :: Model -> View Msg
boardSvg model =
  let dims@(Dims totalRows totalCols) = activeDims model
      svgW = ms ((totalCols + 2) * cellSize)
      svgH = ms ((totalRows + 2) * cellSize)
  in  svg_ [ width_ svgW, height_ svgH
           , style_ $ Map.fromList
               [ ("display",  "block")
               , ("margin",   "0 auto")
               , ("overflow", "visible")
               ]
           ]
      (  voidCells dims
      ++ [ cellRect totalRows c r
         | c <- [0 .. totalCols - 1]
         , r <- [0 .. totalRows - 1]
         ]
      ++ concatMap (\(Coord c r) -> voidCellView (boardX c) (boardY totalRows r))
                   (Set.toList (voids model))
      ++ [ stoneDot totalRows p c r
         | c      <- [0 .. totalCols - 1]
         , r      <- [0 .. totalRows - 1]
         , Just p <- [Map.lookup (Coord c r) (board model)]
         ]
      ++ lastPlacedDot model
      ++ landingPreview model
      ++ concatMap (launcherCell model) (allLaunchers dims)
      )

-- ─── Controls ───────────────────────────────────────────────────────────────

controlStyle :: Map.Map MisoString MisoString
controlStyle = Map.fromList
  [ ("padding",      "4px 12px")
  , ("fontSize",     "1em")
  , ("fontWeight",   "500")
  , ("cursor",       "pointer")
  , ("background",   "#ffffff")
  , ("color",        "#000000")
  , ("border",       "2px solid #000000")
  , ("borderRadius", "4px")
  ]

-- A styled dropdown: encode/decode pair, current value, and (value, label) options.
dropdown :: Eq a => (a -> MisoString) -> (MisoString -> Msg) -> a -> [(a, MisoString)] -> View Msg
dropdown encode toMsg current options =
  select_
    [ onChange toMsg, value_ (encode current), style_ controlStyle ]
    [ option_ [ value_ (encode val), selected_ (current == val) ] [ text label ]
    | (val, label) <- options
    ]

controlBar :: Model -> View Msg
controlBar model =
  div_ [ style_ $ Map.fromList
           [ ("display",        "flex")
           , ("justifyContent", "center")
           , ("alignItems",     "center")
           , ("gap",            "8px")
           ]
       ]
  (  [ modeSelect ]
  ++ pvcControls
  ++ [ sizeSelect
     , button_ [ onClick Restart, style_ controlStyle ] [ text "New Game" ]
     ]
  )
  where
    modeSelect = dropdown encodeMode (SelectMode . decodeMode) (gameMode model)
      [ (PvP, "PvP"), (PvC, "PvC") ]

    pvcControls
      | gameMode model /= PvC = []
      | otherwise =
          [ dropdown encodeDiff (SelectDiff . decodeDiff) (difficulty model)
              [ (Easy, "Easy"), (Hard, "Hard") ]
          , dropdown encodeTurnOrder (SelectTurnOrder . decodeTurnOrder) (selectedTurnOrder model)
              [ (HumanFirst, "Human first"), (ComputerFirst, "Computer first") ]
          ]

    sizeSelect = dropdown encodeDims (SelectSize . decodeDims) (selectedDims model)
      [ (Dims 8 8, "8×8"), (Dims 8 12, "8×12"), (Dims 12 12, "12×12") ]

-- ─── Top-Level View ─────────────────────────────────────────────────────────

gameView :: Model -> View Msg
gameView model =
  div_ [ style_ $ Map.fromList
           [ ("fontFamily", "sans-serif")
           , ("textAlign",  "center")
           , ("padding",    "16px")
           , ("background", "#ffffff")
           , ("minHeight",  "100vh")
           , ("color",      "#000000")
           ]
       ]
  [ h1_ [] [ text "Crossfire" ]
  , p_  [ style_ $ Map.fromList [("fontSize", "1.5em"), ("marginTop", "0")] ]
        [ text "Launch stones, get 4 in a row" ]
  , boardSvg model
  , p_  [ style_ $ Map.fromList [("fontSize", "1.5em"), ("margin", "16px 0")] ]
        [ text (statusMsg model) ]
  , controlBar model
  ]
