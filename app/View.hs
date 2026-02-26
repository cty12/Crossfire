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

-- ─── View ────────────────────────────────────────────────────────────────────

playerName :: Player -> MisoString
playerName P1 = "Red"
playerName P2 = "Blue"

playerColor :: Player -> MisoString
playerColor P1 = "red"
playerColor P2 = "blue"

controlStyle :: Map.Map MisoString MisoString
controlStyle = Map.fromList
  [ ("padding",      "8px 24px")
  , ("fontSize",     "1em")
  , ("cursor",       "pointer")
  , ("background",   "#ffffff")
  , ("color",        "#000000")
  , ("border",       "1px solid #000000")
  , ("borderRadius", "4px")
  ]

-- Encode (rows, cols) as "rows,cols" for use as an option value.
encodeDims :: Dims -> MisoString
encodeDims (rows, cols) = ms (show rows ++ "," ++ show cols)

-- Decode "rows,cols" back to (rows, cols).
decodeDims :: MisoString -> Dims
decodeDims s =
  let (rs, rest) = break (== ',') (fromMisoString s)
  in  (read rs, read (drop 1 rest))

encodeMode :: GameMode -> MisoString
encodeMode PvP = "pvp"
encodeMode PvC = "pvc"

decodeMode :: MisoString -> GameMode
decodeMode "pvc" = PvC
decodeMode _     = PvP

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
  , div_ [ style_ $ Map.fromList
             [ ("display",        "flex")
             , ("justifyContent", "center")
             , ("alignItems",     "center")
             , ("gap",            "8px")
             ]
         ]
      [ select_
          [ onChange (SelectMode . decodeMode)
          , value_   (encodeMode (gameMode model))
          , style_   controlStyle
          ]
          [ option_ [ value_ "pvp" ] [ text "PvP" ]
          , option_ [ value_ "pvc" ] [ text "PvC" ]
          ]
      , select_
          [ onChange (SelectSize . decodeDims)
          , value_   (encodeDims (selectedDims model))
          , style_   controlStyle
          ]
          [ option_ [ value_ "8,8"   ] [ text "8×8"   ]
          , option_ [ value_ "8,12"  ] [ text "8×12"  ]
          , option_ [ value_ "12,12" ] [ text "12×12" ]
          ]
      , button_ [ onClick Restart, style_ controlStyle ] [ text "New Game" ]
      ]
  ]

statusMsg :: Model -> MisoString
statusMsg model = case phase model of
  Playing -> case gameMode model of
               PvC | curPlayer model == P2 -> "Computer's turn"
               _                           -> playerName (curPlayer model) <> "'s turn"
  Won p   -> playerName p <> " wins!"
  Draw    -> "It's a draw!"

-- ─── SVG Coordinate Helpers ──────────────────────────────────────────────────
-- The board is offset by one cell in each direction to make room for launchers.

boardX :: Int -> Int
boardX col = (col + 1) * cellSize

boardY :: Int -> Int -> Int   -- rows row; row 0 = bottom
boardY rows row = (rows - row) * cellSize

boardCX :: Int -> Int
boardCX col = boardX col + cellSize `div` 2

boardCY :: Int -> Int -> Int
boardCY rows row = boardY rows row + cellSize `div` 2

-- SVG top-left corner of a launcher cell.
launcherXY :: Dims -> Launcher -> (Int, Int)
launcherXY (rows, cols) launcher = case launcher of
  TopL    c -> (boardX c,                    0)
  BottomL c -> (boardX c,                    (rows + 1) * cellSize)
  LeftL   r -> (0,                           boardY rows r)
  RightL  r -> ((cols + 1) * cellSize,       boardY rows r)

-- Arrow direction in SVG pixel space (y-axis points down).
launcherArrowDir :: Launcher -> (Int, Int)
launcherArrowDir (TopL    _) = ( 0,  1)
launcherArrowDir (BottomL _) = ( 0, -1)
launcherArrowDir (LeftL   _) = ( 1,  0)
launcherArrowDir (RightL  _) = (-1,  0)

-- Format a list of (x,y) pairs as an SVG points string.
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

-- White cell with a bold × symbol at SVG position (lx, ly).
voidCellView :: Int -> Int -> [View Msg]
voidCellView lx ly =
  rect_ [ x_ (ms lx), y_ (ms ly)
        , width_ (ms cellSize), height_ (ms cellSize)
        , fill_ "white"
        , style_ $ Map.fromList [("stroke", "black"), ("stroke-width", "2")]
        ] []
  : crossPoly (lx + cellSize `div` 2) (ly + cellSize `div` 2)

-- ─── Board SVG ───────────────────────────────────────────────────────────────

boardSvg :: Model -> View Msg
boardSvg model =
  let dims@(rows, cols) = activeDims model
      svgW = ms ((cols + 2) * cellSize)
      svgH = ms ((rows + 2) * cellSize)
  in  svg_ [ width_ svgW, height_ svgH
           , style_ $ Map.fromList
               [ ("display",  "block")
               , ("margin",   "0 auto")
               , ("overflow", "visible")
               ]
           ]
      (  voidCells dims
      ++ [ cellRect rows c r | c <- [0 .. cols - 1], r <- [0 .. rows - 1] ]
      ++ concatMap (\(c,r) -> voidCellView (boardX c) (boardY rows r)) (Set.toList (voids model))
      ++ [ stoneDot rows p c r
         | c      <- [0 .. cols - 1]
         , r      <- [0 .. rows - 1]
         , Just p <- [Map.lookup (c, r) (board model)]
         ]
      ++ landingPreview model
      ++ concatMap (launcherCell model) (allLaunchers dims)
      )

-- Void cells at the four corners: no launcher, marked with a bold × symbol.
voidCells :: Dims -> [View Msg]
voidCells (rows, cols) = concatMap (uncurry voidCellView)
  [ (0,                    0)
  , ((cols + 1) * cellSize, 0)
  , (0,                    (rows + 1) * cellSize)
  , ((cols + 1) * cellSize, (rows + 1) * cellSize)
  ]

-- One board grid cell: white with black border.
cellRect :: Int -> Int -> Int -> View Msg
cellRect rows col row =
  rect_
    [ x_      (ms (boardX col))
    , y_      (ms (boardY rows row))
    , width_  (ms cellSize)
    , height_ (ms cellSize)
    , fill_   "white"
    , style_  $ Map.fromList [("stroke", "black"), ("stroke-width", "2")]
    ]
    []

-- Highlight the predicted landing cell when hovering a launcher.
landingPreview :: Model -> [View Msg]
landingPreview model
  | phase model /= Playing = []
  | otherwise =
      let dims@(rows, _) = activeDims model
      in  case hoverL model >>= launchStone dims (board model) (voids model) of
            Nothing       -> []
            Just (lc, lr) ->
              [ rect_
                  [ x_ (ms (boardX lc)), y_ (ms (boardY rows lr))
                  , width_ (ms cellSize), height_ (ms cellSize)
                  , style_ $ Map.fromList
                      [ ("fill",          "rgba(0,0,0,0.15)")
                      , ("pointerEvents", "none")
                      ]
                  ]
                  []
              ]

-- P1 → red circle, P2 → blue upward triangle.
stoneDot :: Int -> Player -> Int -> Int -> View Msg
stoneDot rows p col row =
  let cx     = boardCX col
      cy     = boardCY rows row
      r      = cellSize `div` 2 - 5
      triPts = ptsStr [(cx, cy - 3*r `div` 4), (cx - r, cy + 3*r `div` 4), (cx + r, cy + 3*r `div` 4)]
  in case p of
       P1 -> circle_  [ cx_ (ms cx), cy_ (ms cy), r_ (ms r), fill_ "red"  ] []
       P2 -> polygon_ [ fill_ "blue", points_ triPts ] []

-- A launcher cell: white clickable background + bold directional arrow.
launcherCell :: Model -> Launcher -> [View Msg]
launcherCell model launcher =
  let dims    = activeDims model
      (lx, ly) = launcherXY dims launcher
      playable  = phase model == Playing
               && isHumanTurn model
               && launchStone dims (board model) (voids model) launcher /= Nothing
      hovered   = hoverL model == Just launcher
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
