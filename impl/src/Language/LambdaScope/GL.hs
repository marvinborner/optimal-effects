-- Parts were originally written for lambdascope in `GL.hs`
--   under the BSD-3-Clause License (as in src/GraphRewriting/LICENSE)
-- Copyright (c) 2010, Jan Rochel
-- Copyright (c) 2024, Marvin Borner

{-# LANGUAGE FlexibleInstances #-}
module Language.LambdaScope.GL
  () where

import           Data.LambdaScope
import           GraphRewriting.GL.Render
import           GraphRewriting.Layout.PortSpec
import           GraphRewriting.Strategies.Control
import qualified Graphics.UI.GLUT              as GL


instance PortSpec NodeLS where
  portSpec node =
    let sd = sameDir
    in
      case node of
        Initiator{}  -> [sd s]
        Applicator{} -> [sd n, sd s, sd e]
        Abstractor{} -> [sd n, sd s, sd e]
        Constant{} ->
          sd n
            : [ sd $ rm (alpha args * x) `mmul` sws | x <- [0 .. la args - 1] ]
        Eraser{} -> [sd n]
        Duplicator{} ->
          [ (Vector2 0 0.9        , n)
          , (Vector2 (-0.6) (-0.5), s)
          , (Vector2 0.6 (-0.5)   , s)
          ]
        Delimiter{} -> [sd $ Vector2 0 0.7, sd $ Vector2 0 (-0.7)]
        Effectful{} ->
          sd n
            : [ sd $ rm (alpha args * x) `mmul` sws | x <- [0 .. la args - 1] ]
   where
    n = Vector2 0 1
    e = Vector2 1 0
    s = Vector2 0 (-1)

    la field = toEnum $ length (field node)
    alpha f = pi / (2 * la f)
    rm t = ((cos t, sin t), (-sin t, cos t))
    mmul ((x1, y1), (x2, y2)) (Vector2 x y) =
      Vector2 (x1 * x + x2 * y) (y1 * x + y2 * y)
    sws = Vector2 (-0.7) (-0.7)

instance Render NodeLS where
  render = renderNode

instance Render n => Render (Wrapper n) where
  render c = do
    render $ wrapped c
    case control c of
      NoControl -> return ()
      Control{} -> GL.renderPrimitive GL.LineLoop (circle 1.2 1.2 20)

renderNode node = drawPorts node >> case node of
  Initiator{}  -> drawNode "I"
  Applicator{} -> drawNode "@"
  Abstractor{} -> drawNode (name node)
  Constant{}   -> drawNode (name node)
  Eraser{}     -> drawNode ""
  Duplicator{} -> do
    GL.preservingMatrix $ GL.renderPrimitive GL.LineLoop $ do
      vertex2 (0, 0.9)
      vertex2 (-1, -0.5)
      vertex2 (1, -0.5)
    renderString $ show $ level node
  Delimiter{} -> do
    GL.preservingMatrix $ GL.renderPrimitive GL.LineStrip $ do
      vertex2 (-0.8, -0.3)
      vertex2 (-0.8, 0)
      vertex2 (0.8, 0)
      vertex2 (0.8, -0.3)
    renderString $ show $ level node
  Effectful{} -> drawNode (name node)

drawPorts :: NodeLS -> IO ()
drawPorts n = sequence_
  [ drawPort (factor p) pos | (pos, p) <- positions `zip` ports ] where
  positions = relPortPos n
  ports     = inspect n
  isLmo p = maybe False (p ==) (lmo n)
  factor p | isLmo p   = 1.5
           | p == pp n = 2.0
           | otherwise = 1

circle r1 r2 step = mapM_ vertex2 vs where
  is = take (truncate step + 1) [0, i' ..]
  i' = 2 * pi / step
  vs = [ (r1 * cos i, r2 * sin i) | i <- is ]

drawPort factor pos = GL.preservingMatrix $ do
  GL.translate $ vector pos
  GL.renderPrimitive GL.Polygon (circle (factor * 0.15) (factor * 0.15) 20)

drawNode label = do
  GL.renderPrimitive GL.LineLoop (circle 1 1 20)
  renderString label
