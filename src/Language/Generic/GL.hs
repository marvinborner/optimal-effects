-- Parts were originally written for lambdascope in `GL.hs`
--   under the BSD-3-Clause License (as in src/GraphRewriting/LICENSE)
-- Copyright (c) 2010, Jan Rochel
-- Copyright (c) 2025, Marvin Borner

module Language.Generic.GL where

import           GraphRewriting.GL.Render
import qualified Graphics.UI.GLUT              as GL

circle r1 r2 step = mapM_ vertex2 vs where
  is = take (truncate step + 1) [0, i' ..]
  i' = 2 * pi / step
  vs = [ (r1 * cos i, r2 * sin i) | i <- is ]

drawPort factor pos = GL.preservingMatrix $ do
  GL.translate $ vector pos
  GL.renderPrimitive GL.Polygon (circle (factor * 0.15) (factor * 0.15) 20)

drawNode label = do
  GL.preservingMatrix $ GL.renderPrimitive GL.LineLoop $ do
    vertex2 (0, 0.9)
    vertex2 (-1, -0.5)
    vertex2 (1, -0.5)
  renderString 128 label

drawNodeCircle label = do
  GL.renderPrimitive GL.LineLoop (circle 1 1 20)
  renderString 164 label

drawNodeBlack _ = do
  GL.color (GL.Color3 0 0 0 :: GL.Color3 GL.GLfloat)
  GL.preservingMatrix $ GL.renderPrimitive GL.Polygon $ do
    vertex2 (0, 0.9)
    vertex2 (-1, -0.5)
    vertex2 (1, -0.5)

