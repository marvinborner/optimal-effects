-- Parts were originally written for lambdascope in `GL.hs`
--   under the BSD-3-Clause License (as in src/GraphRewriting/LICENSE)
-- Copyright (c) 2010, Jan Rochel
-- Copyright (c) 2024, Marvin Borner

{-# LANGUAGE FlexibleInstances #-}
module Language.Monad.GL
  () where

import           Data.Effects                   ( EffectData(..) )
import           Data.Monad
import qualified Data.Text                     as T
import           GraphRewriting.GL.Render
import           GraphRewriting.Layout.PortSpec
import           GraphRewriting.Strategies.Control
import qualified Graphics.UI.GLUT              as GL

instance PortSpec NodeMS where
  portSpec node =
    let sd = sameDir
    in
      case node of
        Initiator{}  -> [sd s]
        Applicator{} -> [sd n, sd s, sd e]
        Abstractor{} -> [sd n, sd s, sd e]
        Eraser{}     -> [sd n]
        Duplicator{} ->
          [ (Vector2 0 0.9        , n)
          , (Vector2 (-0.6) (-0.5), s)
          , (Vector2 0.6 (-0.5)   , s)
          ]
        Actor{}       -> [sd n]
        ActorC{}      -> [sd n, sd s]
        Recursor{}    -> [sd n]
        Token{}       -> [sd n, sd s]
        Data{}        -> [sd n]
        Multiplexer{} -> [sd n, sd s]
        BindN{}       -> [sd n, sd s, sd e, sd e] -- TODO
        UnitN{}       -> [sd n, sd s]
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

instance Render NodeMS where
  render = renderNode

instance Render n => Render (Wrapper n) where
  render c = do
    render $ wrapped c
    case control c of
      NoControl -> return ()
      Control{} -> GL.renderPrimitive GL.LineLoop (circle 1.2 1.2 20)

renderNode node = drawPorts node >> case node of
  Initiator{}                    -> drawNodeCircle "I"
  Applicator{}                   -> drawNode "@"
  Abstractor{}                   -> drawNode "L"
  Eraser{}                       -> drawNodeCircle "E"
  Duplicator{}                   -> drawNodeBlack $ show $ level node
  Actor { name = n, arity = a }  -> drawNode $ T.unpack n <> show a
  ActorC { name = n, arity = a } -> drawNode $ T.unpack n <> show a <> "c"
  Recursor{}                     -> drawNode "REC"
  Token{}                        -> drawNodeCircle "T"
  Data { dat = UnitData }        -> drawNodeCircle "()"
  Data { dat = StringData s }    -> drawNodeCircle $ "D=" <> s
  Data { dat = NumberData n }    -> drawNodeCircle $ "D=" <> show n
  Multiplexer{}                  -> drawNodeCircle "M"
  BindN{}                        -> drawNode ">>="
  UnitN{}                        -> drawNode "<>"

drawPorts :: NodeMS -> IO ()
drawPorts n = sequence_
  [ drawPort (factor p) pos | (pos, p) <- positions `zip` ports ] where
  positions = relPortPos n
  ports     = inspect n
  factor p | p == pp n = 2.0
           | otherwise = 1

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
  renderString label

drawNodeCircle label = do
  GL.renderPrimitive GL.LineLoop (circle 1 1 20)
  renderString label

drawNodeBlack label = do
  GL.color (GL.Color3 0 0 0 :: GL.Color3 GL.GLfloat)
  GL.preservingMatrix $ GL.renderPrimitive GL.Polygon $ do
    vertex2 (0, 0.9)
    vertex2 (-1, -0.5)
    vertex2 (1, -0.5)

  GL.color (GL.Color3 1 1 1 :: GL.Color3 GL.GLfloat)
  renderString label
