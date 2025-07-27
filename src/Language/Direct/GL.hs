-- Parts were originally written for lambdascope in `GL.hs`
--   under the BSD-3-Clause License (as in src/GraphRewriting/LICENSE)
-- Copyright (c) 2010, Jan Rochel
-- Copyright (c) 2024, Marvin Borner

{-# LANGUAGE FlexibleInstances #-}
module Language.Direct.GL
  () where

import           Data.Direct
import           Data.Effects                   ( EffectData(..) )
import           Data.Lambda                    ( ForkType(..) )
import qualified Data.Text                     as T
import           GraphRewriting.GL.Render
import           GraphRewriting.Layout.PortSpec
import           GraphRewriting.Strategies.Control
import qualified Graphics.UI.GLUT              as GL
import           Language.Generic.GL

instance PortSpec NodeDS where
  portSpec node =
    let sd = sameDir
    in  case node of
          Initiator{}       -> [sd s]
          Abstractor{}      -> triangle
          Eraser{}          -> [sd n]
          Duplicator{}      -> triangle
          Redirector{}      -> triangle
          Actor{}           -> [sd n]
          ActorC{}          -> [sd n, sd s]
          Recursor{}        -> [sd n]
          Token{}           -> [sd n, sd s]
          Data{}            -> [sd n]
          Fork{}            -> triangle
          Multiplexer{}     -> [sd n, sd s]
          Wrap { node = n } -> portSpec n
   where
    n = Vector2 0 1
    e = Vector2 1 0
    s = Vector2 0 (-1)

    triangle =
      [(Vector2 0 0.9, n), (Vector2 0.6 (-0.5), s), (Vector2 (-0.6) (-0.5), s)]

    la field = toEnum $ length (field node)
    alpha f = pi / (2 * la f)
    rm t = ((cos t, sin t), (-sin t, cos t))
    mmul ((x1, y1), (x2, y2)) (Vector2 x y) =
      Vector2 (x1 * x + x2 * y) (y1 * x + y2 * y)
    sws = Vector2 (-0.7) (-0.7)

instance Render NodeDS where
  render = renderNode

instance Render n => Render (Wrapper n) where
  render c = do
    render $ wrapped c
    case control c of
      NoControl -> return ()
      Control{} -> GL.renderPrimitive GL.LineLoop (circle 1.2 1.2 20)

renderNode node = drawPorts node >> case node of
  Initiator{}                           -> drawNodeCircle "ι"
  Abstractor{}                          -> drawNode "λ"
  Eraser{}                              -> drawNodeCircle "ε"
  -- Duplicator{}                          -> drawNodeBlack $ show $ level node
  Duplicator{}                          -> drawNodeBlack ""
  Redirector { direction = Top }        -> drawNode "α"
  Redirector { direction = BottomRight } -> drawNode "αᵣ"
  Redirector { direction = BottomLeft } -> drawNode "αₗ"
  Actor { name = n, arity = a }         -> drawNode $ T.unpack n <> show a
  ActorC { name = n, arity = a } -> drawNode $ T.unpack n <> show a <> "ᶜ"
  Recursor{}                            -> drawNode "REC"
  Token{}                               -> drawNodeCircle "↑"
  Data { dat = UnitData }               -> drawNode "⟨⟩"
  Data { dat = StringData s }           -> drawNode $ "⟨" <> s <> "⟩"
  Data { dat = NumberData n }           -> drawNode $ "⟨" <> show n <> "⟩"
  Fork { exec = False }                 -> drawNode "ψ"
  Fork { tpe = Conjunctive }            -> drawNode "∧"
  Fork { tpe = Disjunctive }            -> drawNode "∨"
  Multiplexer{}                         -> drawNodeCircle "M"
  Wrap { node = n }                     -> renderNode n
  -- Wrap { kind = ImmediateNode }         -> drawNode "I"
  -- Wrap { kind = RecursiveNode }         -> drawNode "R"

drawPorts :: NodeDS -> IO ()
drawPorts n = sequence_
  [ drawPort (factor p) pos | (pos, p) <- positions `zip` ports ] where
  positions = relPortPos n
  ports     = inspect n
  factor p | p == pp n = 2.0
           | otherwise = 1
