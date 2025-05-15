{-# LANGUAGE FlexibleContexts #-}

module Language.TokenPassing.Effects
  ( resolveEffect
  , wrapNodeZero
  ) where

import           Control.Monad
import qualified Data.Text                     as T
import           Data.TokenPassing
import           GraphRewriting.Graph.Read
import           GraphRewriting.Graph.Write
import           GraphRewriting.Layout.Wrapper as Layout
import           GraphRewriting.Pattern
import           GraphRewriting.Pattern.InteractionNet
import           GraphRewriting.Rule

import           Debug.Trace

wrapNodeZero :: NodeLS -> Layout.Wrapper NodeLS
wrapNodeZero n = Layout.Wrapper
  { wRot    = Rotation 0
  , wPos    = Position { position = Vector2 { v2x = 0, v2y = 0 } }
  , wrappee = n
  }

churchTrue :: Edge -> Replace (Layout.Wrapper NodeLS) ()
churchTrue edge = do
  tok <- byEdge -- send token back!
  var <- byEdge
  era <- byEdge
  con <- byEdge
  byNode $ wrapNodeZero Eraser { inp = era }
  byNode $ wrapNodeZero Abstractor { inp = tok, body = con, var = var }
  byNode $ wrapNodeZero Abstractor { inp = con, body = var, var = era }
  byNode $ wrapNodeZero Token { inp = edge, out = tok }

churchFalse :: Edge -> Replace (Layout.Wrapper NodeLS) ()
churchFalse edge = do
  tok <- byEdge -- send token back!
  var <- byEdge
  era <- byEdge
  con <- byEdge
  byNode $ wrapNodeZero Eraser { inp = era }
  byNode $ wrapNodeZero Abstractor { inp = tok, body = con, var = era }
  byNode $ wrapNodeZero Abstractor { inp = con, body = var, var = var }
  byNode $ wrapNodeZero Token { inp = edge, out = tok }

-- TODO: allow IO via monad
-- TODO: passing without argument will execute unapplied, we should then just return the action node (??)
resolveEffect :: T.Text -> EffectFunction
resolveEffect "readInt" [UnitData] edge = Just $ do
  tok <- byEdge -- send token back!
  trace "readInt" $ byNode $ wrapNodeZero Data { inp = tok
                                               , dat = NumberData 42
                                               }
  byNode $ wrapNodeZero Token { inp = edge, out = tok }
resolveEffect "writeInt" [NumberData n] edge = Just $ do
  tok <- byEdge -- send token back!
  trace ("writeInt: " <> show n) $ byNode $ wrapNodeZero Data { inp = tok
                                                              , dat = UnitData
                                                              }
  byNode $ wrapNodeZero Token { inp = edge, out = tok }
resolveEffect "equal" [NumberData a, NumberData b] edge | a == b =
  Just $ trace ("equal: " <> show a <> " " <> show b) $ churchTrue edge
resolveEffect "equal" [NumberData a, NumberData b] edge | a /= b =
  Just $ trace ("not equal: " <> show a <> " " <> show b) $ churchFalse edge
resolveEffect f args _ =
  trace (T.unpack f <> " reflected " <> show args) Nothing -- bounce back
