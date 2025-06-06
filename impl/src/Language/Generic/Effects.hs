-- Copyright (c) 2025, Marvin Borner
{-# LANGUAGE FlexibleContexts #-}

module Language.Generic.Effects
  ( resolveEffect
  , wrapNodeZero
  ) where

import           Control.Monad
import           Data.Effects                   ( EffectData(..)
                                                , EffectFunction
                                                )
import qualified Data.Text                     as T
import           Data.TokenPassing
import           GraphRewriting.Graph.Read
import           GraphRewriting.Graph.Write
import           GraphRewriting.Layout.Wrapper as Layout
import           GraphRewriting.Pattern
import           GraphRewriting.Pattern.InteractionNet
import           GraphRewriting.Rule
import           Language.Generic.NodeTransformer

import           Debug.Trace

churchTrue :: Edge -> Replace (Layout.Wrapper NodeTP) ()
churchTrue edge = do
  tok <- byEdge -- send token back!
  var <- byEdge
  era <- byEdge
  con <- byEdge
  byNode $ wrapNodeZero Eraser { inp = era }
  byNode $ wrapNodeZero Abstractor { inp = tok, body = con, var = var }
  byNode $ wrapNodeZero Abstractor { inp = con, body = var, var = era }
  byNode $ wrapNodeZero Token { inp = edge, out = tok }

churchFalse :: Edge -> Replace (Layout.Wrapper NodeTP) ()
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
-- resolveEffect :: T.Text -> EffectFunction n
resolveEffect "readInt" [UnitData] edge = do
  tok <- byEdge -- send token back!
  trace "readInt" $ byNode $ wrapNodeZero Data { inp = tok
                                               , dat = NumberData 42
                                               }
  byNode $ wrapNodeZero Token { inp = edge, out = tok }
resolveEffect "writeInt" [NumberData n] edge = do
  tok <- byEdge -- send token back!
  trace ("writeInt: " <> show n) $ byNode $ wrapNodeZero Data { inp = tok
                                                              , dat = UnitData
                                                              }
  byNode $ wrapNodeZero Token { inp = edge, out = tok }
resolveEffect "equal" [NumberData b, NumberData a] edge | a == b =
  trace ("equal: " <> show a <> " " <> show b) $ churchTrue edge
resolveEffect "equal" [NumberData b, NumberData a] edge | a /= b =
  trace ("not equal: " <> show a <> " " <> show b) $ churchFalse edge
resolveEffect "add" [NumberData b, NumberData a] edge = do
  tok <- byEdge -- send token back!
  trace ("add: " <> show a <> " " <> show b) $ byNode $ wrapNodeZero Data
    { inp = tok
    , dat = NumberData (a + b)
    }
  byNode $ wrapNodeZero Token { inp = edge, out = tok }
resolveEffect "sub" [NumberData b, NumberData a] edge = do
  tok <- byEdge -- send token back!
  trace ("sub: " <> show a <> " " <> show b) $ byNode $ wrapNodeZero Data
    { inp = tok
    , dat = NumberData (a - b)
    }
  byNode $ wrapNodeZero Token { inp = edge, out = tok }
resolveEffect _ _ _ = error "invalid action"
