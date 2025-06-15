-- Copyright (c) 2025, Marvin Borner
{-# LANGUAGE TypeOperators, FlexibleContexts #-}

module Language.Generic.Effects
  ( executeActor
  ) where

import           Data.Effects                   ( EffectData(..)
                                                , EffectFunction
                                                )
import qualified Data.Text                     as T
import           Data.View
import           GraphRewriting.Graph.Types
import           GraphRewriting.Rule

-- TODO: abstract over TokenPassing!
import           Data.TokenPassing
import           Debug.Trace

-- churchTrue :: Edge -> Replace (Layout.Wrapper (NodeTP n)) ()
churchTrue p = replace $ do
  tok <- byEdge -- send token back!
  var <- byEdge
  era <- byEdge
  con <- byEdge
  byNode Eraser { inp = era }
  byNode Abstractor { inp = tok, body = con, var = var }
  byNode Abstractor { inp = con, body = var, var = era }
  byNode Token { inp = p, out = tok }

-- churchFalse :: Edge -> Replace (Layout.Wrapper (NodeTP n)) ()
churchFalse p = replace $ do
  tok <- byEdge -- send token back!
  var <- byEdge
  era <- byEdge
  con <- byEdge
  byNode Eraser { inp = era }
  byNode Abstractor { inp = tok, body = con, var = era }
  byNode Abstractor { inp = con, body = var, var = var }
  byNode Token { inp = p, out = tok }

-- TODO: allow IO via monad
-- TODO: passing without argument will execute unapplied, we should then just return the action node (??)
-- executeActor :: T.Text -> EffectFunction n
executeActor
  :: (View [Port] n, View NodeTP n) => T.Text -> [EffectData] -> Port -> Rule n
executeActor "readInt" [UnitData] p = replace $ do
  tok <- byEdge -- send token back!
  trace "readInt" $ byNode Data { inp = tok, dat = NumberData 42 }
  byNode Token { inp = p, out = tok }
executeActor "writeInt" [NumberData n] p = replace $ do
  tok <- byEdge -- send token back!
  trace ("writeInt: " <> show n) $ byNode Data { inp = tok, dat = UnitData }
  byNode Token { inp = p, out = tok }
executeActor "equal" [NumberData b, NumberData a] p | a == b =
  trace ("equal: " <> show a <> " " <> show b) $ churchTrue p
executeActor "equal" [NumberData b, NumberData a] p | a /= b =
  trace ("not equal: " <> show a <> " " <> show b) $ churchFalse p
executeActor "add" [NumberData b, NumberData a] p = replace $ do
  tok <- byEdge -- send token back!
  trace ("add: " <> show a <> " " <> show b)
    $ byNode Data { inp = tok, dat = NumberData (a + b) }
  byNode Token { inp = p, out = tok }
executeActor "sub" [NumberData b, NumberData a] p = replace $ do
  tok <- byEdge -- send token back!
  trace ("sub: " <> show a <> " " <> show b)
    $ byNode Data { inp = tok, dat = NumberData (a - b) }
  byNode Token { inp = p, out = tok }
executeActor "mul" [NumberData b, NumberData a] p = replace $ do
  tok <- byEdge -- send token back!
  trace ("mul: " <> show a <> " " <> show b)
    $ byNode Data { inp = tok, dat = NumberData (a * b) }
  byNode Token { inp = p, out = tok }
executeActor "div" [NumberData b, NumberData a] p = replace $ do
  tok <- byEdge -- send token back!
  trace ("div: " <> show a <> " " <> show b)
    $ byNode Data { inp = tok, dat = NumberData (a `div` b) }
  byNode Token { inp = p, out = tok }
executeActor _ _ _ = error "invalid action"
