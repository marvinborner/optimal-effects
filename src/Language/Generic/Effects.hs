-- Copyright (c) 2025, Marvin Borner
{-# LANGUAGE ScopedTypeVariables, TypeApplications, AllowAmbiguousTypes, FlexibleContexts #-}

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
import           Language.Generic.Node

import           Debug.Trace

churchTrue
  :: forall m n . (GenericNode m, View [Port] n, View m n) => Edge -> Rule n
churchTrue p = replace $ do
  tok <- byEdge -- send token back!
  var <- byEdge
  era <- byEdge
  con <- byEdge
  byNode $ gEraser @m era
  byNode $ gAbstractor @m tok con var
  byNode $ gAbstractor @m con var era
  byNode $ gToken @m p tok

churchFalse
  :: forall m n . (GenericNode m, View [Port] n, View m n) => Edge -> Rule n
churchFalse p = replace $ do
  tok <- byEdge -- send token back!
  var <- byEdge
  era <- byEdge
  con <- byEdge
  byNode $ gEraser @m era
  byNode $ gAbstractor @m tok con era
  byNode $ gAbstractor @m con var var
  byNode $ gToken @m p tok

-- TODO: allow IO via monad
executeActor
  :: forall m n
   . (GenericNode m, View [Port] n, View m n)
  => T.Text
  -> [EffectData]
  -> Port
  -> Rule n
executeActor "readInt" [UnitData] p = replace $ do
  tok <- byEdge -- send token back!
  trace "readInt" $ byNode $ gData @m tok (NumberData 42)
  byNode $ gToken @m p tok
executeActor "writeInt" [NumberData n] p = replace $ do
  tok <- byEdge -- send token back!
  trace ("writeInt: " <> show n) $ byNode $ gData @m tok UnitData
  byNode $ gToken @m p tok
executeActor "equal" [NumberData b, NumberData a] p
  | a == b = trace ("equal: " <> show a <> " " <> show b) $ churchTrue @m p
  | a /= b = trace ("not equal: " <> show a <> " " <> show b) $ churchFalse @m p
executeActor "succ" [NumberData n] p = replace $ do
  tok <- byEdge -- send token back!
  trace ("succ: " <> show n) $ byNode $ gData @m tok (NumberData $ n + 1)
  byNode $ gToken @m p tok
executeActor "add" [NumberData b, NumberData a] p = replace $ do
  tok <- byEdge -- send token back!
  trace ("add: " <> show a <> " " <> show b) $ byNode $ gData @m
    tok
    (NumberData $ a + b)
  byNode $ gToken @m p tok
executeActor "pred" [NumberData n] p = replace $ do
  tok <- byEdge -- send token back!
  trace ("pred: " <> show n) $ byNode $ gData @m tok (NumberData $ n - 1)
  byNode $ gToken @m p tok
executeActor "sub" [NumberData b, NumberData a] p = replace $ do
  tok <- byEdge -- send token back!
  trace ("sub: " <> show a <> " " <> show b) $ byNode $ gData @m
    tok
    (NumberData $ a - b)
  byNode $ gToken @m p tok
executeActor "mul" [NumberData b, NumberData a] p = replace $ do
  tok <- byEdge -- send token back!
  trace ("mul: " <> show a <> " " <> show b) $ byNode $ gData @m
    tok
    (NumberData $ a * b)
  byNode $ gToken @m p tok
executeActor "div" [NumberData b, NumberData a] p = replace $ do
  tok <- byEdge -- send token back!
  trace ("div: " <> show a <> " " <> show b) $ byNode $ gData @m
    tok
    (NumberData $ a `div` b)
  byNode $ gToken @m p tok
executeActor n args _ =
  error $ "invalid action " <> show n <> ": " <> show args
