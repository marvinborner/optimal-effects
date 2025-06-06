-- Copyright (c) 2025, Marvin Borner
{-# LANGUAGE FlexibleInstances, FlexibleContexts, UndecidableInstances, MultiParamTypeClasses, TypeFamilies #-}

-- we require this abstraction since actors expand to new nets via `Replace`
--  while we generally use `Rewrite`!

module Language.Generic.NodeTransformer where

import           GraphRewriting.Graph.Write
                                         hiding ( Node )
import           GraphRewriting.Layout.Wrapper as Layout
                                         hiding ( Node )
import           GraphRewriting.Rule
import           GraphRewriting.Strategies.Control
                                               as Control

import           Data.TokenPassing

-- wrapNodeZero :: View [Port] n => n -> Layout.Wrapper n
wrapNodeZero n = Layout.Wrapper
  { wRot    = Rotation 0
  , wPos    = Position { position = Vector2 { v2x = 0, v2y = 0 } }
  , wrappee = n
  }

wrapNodeControl :: View [Port] n => n -> Control.Wrapper n
wrapNodeControl n = Control.Wrapper { control = Control.Control [] }

class Monad m => Transformer m where
  type Node m
  edge :: m Edge
  node :: View [Port] (Node m) => Node m -> m ()
  conn :: Edge -> Edge -> m ()

instance View [Port] n => Transformer (Rewrite n) where
  type Node (Rewrite n) = n
  edge = newEdge
  node = newNode'
  conn = mergeEdges

instance View [Port] n => Transformer (Replace (Layout.Wrapper n)) where
  type Node (Replace (Layout.Wrapper n)) = n
  edge = byEdge
  node = byNode . wrapNodeZero
  conn = byWire

instance View [Port] n => Transformer (Replace (Control.Wrapper n)) where
  type Node (Replace (Control.Wrapper n)) = n
  edge = byEdge
  node = byNode . wrapNodeControl
  conn = byWire
