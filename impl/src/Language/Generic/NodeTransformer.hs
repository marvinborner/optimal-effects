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

import           Data.TokenPassing

wrapNodeZero :: View [Port] n => n -> Layout.Wrapper n
wrapNodeZero n = Layout.Wrapper
  { wRot    = Rotation 0
  , wPos    = Position { position = Vector2 { v2x = 0, v2y = 0 } }
  , wrappee = n
  }

type Compiler n = Rewrite n
type Replacer n = Replace (Layout.Wrapper n)

class Monad m => Transformer m where
  type Node m
  edge :: m Edge
  node :: View [Port] (Node m) => Node m -> m ()
  conn :: Edge -> Edge -> m ()

instance View [Port] n => Transformer (Compiler n) where
  type Node (Compiler n) = n
  edge = newEdge
  node = newNode'
  conn = mergeEdges

instance View [Port] n => Transformer (Replacer n) where
  type Node (Replacer n) = n
  edge = byEdge
  node = byNode . wrapNodeZero
  conn = byWire
