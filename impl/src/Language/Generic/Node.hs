-- Copyright (c) 2025, Marvin Borner

module Language.Generic.Node
  ( GenericNode(..)
  ) where

import           Data.Effects                   ( EffectData )
import qualified Data.Lambda                   as Lambda
                                                ( Term )
import qualified Data.Text                     as T
import           GraphRewriting.Graph.Types

class GenericNode n where
  gInitiator   :: Port -> n
  gApplicator  :: Port -> Port -> Port -> n
  gAbstractor  :: Port -> Port -> Port -> n
  gEraser      :: Port -> n
  gDuplicator  :: Int -> Port -> Port -> Port -> n
  gMultiplexer :: Port -> [Port] -> n
  gToken       :: Port -> Port -> n
  gActor       :: Port -> T.Text -> Int -> [EffectData] -> n
  gActorC      :: Port -> Port -> T.Text -> Int -> [EffectData] -> n
  gRecursor    :: Port -> Lambda.Term -> n
  gData        :: Port -> EffectData -> n
