-- Copyright (c) 2025, Marvin Borner
{-# LANGUAGE FlexibleContexts #-}

module Language.Generic.Node
  ( GenericNode(..)
  ) where

import           Data.Effects                   ( EffectData )
import qualified Data.Lambda                   as Lambda
                                                ( ForkType
                                                , Term
                                                )
import qualified Data.Text                     as T
import           Data.View
import           GraphRewriting.Graph.Types
import           GraphRewriting.Pattern.InteractionNet

class (Eq n, INet n, View [Port] n) => GenericNode n where
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
  gFork        :: Lambda.ForkType -> Port -> Port -> Port -> Bool -> n

  gpp :: n -> Port

  isInitiator :: n -> Bool
  isApplicator :: n -> Bool
  isAbstractor :: n -> Bool
  isEraser :: n -> Bool
  isDuplicator :: n -> Bool
  isMultiplexer :: n -> Bool
  isToken :: n -> Bool
  isActor :: n -> Bool
  isActorC :: n -> Bool
  isRecursor :: n -> Bool
  isData :: n -> Bool
  isFork :: n -> Bool
  isConjunctive :: n -> Bool
  isDisjunctive :: n -> Bool
