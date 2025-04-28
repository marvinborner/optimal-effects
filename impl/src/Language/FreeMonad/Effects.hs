{-# LANGUAGE FlexibleContexts #-}

module Language.FreeMonad.Effects
  ( resolveEffect
  , wrapNodeZero
  ) where

import           Control.Monad
import           Data.FreeMonad
import qualified Data.Text                     as T
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

-- TODO: allow IO via monad
resolveEffect :: T.Text -> EffectFunction
resolveEffect "readInt" args edge =
  trace ("readInt: " <> show args) $ byNode $ wrapNodeZero Data { inp = edge
                                                                , dat = "42"
                                                                }
resolveEffect "writeInt" args edge = do
  trace ("writeInt: " <> show args) $ byNode $ wrapNodeZero Data { inp = edge
                                                                 , dat = ""
                                                                 }
resolveEffect _ _ _ = error "invalid effect"
