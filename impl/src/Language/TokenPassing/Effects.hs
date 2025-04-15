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

import Debug.Trace

wrapNodeZero :: NodeLS -> Layout.Wrapper NodeLS
wrapNodeZero n = Layout.Wrapper
  { wRot    = Rotation 0
  , wPos    = Position { position = Vector2 { v2x = 0, v2y = 0 } }
  , wrappee = n
  }

-- readArg :: View [Port] n => Edge -> Rewrite n ()
-- readArg arg = do
--   [Data { dat = dat }] <- attachedNodes arg
--   return dat

-- TODO: read-back arg to some representation (traversal)
-- TODO: allow IO via monad
resolveEffect :: T.Text -> EffectFunction
resolveEffect t = trace ("EFFECT: " <> T.unpack t) go t
  where
  go "readInt" edge arg = do
    byNode $ wrapNodeZero Data { inp = edge, dat = "42" }
    byNode $ wrapNodeZero Eraser { inp = arg }
  go "writeInt" edge arg = do
    -- arg' <- readArg
    byNode $ wrapNodeZero Data { inp = edge, dat = "42" }
    byNode $ wrapNodeZero Eraser { inp = arg }
  go _ _ _ = error "invalid effect"
