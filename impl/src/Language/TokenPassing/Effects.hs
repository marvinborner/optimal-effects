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

wrapNodeZero :: NodeLS -> Layout.Wrapper NodeLS
wrapNodeZero n = Layout.Wrapper
  { wRot    = Rotation 0
  , wPos    = Position { position = Vector2 { v2x = 0, v2y = 0 } }
  , wrappee = n
  }

-- TODO: read-back arg to some representation
-- TODO: allow IO via monad
resolveEffect :: T.Text -> EffectFunction
resolveEffect "readInt" edge arg = do
  byNode $ wrapNodeZero Data { inp = edge, dat = "42" }
  byNode $ wrapNodeZero Eraser { inp = arg }
resolveEffect "writeInt" edge arg = do
  byNode $ wrapNodeZero Data { inp = edge, dat = "42" }
  byNode $ wrapNodeZero Eraser { inp = arg }
-- resolveEffect "writeInt" _ arg = replace $ byNode Eraser { inp = arg }
resolveEffect _ _ _ = error "invalid effect"
