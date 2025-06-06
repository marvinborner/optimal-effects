-- Copyright (c) 2025, Marvin Borner
{-# LANGUAGE FlexibleContexts #-}

module Language.Generic.Effects
  ( resolveEffect
  , wrapNodeZero
  ) where

import           Data.Effects                   ( EffectData(..)
                                                , EffectFunction
                                                )
import qualified Data.Text                     as T
import           Language.Generic.NodeTransformer

-- TODO: abstract over TokenPassing!
import           Data.TokenPassing
import           Debug.Trace

-- churchTrue :: Edge -> Replace (Layout.Wrapper (NodeTP n)) ()
churchTrue p = do
  tok <- edge -- send token back!
  var <- edge
  era <- edge
  con <- edge
  node Eraser { inp = era }
  node Abstractor { inp = tok, body = con, var = var }
  node Abstractor { inp = con, body = var, var = era }
  node Token { inp = p, out = tok }

-- churchFalse :: Edge -> Replace (Layout.Wrapper (NodeTP n)) ()
churchFalse p = do
  tok <- edge -- send token back!
  var <- edge
  era <- edge
  con <- edge
  node Eraser { inp = era }
  node Abstractor { inp = tok, body = con, var = era }
  node Abstractor { inp = con, body = var, var = var }
  node Token { inp = p, out = tok }

-- TODO: allow IO via monad
-- TODO: passing without argument will execute unapplied, we should then just return the action node (??)
-- resolveEffect :: T.Text -> EffectFunction n
-- resolveEffect
--   :: ( Node m ~ NodeTP n
--      , Transformer m => T.Text -> [EffectData] -> Port -> m ()
--      )
resolveEffect "readInt" [UnitData] p = do
  tok <- edge -- send token back!
  trace "readInt" $ node Data { inp = tok, dat = NumberData 42 }
  node Token { inp = p, out = tok }
resolveEffect "writeInt" [NumberData n] p = do
  tok <- edge -- send token back!
  trace ("writeInt: " <> show n) $ node Data { inp = tok, dat = UnitData }
  node Token { inp = p, out = tok }
resolveEffect "equal" [NumberData b, NumberData a] p | a == b =
  trace ("equal: " <> show a <> " " <> show b) $ churchTrue p
resolveEffect "equal" [NumberData b, NumberData a] p | a /= b =
  trace ("not equal: " <> show a <> " " <> show b) $ churchFalse p
resolveEffect "add" [NumberData b, NumberData a] p = do
  tok <- edge -- send token back!
  trace ("add: " <> show a <> " " <> show b)
    $ node Data { inp = tok, dat = NumberData (a + b) }
  node Token { inp = p, out = tok }
resolveEffect "sub" [NumberData b, NumberData a] p = do
  tok <- edge -- send token back!
  trace ("sub: " <> show a <> " " <> show b)
    $ node Data { inp = tok, dat = NumberData (a - b) }
  node Token { inp = p, out = tok }
resolveEffect _ _ _ = error "invalid action"
