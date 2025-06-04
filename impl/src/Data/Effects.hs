module Data.Effects where

import           GraphRewriting.Layout.Wrapper as Layout
import           GraphRewriting.Rule

data EffectData = StringData String | NumberData Int | UnitData
  deriving Show

type EffectFunction n = [EffectData] -> Edge -> Replace n ()
