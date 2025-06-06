module Data.Effects where

import           GraphRewriting.Layout.Wrapper as Layout
import           GraphRewriting.Rule

data EffectData = StringData String | NumberData Int | UnitData

instance Show EffectData where
  show (StringData s) = "<" <> s <> ">"
  show (NumberData n) = "<" <> show n <> ">"
  show UnitData       = "<>"

type EffectFunction n = [EffectData] -> Edge -> Replace n ()
