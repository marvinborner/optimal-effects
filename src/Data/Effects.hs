module Data.Effects where

import qualified Data.Text                     as T
import           GraphRewriting.Layout.Wrapper
import           GraphRewriting.Rule

builtinActions :: [T.Text]
builtinActions =
  [ "readInt"
  , "writeInt"
  , "print"
  , "readFile"
  , "writeFile"
  , "succ"
  , "pred"
  , "isEqual"
  , "add"
  , "sub"
  , "mul"
  , "div"
  , "download"
  , "evalLang"
  , "concat"
  ]

actionArity :: T.Text -> Int
actionArity "readInt"   = 1
actionArity "writeInt"  = 1
actionArity "print"     = 1
actionArity "readFile"  = 1
actionArity "writeFile" = 2
actionArity "succ"      = 1
actionArity "pred"      = 1
actionArity "isEqual"   = 2
actionArity "add"       = 2
actionArity "sub"       = 2
actionArity "mul"       = 2
actionArity "div"       = 2
actionArity "download"  = 2
actionArity "evalLang"  = 3
actionArity "concat"    = 2
actionArity _           = -1

data EffectData = StringData String | NumberData Int | UnitData

instance Show EffectData where
  show (StringData s) = "<" <> s <> ">"
  show (NumberData n) = "<" <> show n <> ">"
  show UnitData       = "<>"

type EffectFunction n = [EffectData] -> Edge -> Replace n ()
