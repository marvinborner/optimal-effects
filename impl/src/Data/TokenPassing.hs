-- Parts were originally written for lambdascope in `Graph.hs`
--   under the BSD-3-Clause License (as in src/GraphRewriting/LICENSE)
-- Copyright (c) 2010, Jan Rochel
-- Copyright (c) 2024, Marvin Borner

{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
module Data.TokenPassing
  ( NodeLS(..)
  , AppDir(..)
  , EffectFunction
  , pp
  ) where

import qualified Data.Text                     as T
import           Data.View
import           GraphRewriting.Graph.Types
import           GraphRewriting.Layout.Wrapper as Layout
import           GraphRewriting.Pattern.InteractionNet
import           GraphRewriting.Rule

data AppDir = Top | BottomLeft | BottomRight

-- | Effects *effectively* get two arguments, the output port and the argument port
type EffectFunction = Edge -> Edge -> Replace (Layout.Wrapper NodeLS) ()

-- | The signature of our graph
data NodeLS
        = Initiator   {out :: Port}
        | Abstractor  {inp, body, var :: Port}
        | Eraser      {inp :: Port}
        | Duplicator  {level :: Int, inp, out1, out2 :: Port}
        | Multiplexer {out :: Port, ins :: [Port]} -- only intermediate compilation result
        | Effectful   {inp :: Port, name :: T.Text, function :: EffectFunction}
        | Redirector  {portA, portB, portC :: Port, direction :: AppDir}
        | Token       {inp, out :: Port}
        | Data        {inp :: Port, dat :: String} -- TODO: custom eraser interaction?

instance Eq NodeLS where
  Eraser{}                  == Eraser{}                       = True
  Abstractor{}              == Redirector { direction = Top } = True -- both CON in SIC!
  Duplicator { level = l1 } == Duplicator { level = l2 }      = l1 == l2
  _                         == _                              = False

instance View [Port] NodeLS where
  inspect node = case node of
    Initiator { out = o }                          -> [o]
    Abstractor { inp = i, body = b, var = v }      -> [i, b, v]
    Eraser { inp = i }                             -> [i]
    Duplicator { inp = i, out1 = o1, out2 = o2 }   -> [i, o1, o2]
    Multiplexer { out = o, ins = is }              -> o : is
    Effectful { inp = i }                          -> [i]
    Redirector { portA = a, portB = b, portC = c } -> [a, b, c]
    Token { inp = i, out = o }                     -> [i, o]
    Data { inp = i }                               -> [i]
  update ports node = case node of
    Initiator{}  -> node { out = o } where [o] = ports
    Abstractor{} -> node { inp = i, body = b, var = v }
      where [i, b, v] = ports
    Eraser{}     -> node { inp = i } where [i] = ports
    Duplicator{} -> node { inp = i, out1 = o1, out2 = o2 }
      where [i, o1, o2] = ports
    Multiplexer{} -> node { out = o, ins = is } where o : is = ports
    Effectful{}   -> node { inp = i } where [i] = ports
    Redirector{}  -> node { portA = a, portB = b, portC = c }
      where [a, b, c] = ports
    Token{} -> node { inp = i, out = o } where [i, o] = ports
    Data{}  -> node { inp = i } where [i] = ports

instance INet NodeLS where
  principalPort = pp

-- The number is an index that specifies which port is the principal port out of the list of ports
pp :: NodeLS -> Port
pp node = case node of
  Initiator { out = o }                        -> o
  Abstractor { inp = i, body = b, var = v }    -> i
  Eraser { inp = i }                           -> i
  Duplicator { inp = i, out1 = o1, out2 = o2 } -> i
  Multiplexer { out = o, ins = is }            -> o
  Effectful { inp = i }                        -> i
  Redirector { portA = a, direction = Top }    -> a
  Redirector { portB = b, direction = BottomRight } -> b
  Redirector { portC = c, direction = BottomLeft } -> c
  Token { inp = i }                            -> i
  Data { inp = i }                             -> i
