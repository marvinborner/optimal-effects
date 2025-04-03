-- Parts were originally written for lambdascope in `Graph.hs`
--   under the BSD-3-Clause License (as in src/GraphRewriting/LICENSE)
-- Copyright (c) 2010, Jan Rochel
-- Copyright (c) 2024, Marvin Borner

{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
module Data.TokenPassing
  ( NodeLS(..)
  , pp
  , lmo
  ) where

import           Data.View
import           GraphRewriting.Graph.Types
import           GraphRewriting.Pattern.InteractionNet
import           GraphRewriting.Strategies.LeftmostOutermost

-- | The signature of our graph
-- TODO: rotating applications
data NodeLS
        = Initiator   {out :: Port}
        | Applicator  {inp, func, arg :: Port}
        | Abstractor  {inp, body, var :: Port}
        | Eraser      {inp :: Port}
        | Duplicator  {level :: Int, inp, out1, out2 :: Port}
        | Multiplexer {out :: Port, ins :: [Port]} -- only intermediate compilation result
        | Effectful   {inp :: Port, name :: String}
        | Token       {inp, out :: Port} -- TODO: actually use

instance Eq NodeLS where
  Eraser{}                  == Eraser{}                  = True
  Abstractor{}              == Applicator{}              = True -- both CON in SIC!
  Duplicator { level = l1 } == Duplicator { level = l2 } = l1 == l2
  _                         == _                         = False

instance View [Port] NodeLS where
  inspect node = case node of
    Initiator { out = o }                        -> [o]
    Applicator { inp = i, func = f, arg = a }    -> [i, f, a]
    Abstractor { inp = i, body = b, var = v }    -> [i, b, v]
    Eraser { inp = i }                           -> [i]
    Duplicator { inp = i, out1 = o1, out2 = o2 } -> [i, o1, o2]
    Multiplexer { out = o, ins = is }            -> o : is
    Effectful { inp = i }                        -> [i]
    Token { inp = i, out = o }                   -> [i, o]
  update ports node = case node of
    Initiator{}  -> node { out = o } where [o] = ports
    Applicator{} -> node { inp = i, func = f, arg = a }
      where [i, f, a] = ports
    Abstractor{} -> node { inp = i, body = b, var = v }
      where [i, b, v] = ports
    Eraser{}     -> node { inp = i } where [i] = ports
    Duplicator{} -> node { inp = i, out1 = o1, out2 = o2 }
      where [i, o1, o2] = ports
    Multiplexer{} -> node { out = o, ins = is } where o : is = ports
    Effectful{}   -> node { inp = i } where [i] = ports
    Token{}       -> node { inp = i } where [i] = ports

instance INet NodeLS where
  principalPort = pp

-- The number is an index that specifies which port is the principal port out of the list of ports
pp :: NodeLS -> Port
pp node = case node of
  Initiator { out = o }                        -> o
  Applicator { inp = i, func = f, arg = a }    -> f
  Abstractor { inp = i, body = b, var = v }    -> i
  Eraser { inp = i }                           -> i
  Duplicator { inp = i, out1 = o1, out2 = o2 } -> i
  Multiplexer { out = o, ins = is }            -> o
  Effectful { inp = i }                        -> i
  Token { inp = i }                            -> i

instance LeftmostOutermost NodeLS where
  lmoPort = lmo

lmo :: NodeLS -> Maybe Port
lmo node = case node of
  Initiator { out = o }                        -> Just o
  Applicator { inp = i, func = f, arg = a }    -> Just f
  Abstractor { inp = i, body = b, var = v }    -> Nothing
  Eraser { inp = i }                           -> Just i
  Duplicator { inp = i, out1 = o1, out2 = o2 } -> Just i
  Effectful { inp = i }                        -> Just i
  Token { inp = i }                            -> Just i
