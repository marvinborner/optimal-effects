-- Parts were originally written for lambdascope in `Graph.hs`
--   under the BSD-3-Clause License (as in src/GraphRewriting/LICENSE)
-- Copyright (c) 2010, Jan Rochel
-- Copyright (c) 2024, Marvin Borner

{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
module Data.Core
  ( NodeLS(..)
  , pp
  , lmo
  ) where

import           Data.View
import           GraphRewriting.Graph.Types
import           GraphRewriting.Pattern.InteractionNet
import           GraphRewriting.Strategies.LeftmostOutermost

-- | The signature of our graph
data NodeLS
        = Initiator   {out :: Port}
        | Applicator  {inp, func, arg :: Port}
        | Abstractor  {inp, body, var :: Port, name :: String}
        | Constant    {inp :: Port, args :: [Port], name :: String}
        | Eraser      {inp :: Port}
        | Duplicator  {level :: Int, inp, out1, out2 :: Port}
        | Delimiter   {level :: Int, inp, out :: Port}
        | Multiplexer {out :: Port, ins :: [Port]} -- only intermediate compilation result
        | Case        {inp :: Port, out :: Port, alts :: [Port], names :: [String]}
        | Operator    {inp :: Port, ops :: [Port], arity :: Int, lmop :: Int,
                       function :: [String] -> Maybe String, name :: String}

-- | equality as defined in the paper with only the relevant cases included
instance Eq NodeLS where
  Eraser{}                  == Eraser{}                  = True
  Duplicator { level = l1 } == Duplicator { level = l2 } = l1 == l2
  Delimiter { level = l1 }  == Delimiter { level = l2 }  = l1 == l2
  _                         == _                         = False

instance View [Port] NodeLS where
  inspect node = case node of
    Initiator { out = o }                        -> [o]
    Applicator { inp = i, func = f, arg = a }    -> [i, f, a]
    Abstractor { inp = i, body = b, var = v }    -> [i, b, v]
    Constant { inp = i, args = as }              -> i : as
    Eraser { inp = i }                           -> [i]
    Duplicator { inp = i, out1 = o1, out2 = o2 } -> [i, o1, o2]
    Delimiter { inp = i, out = o }               -> [i, o]
    Multiplexer { out = o, ins = is }            -> o : is
    Case { inp = i, out = o, alts = as }         -> i : o : as
    Operator { inp = i, ops = os }               -> i : os
  update ports node = case node of
    Initiator{}  -> node { out = o } where [o] = ports
    Applicator{} -> node { inp = i, func = f, arg = a }
      where [i, f, a] = ports
    Abstractor{} -> node { inp = i, body = b, var = v }
      where [i, b, v] = ports
    Constant{}   -> node { inp = i, args = as } where i : as = ports
    Eraser{}     -> node { inp = i } where [i] = ports
    Duplicator{} -> node { inp = i, out1 = o1, out2 = o2 }
      where [i, o1, o2] = ports
    Delimiter{}   -> node { inp = i, out = o } where [i, o] = ports
    Multiplexer{} -> node { out = o, ins = is } where o : is = ports
    Case{} -> node { inp = i, out = o, alts = as } where i : o : as = ports
    Operator{}    -> node { inp = i, ops = os } where i : os = ports

instance INet NodeLS where
  principalPort = pp

-- The number is an index that specifies which port is the principal port out of the list of ports
pp :: NodeLS -> Port
pp node = case node of
  Initiator { out = o }                        -> o
  Applicator { inp = i, func = f, arg = a }    -> f
  Abstractor { inp = i, body = b, var = v }    -> i
  Constant { inp = i, args = as }              -> i
  Eraser { inp = i }                           -> i
  Duplicator { inp = i, out1 = o1, out2 = o2 } -> i
  Delimiter { inp = i, out = o }               -> i
  Multiplexer { out = o, ins = is }            -> o
  Case { inp = i, out = o, alts = as }         -> o
  Operator { lmop = i, ops = os }              -> inspect node !! i

instance LeftmostOutermost NodeLS where
  lmoPort = lmo

lmo :: NodeLS -> Maybe Port
lmo node = case node of
  Initiator { out = o }                        -> Just o
  Applicator { inp = i, func = f, arg = a }    -> Just f
  Abstractor { inp = i, body = b, var = v }    -> Nothing
  Constant { inp = i, args = as }              -> Nothing
  Eraser { inp = i }                           -> Just i
  Duplicator { inp = i, out1 = o1, out2 = o2 } -> Just i
  Delimiter { inp = i, out = o }               -> Just i
  Case { inp = i, out = o, alts = as }         -> Just o
  Operator { lmop = i, ops = os }              -> Just $ inspect node !! i
