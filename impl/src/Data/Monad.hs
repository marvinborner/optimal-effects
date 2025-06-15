-- Parts were originally written for lambdascope in `Graph.hs`
--   under the BSD-3-Clause License (as in src/GraphRewriting/LICENSE)
-- Copyright (c) 2010, Jan Rochel
-- Copyright (c) 2024, Marvin Borner

{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
module Data.Monad
  ( NodeMS(..)
  , pp
  ) where

import           Data.Effects                   ( EffectData )
import qualified Data.Lambda                   as Lambda
                                                ( Term )
import qualified Data.Text                     as T
import           Data.View
import           GraphRewriting.Graph.Types
import           GraphRewriting.Layout.Wrapper as Layout
import           GraphRewriting.Pattern.InteractionNet
import           GraphRewriting.Rule
import           Language.Generic.Node

-- | The signature of our graph
data NodeMS
        = Initiator   {out :: Port}
        | Applicator  {inp, func, arg :: Port}
        | Abstractor  {inp, body, var :: Port}
        | Eraser      {inp :: Port}
        | Duplicator  {level :: Int, inp, out1, out2 :: Port}
        | Multiplexer {out :: Port, ins :: [Port]} -- only intermediate compilation result
        | Token       {inp, out :: Port}
        | Actor       {inp :: Port, name :: T.Text, arity :: Int, args :: [EffectData]}
        | ActorC      {inp, cur :: Port, name :: T.Text, arity :: Int, args :: [EffectData]}
        | Recursor    {inp :: Port, boxed :: Lambda.Term }
        | Data        {inp :: Port, dat :: EffectData} -- TODO: custom eraser interaction?
        -- custom
        | BindN       {inp, arg, next, var :: Port, exec :: Bool}
        | UnitN       {inp, out :: Port}

instance GenericNode NodeMS where
  gInitiator   = Initiator
  gApplicator  = Applicator
  gAbstractor  = Abstractor
  gEraser      = Eraser
  gDuplicator  = Duplicator
  gMultiplexer = Multiplexer
  gToken       = Token
  gActor       = Actor
  gActorC      = ActorC
  gRecursor    = Recursor
  gData        = Data

instance Eq NodeMS where
  Eraser{}                  == Eraser{}                  = True
  Abstractor{}              == Applicator{}              = True -- both CON in SIC!
  Duplicator { level = l1 } == Duplicator { level = l2 } = l1 == l2
  _                         == _                         = False

instance View [Port] NodeMS where
  inspect node = case node of
    Initiator { out = o }                         -> [o]
    Applicator { inp = i, func = f, arg = a }     -> [i, f, a]
    Abstractor { inp = i, body = b, var = v }     -> [i, b, v]
    Eraser { inp = i }                            -> [i]
    Duplicator { inp = i, out1 = o1, out2 = o2 }  -> [i, o1, o2]
    Multiplexer { out = o, ins = is }             -> o : is
    Actor { inp = i }                             -> [i]
    ActorC { inp = i, cur = c }                   -> [i, c]
    Recursor { inp = i }                          -> [i]
    Token { inp = i, out = o }                    -> [i, o]
    Data { inp = i }                              -> [i]
    BindN { inp = i, arg = a, next = n, var = v } -> [i, a, n, v]
    UnitN { inp = i, out = o }                    -> [i, o]
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
    Actor{}       -> node { inp = i } where [i] = ports
    ActorC{}      -> node { inp = i, cur = c } where [i, c] = ports
    Recursor{}    -> node { inp = i } where [i] = ports
    Token{}       -> node { inp = i, out = o } where [i, o] = ports
    Data{}        -> node { inp = i } where [i] = ports
    BindN{}       -> node { inp = i, arg = a, next = n, var = v }
      where [i, a, n, v] = ports
    UnitN{} -> node { inp = i, out = o } where [i, o] = ports

instance INet NodeMS where
  principalPort = pp

-- The number is an index that specifies which port is the principal port out of the list of ports
pp :: NodeMS -> Port
pp node = case node of
  Initiator { out = o }                        -> o
  Applicator { inp = i, func = f, arg = a }    -> f
  Abstractor { inp = i, body = b, var = v }    -> i
  Eraser { inp = i }                           -> i
  Duplicator { inp = i, out1 = o1, out2 = o2 } -> i
  Multiplexer { out = o, ins = is }            -> o
  Actor { inp = i }                            -> i
  ActorC { inp = i }                           -> i
  Recursor { inp = i }                         -> i
  Token { inp = i }                            -> i
  Data { inp = i }                             -> i
  BindN { inp = i, exec = False }              -> i
  BindN { arg = a, exec = True }               -> a
  UnitN { inp = i }                            -> i
