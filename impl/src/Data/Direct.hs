-- Parts were originally written for lambdascope in `Graph.hs`
--   under the BSD-3-Clause License (as in src/GraphRewriting/LICENSE)
-- Copyright (c) 2010, Jan Rochel
-- Copyright (c) 2024, Marvin Borner

{-# LANGUAGE KindSignatures, UndecidableInstances, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses #-}
module Data.Direct
  ( NodeDS(..)
  , AppDir(..)
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

data AppDir = Top | BottomLeft | BottomRight

-- | The signature of our graph
data NodeDS
        = Initiator   {out :: Port}
        | Abstractor  {inp, body, var :: Port}
        | Eraser      {inp :: Port}
        | Duplicator  {level :: Int, inp, out1, out2 :: Port}
        | Multiplexer {out :: Port, ins :: [Port]} -- only intermediate compilation result
        | Token       {inp, out :: Port}
        | Actor       {inp :: Port, name :: T.Text, arity :: Int, args :: [EffectData]}
        | ActorC      {inp, cur :: Port, name :: T.Text, arity :: Int, args :: [EffectData]}
        | Recursor    {inp :: Port, boxed :: Lambda.Term }
        | Data        {inp :: Port, dat :: EffectData} -- TODO: custom eraser interaction?
        | Redirector  {portA, portB, portC :: Port, direction :: AppDir}

instance Eq NodeDS where
  Eraser{}                  == Eraser{}                       = True
  Token{}                   == Token{}                        = True -- for async actions
  Abstractor{}              == Redirector { direction = Top } = True -- both CON in SIC!
  Duplicator { level = l1 } == Duplicator { level = l2 }      = l1 == l2
  _                         == _                              = False

instance View [Port] NodeDS where
  inspect node = case node of
    Initiator { out = o }                          -> [o]
    Abstractor { inp = i, body = b, var = v }      -> [i, b, v]
    Eraser { inp = i }                             -> [i]
    Duplicator { inp = i, out1 = o1, out2 = o2 }   -> [i, o1, o2]
    Multiplexer { out = o, ins = is }              -> o : is
    Actor { inp = i }                              -> [i]
    ActorC { inp = i, cur = c }                    -> [i, c]
    Recursor { inp = i }                           -> [i]
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
    Actor{}       -> node { inp = i } where [i] = ports
    ActorC{}      -> node { inp = i, cur = c } where [i, c] = ports
    Recursor{}    -> node { inp = i } where [i] = ports
    Redirector{}  -> node { portA = a, portB = b, portC = c }
      where [a, b, c] = ports
    Token{} -> node { inp = i, out = o } where [i, o] = ports
    Data{}  -> node { inp = i } where [i] = ports

instance INet NodeDS where
  principalPort = pp

-- The number is an index that specifies which port is the principal port out of the list of ports
pp :: NodeDS -> Port
pp node = case node of
  Initiator { out = o }                        -> o
  Abstractor { inp = i, body = b, var = v }    -> i
  Eraser { inp = i }                           -> i
  Duplicator { inp = i, out1 = o1, out2 = o2 } -> i
  Multiplexer { out = o, ins = is }            -> o
  Actor { inp = i }                            -> i
  ActorC { inp = i }                           -> i
  Recursor { inp = i }                         -> i
  Redirector { portA = a, direction = Top }    -> a
  Redirector { portB = b, direction = BottomRight } -> b
  Redirector { portC = c, direction = BottomLeft } -> c
  Token { inp = i }                            -> i
  Data { inp = i }                             -> i

instance GenericNode NodeDS where
  gInitiator  = Initiator
  gApplicator = \inp func arg ->
    Redirector { portA = inp, portB = func, portC = arg, direction = Top }
  gAbstractor  = Abstractor
  gEraser      = Eraser
  gDuplicator  = Duplicator
  gMultiplexer = Multiplexer
  gToken       = Token
  gActor       = Actor
  gActorC      = ActorC
  gRecursor    = Recursor
  gData        = Data

  gpp          = pp

  -- Racket-style :)
  isInitiator Initiator{} = True
  isInitiator _           = False
  isApplicator Redirector { direction = Top } = True
  isApplicator _                              = False
  isAbstractor Abstractor{} = True
  isAbstractor _            = False
  isEraser Eraser{} = True
  isEraser _        = False
  isDuplicator Duplicator{} = True
  isDuplicator _            = False
  isMultiplexer Multiplexer{} = True
  isMultiplexer _             = False
  isToken Token{} = True
  isToken _       = False
  isActor Actor{} = True
  isActor _       = False
  isActorC ActorC{} = True
  isActorC _        = False
  isRecursor Recursor{} = True
  isRecursor _          = False
  isData Data{} = True
  isData _      = False
