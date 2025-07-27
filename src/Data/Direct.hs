-- Parts were originally written for lambdascope in `Graph.hs`
--   under the BSD-3-Clause License (as in src/GraphRewriting/LICENSE)
-- Copyright (c) 2010, Jan Rochel
-- Copyright (c) 2024, Marvin Borner

{-# LANGUAGE KindSignatures, UndecidableInstances, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses #-}
module Data.Direct
  ( NodeDS(..)
  , AppDir(..)
  , WrapType(..)
  , pp
  ) where

import           Data.Effects                   ( EffectData )
import qualified Data.Lambda                   as Lambda
                                                ( ForkType(..)
                                                , Term
                                                )
import qualified Data.Text                     as T
import           Data.View
import           GraphRewriting.Graph.Types
import           GraphRewriting.Layout.Wrapper as Layout
import           GraphRewriting.Pattern.InteractionNet
import           GraphRewriting.Rule
import           GraphRewriting.Strategies.LeftmostOutermost
import           Language.Generic.Node

data AppDir = Top | BottomLeft | BottomRight

data WrapType = RecursiveNode | ImmediateNode

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
        | Fork        {tpe :: Lambda.ForkType, inp, lhs, rhs :: Port, exec :: Bool}
        | Redirector  {portA, portB, portC :: Port, direction :: AppDir}
        | Wrap        {node :: NodeDS, kind :: WrapType}

instance Eq NodeDS where
  Eraser{}                  == Eraser{}                       = True
  Token{}                   == Token{}                        = True -- for async actions
  Abstractor{}              == Redirector { direction = Top } = True -- both CON in SIC!
  Duplicator { level = l1 } == Duplicator { level = l2 }      = l1 == l2
  Wrap { node = n1 }        == Wrap { node = n2 }             = n1 == n2
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
    Fork { inp = i, lhs = l, rhs = r }             -> [i, l, r]
    Wrap { node = n }                              -> inspect n
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
    Token{}           -> node { inp = i, out = o } where [i, o] = ports
    Data{}            -> node { inp = i } where [i] = ports
    Fork{} -> node { inp = i, lhs = l, rhs = r } where [i, l, r] = ports
    Wrap { node = n } -> update ports n

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
  Fork { inp = i, exec = False }               -> i
  Fork { lhs = l, exec = True }                -> l -- rhs implicit
  Wrap { node = n }                            -> pp n

instance LeftmostOutermost NodeDS where
  lmoPort = lmo

lmo :: NodeDS -> Maybe Port
lmo node = case node of
  Initiator { out = o }                        -> Just o
  Abstractor { inp = i, body = b, var = v }    -> Nothing -- !?
  Eraser { inp = i }                           -> Just i
  Duplicator { inp = i, out1 = o1, out2 = o2 } -> Just i
  Multiplexer { out = o, ins = is }            -> Just o -- ???
  Actor { inp = i }                            -> Just i -- ???
  ActorC { inp = i }                           -> Just i -- ???
  Recursor { inp = i }                         -> Just i
  Redirector { portA = a, direction = Top }    -> Just a
  Redirector { portB = b, direction = BottomRight } -> Just b
  Redirector { portC = c, direction = BottomLeft } -> Just c
  Token { out = i }                            -> Just i
  Data { inp = i }                             -> Nothing -- ?
  Wrap { node = n }                            -> lmo n
  -- Constant { inp = i, args = as }              -> Nothing
  -- Delimiter { inp = i, out = o }               -> Just i
  -- Case { inp = i, out = o, alts = as }         -> Just o
  -- Operator { lmop = i, ops = os }              -> Just $ inspect node !! i

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
  gFork        = Fork

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
  isFork Fork{} = True
  isFork _      = False
