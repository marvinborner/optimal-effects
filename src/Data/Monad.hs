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
                                                ( ForkType(..)
                                                , Term
                                                )
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
        | Fork        {tpe :: Lambda.ForkType, inp, lhs, rhs :: Port, exec :: Bool}
        | BindN       {inp, arg, var :: Port, exec :: Bool}
        | UnitN       {inp, out :: Port}
        | Wrap        {node :: NodeMS, kind :: WrapType}

instance Eq NodeMS where
  Eraser{}                  == Eraser{}                  = True
  Token{}                   == Token{}                   = True -- for async actions
  Abstractor{}              == Applicator{}              = True -- both CON in SIC!
  Duplicator { level = l1 } == Duplicator { level = l2 } = l1 == l2
  Wrap { node = n1 }        == Wrap { node = n2 }        = n1 == n2
  _                         == _                         = False

instance View [Port] NodeMS where
  inspect node = case node of
    Initiator { out = o }                        -> [o]
    Applicator { inp = i, func = f, arg = a }    -> [i, f, a]
    Abstractor { inp = i, body = b, var = v }    -> [i, b, v]
    Eraser { inp = i }                           -> [i]
    Duplicator { inp = i, out1 = o1, out2 = o2 } -> [i, o1, o2]
    Multiplexer { out = o, ins = is }            -> o : is
    Actor { inp = i }                            -> [i]
    ActorC { inp = i, cur = c }                  -> [i, c]
    Recursor { inp = i }                         -> [i]
    Token { inp = i, out = o }                   -> [i, o]
    Data { inp = i }                             -> [i]
    Fork { inp = i, lhs = l, rhs = r }           -> [i, l, r]
    BindN { inp = i, arg = a, var = v }          -> [i, a, v]
    UnitN { inp = i, out = o }                   -> [i, o]
    Wrap { node = n }                            -> inspect n
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
    Actor{} -> node { inp = i } where [i] = ports
    ActorC{} -> node { inp = i, cur = c } where [i, c] = ports
    Recursor{} -> node { inp = i } where [i] = ports
    Token{} -> node { inp = i, out = o } where [i, o] = ports
    Data{} -> node { inp = i } where [i] = ports
    Fork{} -> node { inp = i, lhs = l, rhs = r } where [i, l, r] = ports
    BindN{} -> node { inp = i, arg = a, var = v } where [i, a, v] = ports
    UnitN{} -> node { inp = i, out = o } where [i, o] = ports
    Wrap { node = n, kind = w } -> Wrap (update ports n) w

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
  Fork { inp = i, exec = False }               -> i
  Fork { lhs = l, exec = True }                -> l -- rhs implicit
  BindN { inp = i, exec = False }              -> i
  BindN { arg = a, exec = True }               -> a
  UnitN { inp = i }                            -> i
  Wrap { node = n }                            -> pp n

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
  gFork        = Fork
  gWrap        = Wrap

  gpp          = pp

  -- Racket-style :)
  isInitiator Initiator{} = True
  isInitiator (Wrap n _)  = isInitiator n
  isInitiator _           = False
  isApplicator Applicator{} = True
  isApplicator (Wrap n _)   = isApplicator n
  isApplicator _            = False
  isAbstractor Abstractor{} = True
  isAbstractor (Wrap n _)   = isAbstractor n
  isAbstractor _            = False
  isEraser Eraser{}   = True
  isEraser (Wrap n _) = isEraser n
  isEraser _          = False
  isDuplicator Duplicator{} = True
  isDuplicator (Wrap n _)   = isDuplicator n
  isDuplicator _            = False
  isMultiplexer Multiplexer{} = True
  isMultiplexer (Wrap n _)    = isMultiplexer n
  isMultiplexer _             = False
  isToken Token{}    = True
  isToken (Wrap n _) = isToken n
  isToken _          = False
  isActor Actor{}    = True
  isActor (Wrap n _) = isActor n
  isActor _          = False
  isActorC ActorC{}   = True
  isActorC (Wrap n _) = isActorC n
  isActorC _          = False
  isRecursor Recursor{} = True
  isRecursor (Wrap n _) = isRecursor n
  isRecursor _          = False
  isData Data{}     = True
  isData (Wrap n _) = isData n
  isData _          = False
  isFork Fork{}     = True
  isFork (Wrap n _) = isFork n
  isFork _          = False

  isWrapType w (Wrap _ w') = w == w'
  isWrapType w _           = False
