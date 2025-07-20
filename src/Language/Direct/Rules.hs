-- Copyright (c) 2025, Marvin Borner

{-# LANGUAGE TypeApplications, FlexibleContexts, ScopedTypeVariables, FlexibleInstances #-}
module Language.Direct.Rules where

import           Control.Applicative            ( optional )
import           Control.Monad
import           Data.Direct
import           Data.Effects
import           Data.Lambda                    ( ForkType(..) )
import           Data.List                      ( delete
                                                , elemIndex
                                                , transpose
                                                )
import           Data.Maybe                     ( fromJust )
import qualified Data.Text                     as T
import           GraphRewriting.Graph.Read
                                         hiding ( Node )
import           GraphRewriting.Graph.Write
                                         hiding ( Node )
import           GraphRewriting.Layout.Wrapper as Layout
                                         hiding ( Node )
import           GraphRewriting.Pattern
import           GraphRewriting.Pattern.InteractionNet
import           GraphRewriting.Rule
import           GraphRewriting.Strategies.Control
                                               as Control
import           Language.Generic.Effects
import           Language.Generic.Node
import           Language.Generic.Rules
import           Language.Lambda.Transformer.Direct
                                                ( executeRecursor )

reflectsToken :: NodeDS -> Bool
reflectsToken Abstractor{}        = True
reflectsToken Actor { arity = a } = a > 0
reflectsToken Data{}              = True
reflectsToken _                   = False

reflectToken :: (View [Port] n, View NodeDS n) => Rule n
reflectToken = do
  reflector :-: tok@(Token { inp = iT, out = oT }) <- activePair
  guard $ reflectsToken reflector
  replace $ do
    byNode $ reflector { inp = iT }
    byNode $ tok { inp = oT, out = iT }

redirectToken :: (View [Port] n, View NodeDS n) => Rule n
redirectToken = do
  red@(Redirector { portA = a, portB = b, portC = c, direction = r }) :-: tok@(Token { inp = iT, out = oT }) <-
    activePair
  attached <- liftReader . flip adverseNodes iT =<< previous
  require $ length attached == 1 -- in order to prevent reds over multiplexed (e.g. disj fork)
  case r of
    BottomRight -> replace $ do
      v <- byEdge -- tok-bl edge
      byNode $ tok { inp = c, out = v }
      byNode $ red { portB = oT, portC = v, direction = BottomLeft }
    BottomLeft -> replace $ do
      v <- byEdge
      byNode $ tok { inp = a, out = v }
      byNode $ red { portA = v, portC = oT, direction = Top }
    Top -> replace $ do -- passthrough
      v <- byEdge
      byNode $ tok { inp = v, out = b }
      byNode $ red { portA = oT, portB = v, direction = Top }

execConjunctive :: (View [Port] n, View NodeDS n) => Rule n
execConjunctive = do
  fork@(Fork { tpe = Conjunctive, inp = i, lhs = l, rhs = r, exec = False }) :-: tok@(Token { out = oT }) <-
    activePair
  replace $ do
    tl <- byEdge
    tr <- byEdge
    byNode fork { inp = oT, lhs = tl, rhs = tr, exec = True }
    byNode tok { inp = l, out = tl }
    byNode tok { inp = r, out = tr }

execDisjunctive :: (View [Port] n, View NodeDS n) => Rule n
execDisjunctive = do
  fork@(Fork { tpe = Disjunctive, inp = i, lhs = l, rhs = r, exec = False }) :-: tok@(Token { out = oT }) <-
    activePair
  replace $ do -- we do not connect the right port in order to have ambiguity
    tl <- byEdge
    byNode fork { inp = oT, lhs = tl, rhs = tl, exec = True }
    byNode tok { inp = l, out = tl }
    byNode tok { inp = r, out = tl }

returnConjunctive :: (View [Port] n, View NodeDS n) => Rule n
returnConjunctive = do
  (Fork { tpe = Conjunctive, inp = i1, lhs = l1, rhs = r1, exec = True }) :-: tok1@(Token { inp = iT1, out = oT1 }) <-
    activePair
  (Token { inp = iT2, out = oT2 }) <- nodeWith r1
  (Fork { tpe = Conjunctive, inp = i2, lhs = l2, rhs = r2, exec = True }) <-
    nodeWith iT2
  require $ oT1 /= oT2 && i1 == i2 && l1 == l2 && r1 == r2 -- same fork, different tokens

  replace $ do
    t <- byEdge
    byNode Redirector { direction = Top, portA = oT1, portB = t, portC = oT2 }
    byNode Token { inp = t, out = i1 }

returnDisjunctive :: (View [Port] n, View NodeDS n) => Rule n
returnDisjunctive = do
  fork@(Fork { tpe = Disjunctive, inp = i, exec = True }) :-: tok@(Token { inp = iT, out = oT }) <-
    activePair
  replace $ do
    byNode tok { inp = i, out = oT }
    byNode Eraser { inp = iT }

-- this is basically almost !reflectsToken
hasActionPotential :: NodeDS -> Bool
hasActionPotential (Redirector { direction = BottomRight }) = True
hasActionPotential (Actor { arity = 0 }) = True
hasActionPotential (Fork{}             ) = True
hasActionPotential (Recursor{}         ) = True
hasActionPotential (Data{}             ) = False -- Must be False or possible loops with T-App
hasActionPotential _                     = False

isControlling :: NodeDS -> Bool
isControlling (Token{}     ) = True
isControlling (Duplicator{}) = True
isControlling (Eraser{}    ) = True
isControlling _              = False

backpropagateActor :: (View [Port] n, View NodeDS n) => Rule n
backpropagateActor = do -- ==> there is an action somewhere inside b
  a@(Redirector { direction = BottomLeft }) :-: b <- activePair
  guard $ hasActionPotential b
  guard $ not $ isControlling b
  replace $ do
    byNode $ a { direction = BottomRight }
    byNode b

backpropagateActor2 :: (View [Port] n, View NodeDS n) => Rule n
backpropagateActor2 = do -- ==> there is an action somewhere inside b
  a@(Redirector { direction = Top }) :-: b <- activePair
  guard $ hasActionPotential b
  guard $ not $ isControlling b
  replace $ do
    byNode $ a { direction = BottomRight }
    byNode b

backpropagateUneffectful :: (View [Port] n, View NodeDS n) => Rule n
backpropagateUneffectful = do -- ==> there is no immediate action potential in b
  a@(Redirector { direction = BottomLeft }) :-: b <- activePair
  guard $ not $ hasActionPotential b
  guard $ not $ isControlling b
  replace $ do
    byNode $ a { direction = Top }
    byNode b

initializeDataPartial :: (View [Port] n, View NodeDS n) => Rule n
initializeDataPartial = do
  act@(Actor { name = nm, arity = n, args = as }) :-: Redirector { portA = f, portB = p, portC = a, direction = Top } <-
    activePair
  guard $ n > 0
  replace $ byNode $ ActorC { inp   = a
                            , cur   = p
                            , name  = nm
                            , arity = n
                            , args  = as
                            }

applyDataPartial :: (View [Port] n, View NodeDS n) => Rule n
applyDataPartial = do
  act@(ActorC { name = nm, arity = n, args = as, cur = c }) :-: Data { inp = p, dat = d } <-
    activePair
  guard $ n > 0
  replace $ byNode $ Actor { inp = c, name = nm, arity = n - 1, args = d : as }

applyActor :: (View [Port] n, View NodeDS n) => Rule n
applyActor = do
  (Token { out = p, inp = i }) :-: (Actor { name = n, args = a, arity = 0 }) <-
    activePair
  executeActor @NodeDS n a p
  -- exhaustive $ compileShare @NodeDS -- TODO!

applyRecursor :: (View [Port] n, View NodeDS n) => AppDir -> Rule n
applyRecursor dir = do
  (Token { out = p, inp = i }) :-: (Recursor { boxed = t }) <- activePair
  executeRecursor dir t p
