-- Copyright (c) 2025, Marvin Borner

{-# LANGUAGE TypeApplications, FlexibleContexts, ScopedTypeVariables, FlexibleInstances #-}
module Language.Monad.Rules where

import           Control.Applicative            ( optional )
import           Control.Monad
import           Data.Lambda                    ( ForkType(..) )
import           Data.List                      ( delete
                                                , elemIndex
                                                , transpose
                                                )
import           Data.Maybe                     ( fromJust )
import           Data.Monad
import qualified Data.Text                     as T
import           GraphRewriting.Graph.Read
import           GraphRewriting.Graph.Write
import           GraphRewriting.Layout.Wrapper as Layout
import           GraphRewriting.Pattern
import           GraphRewriting.Pattern.InteractionNet
import           GraphRewriting.Rule
import           Language.Generic.Effects
import           Language.Generic.Node
import           Language.Generic.Rules
import           Language.Lambda.Transformer.Monad
                                                ( executeRecursor )

import           Debug.Trace

rotateBind :: (View [Port] n, View NodeMS n) => Rule n
rotateBind = do
  bind@(BindN { arg = a, exec = False }) :-: tok@(Token { out = oT }) <-
    activePair
  replace $ do
    t <- byEdge
    byNode bind { inp = oT, arg = t, exec = True }
    byNode tok { inp = a, out = t }

popUnit :: (View [Port] n, View NodeMS n) => Rule n
popUnit = do
  unit@(UnitN { out = a }) :-: tok@(Token { out = oT }) <- activePair
  replace $ byNode tok { inp = oT, out = a }

popBind :: (View [Port] n, View NodeMS n) => Rule n
popBind = do
  bind@(BindN { inp = i, arg = a, var = v, exec = True }) :-: tok@(Token { out = oT }) <-
    activePair
  replace $ do
    t <- byEdge
    byNode Applicator { inp = t, func = v, arg = oT }
    byNode tok { inp = t, out = i }

execConjunctive :: (View [Port] n, View NodeMS n) => Rule n
execConjunctive = do
  fork@(Fork { tpe = Conjunctive, inp = i, lhs = l, rhs = r, exec = False }) :-: tok@(Token { out = oT }) <-
    activePair
  replace $ do
    tl <- byEdge
    tr <- byEdge
    byNode fork { inp = oT, lhs = tl, rhs = tr, exec = True }
    byNode tok { inp = l, out = tl }
    byNode tok { inp = r, out = tr }

execDisjunctive :: (View [Port] n, View NodeMS n) => Rule n
execDisjunctive = do
  fork@(Fork { tpe = Disjunctive, inp = i, lhs = l, rhs = r, exec = False }) :-: tok@(Token { out = oT }) <-
    activePair
  replace $ do -- we do not connect the right port in order to have ambiguity
    tl <- byEdge
    byNode fork { inp = oT, lhs = tl, rhs = tl, exec = True }
    byNode tok { inp = l, out = tl }
    byNode tok { inp = r, out = tl }

returnConjunctive :: (View [Port] n, View NodeMS n) => Rule n
returnConjunctive = do
  (Fork { tpe = Conjunctive, inp = i1, lhs = l1, rhs = r1, exec = True }) :-: tok1@(Token { inp = iT1, out = oT1 }) <-
    activePair
  (Token { inp = iT2, out = oT2 }) <- nodeWith r1
  (Fork { tpe = Conjunctive, inp = i2, lhs = l2, rhs = r2, exec = True }) <-
    nodeWith iT2
  require $ oT1 /= oT2 && i1 == i2 && l1 == l2 && r1 == r2 -- same fork, different tokens

  replace $ do
    t <- byEdge
    byNode Applicator { inp = t, func = oT1, arg = oT2 }
    byNode Token { inp = t, out = i1 }

returnDisjunctive :: (View [Port] n, View NodeMS n) => Rule n
returnDisjunctive = do
  fork@(Fork { tpe = Disjunctive, inp = i, exec = True }) :-: tok@(Token { inp = iT, out = oT }) <-
    activePair
  -- forkNode <- liftReader . pure . head . tail =<< history
  -- attached <- liftReader . flip adverseNodes iT =<< previous -- fork + other token
  -- let [otherThread] = filter (/= forkNode) attached -- must always be 1 by construction
  -- otherThreadInput <- last <$> liftReader (attachedEdges otherThread) -- very hacky
  replace $ do
    byNode tok { inp = i, out = oT }
    byNode Eraser { inp = iT }
    -- byNode Eraser { inp = otherThreadInput }

initializeDataPartial :: (View [Port] n, View NodeMS n) => Rule n
initializeDataPartial = do
  act@(Actor { name = nm, arity = n, args = as }) :-: Applicator { func = f, inp = p, arg = a } <-
    activePair
  guard $ n > 0
  replace $ byNode $ ActorC { inp   = a
                            , cur   = p
                            , name  = nm
                            , arity = n
                            , args  = as
                            }

applyDataPartial :: (View [Port] n, View NodeMS n) => Rule n
applyDataPartial = do
  act@(ActorC { name = nm, arity = n, args = as, cur = c }) :-: Data { inp = p, dat = d } <-
    activePair
  guard $ n > 0
  replace $ byNode $ Actor { inp = c, name = nm, arity = n - 1, args = d : as }

applyActor :: (View [Port] n, View NodeMS n) => Rule n
applyActor = do
  (Token { out = p, inp = i }) :-: (Actor { name = n, args = a, arity = 0 }) <-
    activePair
  executeActor @NodeMS n a p

applyRecursor :: (View [Port] n, View NodeMS n) => Rule n
applyRecursor = do
  (Token { out = p, inp = i }) :-: (Recursor { boxed = t }) <- activePair
  executeRecursor t p >>> exhaustive (compileShare @NodeMS)
