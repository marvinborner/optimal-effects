-- Copyright (c) 2025, Marvin Borner

{-# LANGUAGE TypeApplications, FlexibleContexts, ScopedTypeVariables, FlexibleInstances, AllowAmbiguousTypes #-}
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

rotateBind :: (View [Port] n, View NodeMS n) => WrapType -> Rule n
rotateBind w = do
  Wrap bind@(BindN { arg = a, exec = False }) w1 :-: Wrap tok@(Token { out = oT }) w2 <-
    activePair
  require $ w1 == w && w2 == w
  replace $ do
    t <- byEdge
    byNode $ Wrap bind { inp = oT, arg = t, exec = True } w1
    byNode $ Wrap tok { inp = a, out = t } w2

popUnit :: (View [Port] n, View NodeMS n) => WrapType -> Rule n
popUnit w = do
  Wrap unit@(UnitN { out = a }) w1 :-: Wrap tok@(Token { out = oT }) w2 <-
    activePair
  require $ w1 == w && w2 == w
  replace $ byNode $ Wrap tok { inp = oT, out = a } w2

popBind :: (View [Port] n, View NodeMS n) => WrapType -> Rule n
popBind w = do
  Wrap bind@(BindN { inp = i, arg = a, var = v, exec = True }) w1 :-: Wrap tok@(Token { out = oT }) w2 <-
    activePair
  require $ w1 == w && w2 == w
  replace $ do
    t <- byEdge
    byNode $ Wrap Applicator { inp = t, func = v, arg = oT } w
    byNode $ Wrap tok { inp = t, out = i } w2

execConjunctive :: (View [Port] n, View NodeMS n) => WrapType -> Rule n
execConjunctive w = do
  Wrap fork@(Fork { tpe = Conjunctive, inp = i, lhs = l, rhs = r, exec = False }) w1 :-: Wrap tok@(Token { out = oT }) w2 <-
    activePair
  require $ w1 == w && w2 == w
  replace $ do
    tl <- byEdge
    tr <- byEdge
    byNode $ Wrap fork { inp = oT, lhs = tl, rhs = tr, exec = True } w1
    byNode $ Wrap tok { inp = l, out = tl } w2
    byNode $ Wrap tok { inp = r, out = tr } w2

execDisjunctive :: (View [Port] n, View NodeMS n) => WrapType -> Rule n
execDisjunctive w = do
  Wrap fork@(Fork { tpe = Disjunctive, inp = i, lhs = l, rhs = r, exec = False }) w1 :-: Wrap tok@(Token { out = oT }) w2 <-
    activePair
  require $ w1 == w && w2 == w
  replace $ do -- we do not connect the right port in order to have ambiguity
    tl <- byEdge
    byNode $ Wrap fork { inp = oT, lhs = tl, rhs = tl, exec = True } w1
    byNode $ Wrap tok { inp = l, out = tl } w2
    byNode $ Wrap tok { inp = r, out = tl } w2

returnConjunctive :: (View [Port] n, View NodeMS n) => WrapType -> Rule n
returnConjunctive w = do
  Wrap (Fork { tpe = Conjunctive, inp = i1, lhs = l1, rhs = r1, exec = True }) w1 :-: Wrap tok1@(Token { inp = iT1, out = oT1 }) w2 <-
    activePair
  (Wrap (Token { inp = iT2, out = oT2 }) w3) <- nodeWith r1
  (Wrap (Fork { tpe = Conjunctive, inp = i2, lhs = l2, rhs = r2, exec = True }) w4) <-
    nodeWith iT2
  require $ w1 == w && w2 == w && w3 == w && w4 == w
  require $ oT1 /= oT2 && i1 == i2 && l1 == l2 && r1 == r2 -- same fork, different tokens

  replace $ do
    t <- byEdge
    byNode $ Wrap Applicator { inp = t, func = oT1, arg = oT2 } w
    byNode $ Wrap Token { inp = t, out = i1 } w2

returnDisjunctive :: (View [Port] n, View NodeMS n) => WrapType -> Rule n
returnDisjunctive w = do
  Wrap (Fork { tpe = Disjunctive, inp = i, exec = True }) w1 :-: Wrap tok@(Token { inp = iT, out = oT }) w2 <-
    activePair
  require $ w1 == w && w2 == w
  replace $ do
    byNode $ Wrap tok { inp = i, out = oT } w2
    byNode $ Wrap Eraser { inp = iT } w

initializeDataPartial :: (View [Port] n, View NodeMS n) => WrapType -> Rule n
initializeDataPartial w = do
  Wrap act@(Actor { name = nm, arity = n, args = as }) w1 :-: Wrap Applicator { func = f, inp = p, arg = a } w2 <-
    activePair
  require $ w1 == w && w2 == w
  guard $ n > 0
  replace $ byNode $ Wrap
    ActorC { inp = a, cur = p, name = nm, arity = n, args = as }
    w1

applyDataPartial :: (View [Port] n, View NodeMS n) => WrapType -> Rule n
applyDataPartial w = do
  Wrap act@(ActorC { name = nm, arity = n, args = as, cur = c }) w1 :-: Wrap Data { inp = p, dat = d } w2 <-
    activePair
  require $ w1 == w && w2 == w
  guard $ n > 0
  replace $ byNode $ Wrap
    Actor { inp = c, name = nm, arity = n - 1, args = d : as }
    w1

applyActor :: (View [Port] n, View NodeMS n) => WrapType -> Rule n
applyActor w = do
  Wrap (Token { out = p, inp = i }) w1 :-: Wrap (Actor { name = n, args = a, arity = 0 }) w2 <-
    activePair
  require $ w1 == w && w2 == w
  executeActor @NodeMS ImmediateNode n a p

prereduceRules
  :: forall m n
   . (GenericNode m, View NodeMS n, View [Port] n, View m n)
  => WrapType
  -> [Rule n]
prereduceRules w =
  [ duplicate @m w
  , annihilate @m w
  , eraser @m w
  , compileShare @m w
  , rotateBind w
  , popUnit w
  , popBind w
  -- , applyActor w
  -- , applyRecursor w
  , execConjunctive w
  , execDisjunctive w
  , returnConjunctive w
  , returnDisjunctive w
  , initializeDataPartial w
  , applyDataPartial w
  ]

rewrap :: (View [Port] n, View NodeMS n) => WrapType -> WrapType -> Rule n
rewrap wp wn = do
  Wrap n w <- GraphRewriting.Pattern.node
  require $ w == wp
  replace $ byNode $ Wrap n wn

applyRecursor :: (View [Port] n, View NodeMS n) => WrapType -> Rule n
applyRecursor w = do
  Wrap (Token { out = p, inp = i }) w1 :-: Wrap (Recursor { boxed = t }) w2 <-
    activePair
  require $ w1 == w && w2 == w

  (   executeRecursor t p
    >>> exhaustive (foldl1 (<|>) (prereduceRules @NodeMS RecursiveNode))
    )
    >>> exhaustive (rewrap RecursiveNode ImmediateNode)

  -- | disable pre-reduction
  -- (executeRecursor t p >>> exhaustive (compileShare @NodeMS RecursiveNode))
  --   >>> exhaustive (rewrap RecursiveNode ImmediateNode)
