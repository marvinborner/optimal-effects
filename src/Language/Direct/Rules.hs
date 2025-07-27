-- Copyright (c) 2025, Marvin Borner

{-# LANGUAGE TypeApplications, FlexibleContexts, ScopedTypeVariables, FlexibleInstances, AllowAmbiguousTypes #-}
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

reflectToken :: (View [Port] n, View NodeDS n) => WrapType -> Rule n
reflectToken w = do
  Wrap reflector w1 :-: Wrap tok@(Token { inp = iT, out = oT }) w2 <- activePair
  require $ w1 == w && w2 == w
  guard $ reflectsToken reflector
  replace $ do
    byNode $ Wrap reflector { inp = iT } w1
    byNode $ Wrap tok { inp = oT, out = iT } w2

redirectToken :: (View [Port] n, View NodeDS n) => WrapType -> Rule n
redirectToken w = do
  Wrap red@(Redirector { portA = a, portB = b, portC = c, direction = r }) w1 :-: Wrap tok@(Token { inp = iT, out = oT }) w2 <-
    activePair
  require $ w1 == w && w2 == w
  attached <- liftReader . flip adverseNodes iT =<< previous
  require $ length attached == 1 -- in order to prevent reds over multiplexed (e.g. disj fork)
  case r of
    BottomRight -> replace $ do
      v <- byEdge -- tok-bl edge
      byNode $ Wrap red { portB = oT, portC = v, direction = BottomLeft } w1
      byNode $ Wrap tok { inp = c, out = v } w2
    BottomLeft -> replace $ do
      v <- byEdge
      byNode $ Wrap red { portA = v, portC = oT, direction = Top } w1
      byNode $ Wrap tok { inp = a, out = v } w2
    Top -> replace $ do -- passthrough
      v <- byEdge
      byNode $ Wrap red { portA = oT, portB = v, direction = Top } w1
      byNode $ Wrap tok { inp = v, out = b } w2

execConjunctive :: (View [Port] n, View NodeDS n) => WrapType -> Rule n
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

execDisjunctive :: (View [Port] n, View NodeDS n) => WrapType -> Rule n
execDisjunctive w = do
  Wrap fork@(Fork { tpe = Disjunctive, inp = i, lhs = l, rhs = r, exec = False }) w1 :-: Wrap tok@(Token { out = oT }) w2 <-
    activePair
  require $ w1 == w && w2 == w
  replace $ do -- we do not connect the right port in order to have ambiguity
    tl <- byEdge
    byNode $ Wrap fork { inp = oT, lhs = tl, rhs = tl, exec = True } w1
    byNode $ Wrap tok { inp = l, out = tl } w2
    byNode $ Wrap tok { inp = r, out = tl } w2

returnConjunctive :: (View [Port] n, View NodeDS n) => WrapType -> Rule n
returnConjunctive w = do
  Wrap (Fork { tpe = Conjunctive, inp = i1, lhs = l1, rhs = r1, exec = True }) w1 :-: Wrap tok1@(Token { inp = iT1, out = oT1 }) w2 <-
    activePair
  require $ w1 == w && w2 == w
  (Token { inp = iT2, out = oT2 }) <- nodeWith r1
  (Fork { tpe = Conjunctive, inp = i2, lhs = l2, rhs = r2, exec = True }) <-
    nodeWith iT2
  require $ oT1 /= oT2 && i1 == i2 && l1 == l2 && r1 == r2 -- same fork, different tokens

  replace $ do
    t <- byEdge
    byNode $ Wrap
      Redirector { direction = Top, portA = oT1, portB = t, portC = oT2 }
      w
    byNode $ Wrap Token { inp = t, out = i1 } w2

returnDisjunctive :: (View [Port] n, View NodeDS n) => WrapType -> Rule n
returnDisjunctive w = do
  Wrap (Fork { tpe = Disjunctive, inp = i, exec = True }) w1 :-: Wrap tok@(Token { inp = iT, out = oT }) w2 <-
    activePair
  require $ w1 == w && w2 == w
  replace $ do
    byNode $ Wrap tok { inp = i, out = oT } w2
    byNode $ Wrap Eraser { inp = iT } w

-- this is basically almost !reflectsToken
hasActionPotential :: NodeDS -> Bool
hasActionPotential (Redirector { direction = BottomRight }) = True
hasActionPotential (Actor { arity = 0 }) = True
hasActionPotential (Fork{}             ) = True
hasActionPotential (Recursor{}         ) = True
hasActionPotential (Data{}             ) = False -- Must be False or possible loops with T-App
hasActionPotential (Wrap n _           ) = hasActionPotential n
hasActionPotential _                     = False

isControlling :: NodeDS -> Bool
isControlling (Token{}     ) = True
isControlling (Duplicator{}) = True
isControlling (Eraser{}    ) = True
isControlling (Wrap n _    ) = isControlling n
isControlling _              = False

inferLeftEffectful :: (View [Port] n, View NodeDS n) => WrapType -> Rule n
inferLeftEffectful w = do -- ==> there is an action somewhere inside b
  Wrap a@(Redirector { direction = BottomLeft }) w1 :-: Wrap b w2 <- activePair
  require $ w1 == w && w2 == w
  guard $ hasActionPotential b
  guard $ not $ isControlling b
  replace $ do
    byNode $ Wrap a { direction = BottomRight } w
    byNode $ Wrap b w2

inferTopEffectful :: (View [Port] n, View NodeDS n) => WrapType -> Rule n
inferTopEffectful w = do -- ==> there is an action somewhere inside b
  Wrap a@(Redirector { direction = Top }) w1 :-: Wrap b w2 <- activePair
  require $ w1 == w && w2 == w
  guard $ hasActionPotential b
  guard $ not $ isControlling b
  replace $ do
    byNode $ Wrap a { direction = BottomRight } w1
    byNode $ Wrap b w2

inferLeftUneffectful :: (View [Port] n, View NodeDS n) => WrapType -> Rule n
inferLeftUneffectful w = do -- ==> there is no immediate action potential in b
  Wrap a@(Redirector { direction = BottomLeft }) w1 :-: Wrap b w2 <- activePair
  require $ w1 == w && w2 == w
  guard $ not $ hasActionPotential b
  guard $ not $ isControlling b
  replace $ do
    byNode $ Wrap a { direction = Top } w1
    byNode $ Wrap b w2

initializeDataPartial :: (View [Port] n, View NodeDS n) => WrapType -> Rule n
initializeDataPartial w = do
  Wrap act@(Actor { name = nm, arity = n, args = as }) w1 :-: Wrap (Redirector { portA = f, portB = p, portC = a, direction = Top }) w2 <-
    activePair
  require $ w1 == w && w2 == w
  guard $ n > 0
  replace $ byNode $ Wrap
    ActorC { inp = a, cur = p, name = nm, arity = n, args = as }
    w1

applyDataPartial :: (View [Port] n, View NodeDS n) => WrapType -> Rule n
applyDataPartial w = do
  Wrap act@(ActorC { name = nm, arity = n, args = as, cur = c }) w1 :-: Wrap (Data { inp = p, dat = d }) w2 <-
    activePair
  require $ w1 == w && w2 == w
  guard $ n > 0
  replace $ byNode $ Wrap
    Actor { inp = c, name = nm, arity = n - 1, args = d : as }
    w1

applyActor :: (View [Port] n, View NodeDS n) => WrapType -> Rule n
applyActor w = do
  Wrap (Token { out = p, inp = i }) w1 :-: Wrap (Actor { name = n, args = a, arity = 0 }) w2 <-
    activePair
  require $ w1 == w && w2 == w
  executeActor @NodeDS w n a p

-- | Rules applied to prereduce a net (e.g. in recursion)
-- | must never have side effects!
prereduceRules
  :: forall m n
   . (GenericNode m, View NodeDS n, View [Port] n, View m n)
  => WrapType
  -> [Rule n]
prereduceRules w =
  [ duplicate @m w
  , annihilate @m w
  , eraser @m w
  , compileShare @m w
  , redirectToken w
  , reflectToken w
  , inferLeftEffectful w
  , inferTopEffectful w
  , inferLeftUneffectful w
  -- , applyActor w
  -- , applyRecursor w dir
  , execConjunctive w
  , execDisjunctive w
  , returnConjunctive w
  , returnDisjunctive w
  , initializeDataPartial w
  , applyDataPartial w
  ]

rewrap :: (View [Port] n, View NodeDS n) => WrapType -> WrapType -> Rule n
rewrap wp wn = do
  Wrap n w <- GraphRewriting.Pattern.node
  require $ w == wp
  replace $ byNode $ Wrap n wn

applyRecursor :: (View [Port] n, View NodeDS n) => WrapType -> AppDir -> Rule n
applyRecursor w dir = do
  Wrap (Token { out = p, inp = i }) w1 :-: Wrap (Recursor { boxed = t }) w2 <-
    activePair
  require $ w1 == w && w2 == w

  (   executeRecursor dir t p
    >>> exhaustive (foldl1 (<|>) (prereduceRules @NodeDS RecursiveNode))
    )
    >>> exhaustive (rewrap RecursiveNode ImmediateNode)
