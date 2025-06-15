-- Parts were originally written for lambdascope in `Rules.hs`
--   under the BSD-3-Clause License (as in src/GraphRewriting/LICENSE)
-- Copyright (c) 2010, Jan Rochel
-- Copyright (c) 2025, Marvin Borner

{-# LANGUAGE TypeApplications, FlexibleContexts, ScopedTypeVariables, FlexibleInstances #-}
module Language.TokenPassing.Rules where

import           Control.Applicative            ( optional )
import           Control.Monad
import           Data.Effects
import           Data.List                      ( delete
                                                , elemIndex
                                                , transpose
                                                )
import           Data.Maybe                     ( fromJust )
import qualified Data.Text                     as T
import           Data.TokenPassing
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
import           Language.Generic.Rules
import           Language.Lambda.Transformer.TokenPassing
                                                ( executeRecursor )

reflectsToken Abstractor{}        = True
reflectsToken Actor { arity = a } = a > 0
reflectsToken Data{}              = True
reflectsToken _                   = False

reflectToken :: (View [Port] n, View NodeTP n) => Rule n
reflectToken = do
  reflector :-: tok@(Token { inp = iT, out = oT }) <- activePair
  guard $ reflectsToken reflector
  replace $ do
    byNode $ reflector { inp = iT }
    byNode $ tok { inp = oT, out = iT }

redirectToken :: (View [Port] n, View NodeTP n) => Rule n
redirectToken = do
  red@(Redirector { portA = a, portB = b, portC = c, direction = r }) :-: tok@(Token { inp = iT, out = oT }) <-
    activePair
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

-- TODO: should this basically almost be !reflectsToken ??
hasActionPotential :: NodeTP -> Bool
hasActionPotential (Redirector { direction = BottomRight }) = True
-- hasActionPotential (Actor{}) = True
hasActionPotential (Actor { arity = 0 }) = True
hasActionPotential (Data{}             ) = False -- Must be False or possible loops with T-App
hasActionPotential _                     = False

backpropagateActor :: (View [Port] n, View NodeTP n) => Rule n
backpropagateActor = do -- ==> there is an action somewhere inside b
  a@(Redirector { direction = BottomLeft }) :-: b <- activePair
  guard $ hasActionPotential b
  replace $ do
    byNode $ a { direction = BottomRight }
    byNode b

-- TODO: is this fully correct?
backpropagateActor2 :: (View [Port] n, View NodeTP n) => Rule n
backpropagateActor2 = do -- ==> there is an action somewhere inside b
  a@(Redirector { direction = Top }) :-: b <- activePair
  guard $ hasActionPotential b
  replace $ do
    byNode $ a { direction = BottomRight }
    byNode b

backpropagateUneffectful :: (View [Port] n, View NodeTP n) => Rule n
backpropagateUneffectful = do -- ==> there is no immediate action potential in b
  a@(Redirector { direction = BottomLeft }) :-: b <- activePair
  guard $ not $ hasActionPotential b
  replace $ do
    byNode $ a { direction = Top }
    byNode b

-- is this fully correct?
-- THIS is certisfiably wrong, can cause infinite loops
-- TODO: is something like this still required somehow?
-- backpropagateUneffectful2 :: (View [Port] n, View NodeTP n) => Rule n
-- backpropagateUneffectful2 = do -- ==> there is no immediate action potential in b
--   a@(Redirector { direction = Top }) :-: b <- activePair
--   guard $ not $ hasActionPotential b
--   replace $ do
--     byNode $ a { direction = BottomLeft }
--     byNode b

-- TODO: WHAT IS THIS RULE?? does it have any use?
-- passthroughRight :: (View [Port] n, View NodeTP n) => Rule n
-- passthroughRight = do
--   red@(Redirector { direction = BottomRight }) :-: n <- activePair
--   guard $ not $ isToken n
--   replace $ do
--     byNode $ red { direction = Top } -- everything else stays!
--     byNode n

initializeDataPartial :: (View [Port] n, View NodeTP n) => Rule n
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

applyDataPartial :: (View [Port] n, View NodeTP n) => Rule n
applyDataPartial = do
  act@(ActorC { name = nm, arity = n, args = as, cur = c }) :-: Data { inp = p, dat = d } <-
    activePair
  guard $ n > 0
  replace $ byNode $ Actor { inp = c, name = nm, arity = n - 1, args = d : as }

applyActor :: (View [Port] n, View NodeTP n) => Rule n
applyActor = do
  (Token { out = p, inp = i }) :-: (Actor { name = n, args = a, arity = 0 }) <-
    activePair
  executeActor @NodeTP n a p
  -- exhaustive $ compileShare @NodeTP -- TODO!

applyRecursor :: (View [Port] n, View NodeTP n) => Rule n
applyRecursor = do
  (Token { out = p, inp = i }) :-: (Recursor { boxed = t }) <- activePair
  executeRecursor t p
