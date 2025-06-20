-- Copyright (c) 2025, Marvin Borner

{-# LANGUAGE TypeApplications, FlexibleContexts, ScopedTypeVariables, FlexibleInstances #-}
module Language.Monad.Rules where

import           Control.Applicative            ( optional )
import           Control.Monad
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
import           Language.Lambda.Transformer.Monad
                                                ( executeRecursor )

rotateBind :: (View [Port] n, View NodeMS n) => Rule n
rotateBind = do
  bind@(BindN { arg = a, exec = False }) :-: tok@(Token { out = oT }) <-
    activePair
  replace $ do
    t <- byEdge
    byNode $ bind { inp = oT, arg = t, exec = True }
    byNode $ tok { inp = a, out = t }

popUnit :: (View [Port] n, View NodeMS n) => Rule n
popUnit = do
  unit@(UnitN { out = a }) :-: tok@(Token { out = oT }) <- activePair
  replace $ byNode $ tok { inp = oT, out = a }

popBind :: (View [Port] n, View NodeMS n) => Rule n
popBind = do
  bind@(BindN { inp = i, arg = a, var = v, exec = True }) :-: tok@(Token { out = oT }) <-
    activePair
  replace $ do
    t <- byEdge
    byNode $ Applicator { inp = t, func = v, arg = oT }
    byNode $ tok { inp = t, out = i }

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
  -- exhaustive compileShare -- TODO!

applyRecursor :: (View [Port] n, View NodeMS n) => Rule n
applyRecursor = do
  (Token { out = p, inp = i }) :-: (Recursor { boxed = t }) <- activePair
  executeRecursor t p
