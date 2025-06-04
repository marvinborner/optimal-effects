-- Parts were originally written for lambdascope in `Rules.hs`
--   under the BSD-3-Clause License (as in src/GraphRewriting/LICENSE)
-- Copyright (c) 2010, Jan Rochel
-- Copyright (c) 2024, Marvin Borner

{-# LANGUAGE FlexibleContexts, ScopedTypeVariables, FlexibleInstances #-}
module Language.FreeMonad.Rules where

import           Control.Applicative            ( optional )
import           Control.Monad
import           Data.FreeMonad
import           Data.List                      ( delete
                                                , elemIndex
                                                , transpose
                                                )
import           Data.Maybe                     ( fromJust )
import qualified Data.Text                     as T
import           GraphRewriting.Graph.Read
import           GraphRewriting.Graph.Write
import           GraphRewriting.Layout.Wrapper as Layout
import           GraphRewriting.Pattern
import           GraphRewriting.Pattern.InteractionNet
import           GraphRewriting.Rule

compileShare :: (View [Port] n, View NodeLS n) => Rule n
compileShare = do
  Multiplexer { out = o, ins = is } <- node
  case is of
    []  -> replace $ byNode Eraser { inp = o }
    [i] -> rewire [[o, i]]
    ins ->
      let (ins1, ins2) = splitAt (length ins `div` 2) ins
      in  replace $ do
            (o1, o2) <- (,) <$> byEdge <*> byEdge
            byNode $ Duplicator { level = 0, inp = o, out1 = o1, out2 = o2 }
            byNode $ Multiplexer { out = o1, ins = ins1 }
            byNode $ Multiplexer { out = o2, ins = ins2 }

withoutIdx :: [a] -> Int -> [a]
withoutIdx xs i = let (ys, zs) = splitAt i xs in ys ++ tail zs

insertIdx :: Int -> a -> [a] -> [a]
insertIdx i x xs = let (l, r) = splitAt i xs in l ++ [x] ++ r

split :: Int -> Int -> [a] -> [[a]]
split i n [] = replicate n []
split i n xs = let (x, xs') = splitAt i xs in x : split i n xs'

transpose' n [] = replicate n []
transpose' n xs = transpose xs

commute :: (View [Port] n, View NodeLS n) => Rule n
commute = do
  n1 :-: n2 <- activePair
  require (n1 /= n2) -- TODO: replace by linear
  let ports1 = inspect n1 :: [Port]
  let ports2 = inspect n2 :: [Port]
  let (pp1, pp1idx) =
        head [ (p, i) | (p, i) <- ports1 `zip` [0 ..], p == pp n1 ]
  let (pp2, pp2idx) =
        head [ (p, i) | (p, i) <- ports2 `zip` [0 ..], p == pp n2 ]
  let aux1 = pp1 `delete` inspect n1
  let aux2 = pp2 `delete` inspect n2
  let es1  = length aux1
  let es2  = length aux2
  replace $ do
    edges <- replicateM (es1 * es2) byEdge
    let edges1 = split es1 es2 edges
    let edges2 = transpose' es1 edges1
    mconcat
      [ byNode $ updateLevel n2 $ update (insertIdx pp1idx pp1 auxs) n1
      | (pp1, auxs) <- zip aux2 edges1
      ]
    mconcat
      [ byNode $ updateLevel n1 $ update (insertIdx pp2idx pp2 auxs) n2
      | (pp2, auxs) <- zip aux1 edges2
      ]
 where
  updateLevel you me = case me of
    Duplicator{} -> maybeLevelUp
    _            -> me
   where
    maybeLevelUp = case you of
      Abstractor{} -> me { level = level me + 1 }
      _            -> me

annihilate :: (View [Port] n, View NodeLS n) => Rule n
annihilate = do
  n1 :-: n2 <- activePair
  require (n1 == n2)
  let aux1 = pp n1 `delete` inspect n1
  let aux2 = pp n2 `delete` inspect n2
  rewire $ [ [a1, a2] | (a1, a2) <- aux1 `zip` aux2 ]

eliminateDuplicator :: (View [Port] n, View NodeLS n) => Rule n
eliminateDuplicator = do
  Eraser { inp = iE }                           <- node
  Duplicator { inp = iD, out1 = o1, out2 = o2 } <- neighbour =<< previous
  require (iE == o1 || iE == o2)
  if iE == o1 then rewire [[iD, o2]] else rewire [[iD, o1]]

eraser :: (View [Port] n, View NodeLS n) => Rule n
eraser = do
  rewrite  <- commute
  Eraser{} <- liftReader . inspectNode =<< previous
  return rewrite

duplicate :: (View [Port] n, View NodeLS n) => Rule n
duplicate = do
  rewrite      <- commute
  Duplicator{} <- liftReader . inspectNode =<< previous
  return rewrite

bindExecBind :: (View [Port] n, View NodeLS n) => Rule n
bindExecBind = do
  a@(BindN { exec = True }) :-: b@(BindN { exec = False }) <- activePair
  replace $ do
    byNode a
    byNode $ b { exec = True }

-- TODO: verify
bindExecUnit :: (View [Port] n, View NodeLS n) => Rule n
bindExecUnit = do
  BindN { inp = p, next = n, var = v, exec = True } :-: UnitN { out = o, exec = False } <-
    activePair
  replace $ do
    byWire p n
    byWire o v

initializeDataPartial :: (View [Port] n, View NodeLS n) => Rule n
initializeDataPartial = do
  eff@(Actor{}) :-: Applicator { inp = p, func = f, arg = a } <- activePair
  replace $ byNode $ eff { inp = a, cur = p }

applyDataPartial :: (View [Port] n, View NodeLS n) => Rule n
applyDataPartial = do
  eff@(Actor { args = as, cur = c }) :-: Data { inp = p, dat = d } <- activePair
  replace $ byNode $ eff { inp = c, args = d : as }

initializeUnitExec :: (View [Port] n, View NodeLS n) => Rule n
initializeUnitExec = do
  bind@(UnitN { exec = False }) :-: init@(Initiator{}) <- activePair
  replace $ do
    byNode $ bind { exec = True }
    byNode init

initializeBindExec :: (View [Port] n, View NodeLS n) => Rule n
initializeBindExec = do
  bind@(BindN { exec = False }) :-: init@(Initiator{}) <- activePair
  replace $ do
    byNode $ bind { exec = True }
    byNode init

applyUnitExec' :: Rule (Layout.Wrapper NodeLS)
applyUnitExec' = do
  UnitN { inp = p, exec = True } :-: Actor { function = f, args = a } <-
    activePair
  replace $ f a p

applyBindExec' :: Rule (Layout.Wrapper NodeLS)
applyBindExec' = do
  BindN { inp = i, next = n, var = p, exec = True } :-: Actor { function = f, args = a } <-
    activePair
  replace $ do
    byWire i n
    f a p

class EffectApplicable n where
  applyUnitEffect :: Rule n
  applyBindEffect :: Rule n

instance EffectApplicable (Layout.Wrapper NodeLS) where
  applyUnitEffect = applyUnitExec'
  applyBindEffect = applyBindExec'

applyUnitExec :: (EffectApplicable n, View [Port] n, View NodeLS n) => Rule n
applyUnitExec = do
  eff <- applyUnitEffect
  -- optional $ exhaustive compileShare -- TODO!
  return eff

applyBindExec :: (EffectApplicable n, View [Port] n, View NodeLS n) => Rule n
applyBindExec = do
  eff <- applyBindEffect
  -- optional $ exhaustive compileShare -- TODO!
  return eff
