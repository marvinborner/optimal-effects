-- Parts were originally written for lambdascope in `Rules.hs`
--   under the BSD-3-Clause License (as in src/GraphRewriting/LICENSE)
-- Copyright (c) 2010, Jan Rochel
-- Copyright (c) 2025, Marvin Borner

{-# LANGUAGE ScopedTypeVariables, TypeApplications, AllowAmbiguousTypes, FlexibleContexts #-}

module Language.Generic.Rules where

import           Control.Monad
import           Data.List                      ( delete
                                                , elemIndex
                                                , transpose
                                                )
import           Data.View
import           GraphRewriting.Graph.Read
import           GraphRewriting.Graph.Types
import           GraphRewriting.Graph.Write
import           GraphRewriting.Pattern
import           GraphRewriting.Pattern.InteractionNet
import           GraphRewriting.Rule
import           Language.Generic.Node

import           Debug.Trace

compileShare
  :: forall m n
   . (GenericNode m, View [Port] n, View m n)
  => WrapType
  -> Rule n
compileShare w = do
  mpx <- node
  require $ isMultiplexer @m mpx
  require $ isWrapType @m w mpx
  let (o : is) = inspect mpx :: [Port]
  case is of
    []  -> replace $ byNode $ gWrap (gEraser @m o) w
    [i] -> rewire [[o, i]]
    ins ->
      let (ins1, ins2) = splitAt (length ins `div` 2) ins
      in  replace $ do
            (o1, o2) <- (,) <$> byEdge <*> byEdge
            byNode $ gWrap (gDuplicator @m 0 o o1 o2) w
            byNode $ gWrap (gMultiplexer @m o1 ins1) w
            byNode $ gWrap (gMultiplexer @m o2 ins2) w

withoutIdx :: [a] -> Int -> [a]
withoutIdx xs i = let (ys, zs) = splitAt i xs in ys ++ tail zs

insertIdx :: Int -> a -> [a] -> [a]
insertIdx i x xs = let (l, r) = splitAt i xs in l ++ [x] ++ r

split :: Int -> Int -> [a] -> [[a]]
split i n [] = replicate n []
split i n xs = let (x, xs') = splitAt i xs in x : split i n xs'

transpose' n [] = replicate n []
transpose' n xs = transpose xs

commute
  :: forall m n
   . (GenericNode m, View [Port] n, View m n)
  => WrapType
  -> Rule n
commute w = do
  n1 :-: n2 <- activePair
  require $ n1 /= n2 -- TODO: replace by linear
  require $ isWrapType @m w n1 && isWrapType @m w n2
  let ports1 = inspect n1 :: [Port]
  let ports2 = inspect n2 :: [Port]
  let (pp1, pp1idx) =
        head [ (p, i) | (p, i) <- ports1 `zip` [0 ..], p == gpp @m n1 ]
  let (pp2, pp2idx) =
        head [ (p, i) | (p, i) <- ports2 `zip` [0 ..], p == gpp @m n2 ]
  let aux1 = pp1 `delete` inspect n1
  let aux2 = pp2 `delete` inspect n2
  let es1  = length aux1
  let es2  = length aux2
  replace $ do
    edges <- replicateM (es1 * es2) byEdge
    let edges1 = split es1 es2 edges
    let edges2 = transpose' es1 edges1
    mconcat
      [ byNode $ update (insertIdx pp1idx pp1 auxs) n1
      | (pp1, auxs) <- zip aux2 edges1
      ]
    mconcat
      [ byNode $ update (insertIdx pp2idx pp2 auxs) n2
      | (pp2, auxs) <- zip aux1 edges2
      ]

annihilate
  :: forall m n
   . (GenericNode m, View [Port] n, View m n)
  => WrapType
  -> Rule n
annihilate w = do
  n1 :-: n2 <- activePair
  require $ isWrapType @m w n1 && isWrapType @m w n2
  require $ n1 == n2
  attached <- liftReader . flip adverseNodes (gpp @m n1) =<< previous
  require $ length attached == 1 -- in order to prevent anns over multiplexed (e.g. disj fork)
  let aux1 = gpp @m n1 `delete` inspect n1
  let aux2 = gpp @m n2 `delete` inspect n2
  rewire $ [ [a1, a2] | (a1, a2) <- aux1 `zip` aux2 ]

eraser
  :: forall m n
   . (GenericNode m, View [Port] n, View m n)
  => WrapType
  -> Rule n
eraser w = do
  rewrite <- commute @m w
  era     <- liftReader . inspectNode =<< previous
  require $ isEraser @m era
  -- require $ isWrapType @m w era
  return rewrite

duplicate
  :: forall m n
   . (GenericNode m, View [Port] n, View m n)
  => WrapType
  -> Rule n
duplicate w = do
  rewrite <- commute @m w
  dup     <- liftReader . inspectNode =<< previous
  other   <- (liftReader . inspectNode) . head . tail =<< history
  require $ isDuplicator @m dup
  -- require $ isWrapType @m w dup
  require $ not $ isEraser @m other -- do not duplicate erasers
  -- require $ isWrapType @m w other
  return rewrite
