-- Parts were originally written for lambdascope in `Rules.hs`
--   under the BSD-3-Clause License (as in src/GraphRewriting/LICENSE)
-- Copyright (c) 2010, Jan Rochel
-- Copyright (c) 2024, Marvin Borner

{-# LANGUAGE FlexibleContexts, ScopedTypeVariables #-}
module Language.TokenPassing.Rules where

import           Control.Monad
import           Data.List                      ( delete
                                                , elemIndex
                                                , transpose
                                                )
import           Data.Maybe                     ( fromJust )
import           Data.TokenPassing
import           GraphRewriting.Graph.Read
import           GraphRewriting.Graph.Write
import           GraphRewriting.Pattern
import           GraphRewriting.Pattern.InteractionNet
import           GraphRewriting.Rule

-- TODO: REMOVE THIS
import           System.IO.Unsafe               ( unsafePerformIO )
import           System.Process                 ( readProcess )

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

reflectsToken Abstractor{} = True
reflectsToken Effectful{}  = True
reflectsToken _            = False

reflectToken :: (View [Port] n, View NodeLS n) => Rule n
reflectToken = do
  reflector :-: tok@(Token { inp = iT, out = oT }) <- activePair
  guard $ reflectsToken reflector
  replace $ do
    byNode $ reflector { inp = iT }
    byNode $ tok { inp = oT, out = iT }

redirectToken :: (View [Port] n, View NodeLS n) => Rule n
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
      byNode $ tok { inp = v, out = b }
      byNode $ red { portB = v, portC = oT, direction = Top }

passthroughRight :: (View [Port] n, View NodeLS n) => Rule n
passthroughRight = do
  red@(Redirector { direction = BottomRight }) :-: n <- activePair
  replace $ do
    byNode $ red { direction = Top } -- everything else stays!
    byNode n

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

applyEffectful :: (View [Port] n, View NodeLS n) => Rule n
applyEffectful = do
  Redirector { portB = iB, portC = aA, direction = Top } :-: Effectful { inp = iE, name = nE } <-
    activePair
  case runEffect nE aA of
    Nothing -> mempty
    Just n' -> replace $ byNode $ Effectful { inp = iB, name = n' }

runEffect "readInt" _ = Just $ unsafePerformIO $ do
  result <- readProcess
    "zenity"
    ["--entry", "--title=Input", "--text=Please enter some input:"]
    ""
  return result
runEffect "writeInt" n = Just $ unsafePerformIO $ do
  result <- readProcess "zenity"
                        ["--info", "--title=Output", "--text=" <> show n]
                        ""
  return result
runEffect n _ = error n
