-- Parts were originally written for lambdascope in `Reducer.hs`
--   under the BSD-3-Clause License (as in src/GraphRewriting/LICENSE)
-- Copyright (c) 2010, Jan Rochel
-- Copyright (c) 2024, Marvin Borner

{-# LANGUAGE ScopedTypeVariables, TypeApplications, AllowAmbiguousTypes, FlexibleContexts, FlexibleInstances #-}

module Language.Monad.Reducer
  ( bench
  , visualize
  , count
  ) where

import           Data.Foldable                  ( toList )
import           Data.Monad
import           Data.Traversable               ( mapAccumL )
import           Data.Vector.V2                 ( Vector2(..) )
import           GraphRewriting.GL.Render
import           GraphRewriting.GL.UI          as UI
import           GraphRewriting.Graph
import           GraphRewriting.Graph.Read
import           GraphRewriting.Graph.Write.Unsafe
                                               as Unsafe
import           GraphRewriting.Layout.Coulomb
import           GraphRewriting.Layout.Gravitation
import           GraphRewriting.Layout.SpringEmbedder
import           GraphRewriting.Layout.Wrapper as Layout
import           GraphRewriting.Pattern
import           GraphRewriting.Pattern.InteractionNet
import           GraphRewriting.Rule
import           GraphRewriting.Strategies.Control
                                               as Control
import           GraphRewriting.Strategies.LeftmostOutermost
import           Language.Generic.Node
import           Language.Generic.Rules
import           Language.Monad.GL
import           Language.Monad.Rules
import           System.Random
import           System.Random.Shuffle

-- from LambdaScope/GraphRewriting
instance Render n => Render (Layout.Wrapper n) where
  render = render . wrappee
instance PortSpec n => PortSpec (Control.Wrapper n) where
  portSpec = portSpec . wrapped
instance LeftmostOutermost n => LeftmostOutermost (Layout.Wrapper n) where
  lmoPort = lmoPort . wrappee

-- from LambdaScope/GraphRewriting
layoutStep
  :: (PortSpec n, View Position n, View Rotation n, View [Port] n)
  => Node
  -> Rewrite n ()
layoutStep n = do
  (cgf, cf, sf, rot) <- readOnly $ do
    cgf <- centralGravitation n
    cf  <- coulombForce n
    sf  <- springForce 1.5 n
    rot <- angularMomentum n
    return (cgf, cf, sf, rot)
  Unsafe.adjustNode n
    $ Position
    . sf (\x -> 0.2 * (min 10 (x * 0.9)))
    . cgf (\x -> 0.2 * (min 10 (x * 0.01)))
    . cf (\x -> 0.2 * (min 10 (100 / (x ^ 2 + 0.1))))
    . position
  Unsafe.adjustNode n $ rot (* 0.9)

-- | Visualize reduction to normal form
-- from LambdaScope/GraphRewriting
visualize :: Bool -> Bool -> Bool -> Graph NodeMS -> IO ()
visualize _ _ _ term = do
  (_, _) <- UI.initialise
  let hypergraph = execGraph (apply $ exhaustive $ compileShare @NodeMS) term
  let layoutGraph = Layout.wrapGraph hypergraph
  UI.run 50 id layoutStep layoutGraph $ ruleTree @NodeMS

-- from LambdaScope/GraphRewriting
incIndex :: Int -> [Int] -> [Int]
incIndex 0 (i : is) = i + 1 : is
incIndex 0 []       = [1]
incIndex n (i : is) = i : incIndex (n - 1) is
incIndex n []       = 0 : incIndex (n - 1) []

count :: Bool -> Bool -> Bool -> Graph NodeMS -> IO ()
count _ _ _ term = do
  let hypergraph = execGraph (apply $ exhaustive $ compileShare @NodeMS) term
  let layoutGraph = Layout.wrapGraph hypergraph
  UI.iterations layoutStep layoutGraph $ ruleTree @NodeMS

-- from LambdaScope/GraphRewriting
bench :: Bool -> Bool -> Bool -> Graph NodeMS -> IO ()
bench _ random parallel term = do
  let hypergraph = execGraph (apply $ exhaustive $ compileShare @NodeMS) term
  rng <- newStdGen
  let func | random    = benchmarkRandom rng
           | otherwise = benchmark
  let tree = ruleTree @NodeMS
  let rules | parallel  = func (exhaustive <$> toList tree)
            | otherwise = func $ toList tree
  let indices    = evalGraph rules (Control.wrapGraph hypergraph)
  let indexTable = foldl (flip incIndex) [] indices
  let (_, numTree) =
        mapAccumL (\(i : is) _ -> (is, i)) (indexTable ++ repeat 0) tree
  putStrLn $ showLabelledTree 2 0 (+) numTree

ruleTree
  :: forall m n
   . (GenericNode m, View NodeMS n, View [Port] n, View m n)
  => LabelledTree (Rule n)
ruleTree = Branch
  "All"
  [ Leaf "Duplicate" $ duplicate @m
  , Leaf "Annihilate" $ annihilate @m
  , Leaf "Erase" $ eraser @m
  , Branch
    "Token"
    [ Leaf "Rotate Bind" rotateBind
    , Leaf "Pop Unit"    popUnit
    , Leaf "Pop Bind"    popBind
    ]
  , Branch
    "Effectful"
    [ Leaf "Apply Actor"                    applyActor
    , Leaf "Apply Recursor"                 applyRecursor
    , Leaf "Execute Conjunctive Fork"       execConjunctive
    , Leaf "Execute Disjunctive Fork"       execDisjunctive
    , Leaf "Return Conjunctive Fork"        returnConjunctive
    , Leaf "Return Disjunctive Fork"        returnDisjunctive
    , Leaf "Initialize Partial Application" initializeDataPartial
    , Leaf "Apply Partially"                applyDataPartial
    ]
  ]
