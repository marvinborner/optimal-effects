-- Parts were originally written for lambdascope in `Reducer.hs`
--   under the BSD-3-Clause License (as in src/GraphRewriting/LICENSE)
-- Copyright (c) 2010, Jan Rochel
-- Copyright (c) 2024, Marvin Borner

{-# LANGUAGE ScopedTypeVariables, TypeApplications, AllowAmbiguousTypes, FlexibleContexts, FlexibleInstances #-}

module Language.Monad.Reducer
  ( bench
  , visualize
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
import           GraphRewriting.Rule
import           GraphRewriting.Strategies.Control
                                               as Control
import           GraphRewriting.Strategies.LeftmostOutermost
import           Language.Generic.Node
import           Language.Generic.Rules
import           Language.Monad.GL
import           Language.Monad.Rules

-- TODO
import           GraphRewriting.Pattern.InteractionNet

instance Render n => Render (Layout.Wrapper n) where
  render = render . wrappee
instance PortSpec n => PortSpec (Control.Wrapper n) where
  portSpec = portSpec . wrapped
instance LeftmostOutermost n => LeftmostOutermost (Layout.Wrapper n) where
  lmoPort = lmoPort . wrappee

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
    . sf (\x -> min 10 (x * 0.9))
    . cgf (\x -> min 10 (x * 0.01))
    . cf (\x -> min 10 (100 / (x ^ 2 + 0.1)))
    . position
  Unsafe.adjustNode n $ rot (* 0.9)

-- | Visualize reduction to normal form
-- TODO: only app should use IO
visualize :: Graph NodeMS -> IO ()
visualize term = do
  (_, _) <- UI.initialise
  let hypergraph = execGraph (apply $ exhaustive $ compileShare @NodeMS) term
  let layoutGraph = Layout.wrapGraph hypergraph
  UI.run 50 id layoutStep layoutGraph (ruleTree @NodeMS)

-- from LambdaScope/GraphRewriting
incIndex :: Int -> [Int] -> [Int]
incIndex 0 (i : is) = i + 1 : is
incIndex 0 []       = [1]
incIndex n (i : is) = i : incIndex (n - 1) is
incIndex n []       = 0 : incIndex (n - 1) []

-- from LambdaScope/GraphRewriting
bench :: Graph NodeMS -> IO ()
bench term = do
  (_, _) <- UI.initialise
  let hypergraph = execGraph (apply $ exhaustive $ compileShare @NodeMS) term
  let indices = evalGraph (benchmark $ toList (ruleTree @NodeMS))
                          (Control.wrapGraph hypergraph)
  let indexTable = foldl (flip incIndex) [] indices
  let (_, numTree) = mapAccumL (\(i : is) _ -> (is, i))
                               (indexTable ++ repeat 0)
                               (ruleTree @NodeMS @(Control.Wrapper NodeMS))
  putStrLn $ showLabelledTree 2 0 (+) numTree

ruleTree
  :: forall m n
   . (GenericNode m, View NodeMS n, View [Port] n, View m n)
  => LabelledTree (Rule n)
ruleTree = Branch
  "All"
  [ Leaf "Duplicate" $ duplicate @m
  , Leaf "Eliminate" $ eliminateDuplicator @m
  , Leaf "Annihilate" $ annihilate @m
  , Leaf "Erase" $ eraser @m
  , Leaf "Multiplexer" $ compileShare @m
  , Branch
    "Token"
    [ Leaf "Rotate Bind" rotateBind
    , Leaf "Pop Unit"    popUnit
    , Leaf "Pop Bind"    popBind
    ]
  , Branch
    "Effective"
    [ Leaf "Apply Actor"                    applyActor
    , Leaf "Apply Recursor"                 applyRecursor
    , Leaf "Initialize Partial Application" initializeDataPartial
    , Leaf "Apply Partially"                applyDataPartial
    ]
  ]
