-- Parts were originally written for lambdascope in `Reducer.hs`
--   under the BSD-3-Clause License (as in src/GraphRewriting/LICENSE)
-- Copyright (c) 2010, Jan Rochel
-- Copyright (c) 2024, Marvin Borner

{-# LANGUAGE TypeApplications, FlexibleContexts, FlexibleInstances #-}

module Language.TokenPassing.Reducer
  ( visualize
  -- , bench
  ) where

import           Data.Foldable                  ( toList )
import           Data.TokenPassing
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
import           Language.TokenPassing.GL
import           Language.TokenPassing.Rules

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
    $ Position -- TODO: find good values
    . sf (\x -> min 1000 (x * 0.9))
    . cgf (\x -> min 10000 (x * 0.01))
    . cf (\x -> min 10000 (100 / (x ^ 2 + 0.1)))
    . position
  Unsafe.adjustNode n $ rot (* 0.9)

-- layoutTree
--   :: ( EffectApplicable n
--      , View [Port] n
--      , View (NodeTP Layout.Wrapper) n
--      , View (Layout.Wrapper (NodeTP Layout.Wrapper)) n
--      )
--   => LabelledTree (Rule n)
layoutTree = ruleTree

-- | Visualize reduction to normal form
-- TODO: only app should use IO
visualize :: Graph (NodeTP Layout.Wrapper) -> IO ()
visualize term = do
  (_, _) <- UI.initialise
  let hypergraph = execGraph
        (apply $ exhaustive (compileShare @(NodeTP Layout.Wrapper)))
        term
  let layoutGraph = Layout.wrapGraph hypergraph
  UI.run 50 id layoutStep layoutGraph layoutTree

-- from LambdaScope/GraphRewriting
incIndex :: Int -> [Int] -> [Int]
incIndex 0 (i : is) = i + 1 : is
incIndex 0 []       = [1]
incIndex n (i : is) = i : incIndex (n - 1) is
incIndex n []       = 0 : incIndex (n - 1) []

-- from LambdaScope/GraphRewriting
-- bench :: forall w . Graph (NodeTP w) -> IO ()
-- bench term = do
--   (_, _) <- UI.initialise
--   let hypergraph = execGraph (apply $ exhaustive compileShare) term
--   let indices =
--         evalGraph (benchmark $ toList ruleTree) (Control.wrapGraph hypergraph)
--         --(Control.wrapGraph hypergraph) -- derive control?? TODO
--   print indices
--   let indexTable = foldl (flip incIndex) [] indices
--   let (_, numTree) =
--         mapAccumL (\(i : is) _ -> (is, i)) (indexTable ++ repeat 0) ruleTree
--   putStrLn $ showLabelledTree 2 0 (+) numTree

-- ruleTree
--   :: (EffectApplicable n, forall w . View (NodeTP w) n, View [Port] n)
--   => LabelledTree (Rule n)
-- ruleTree
--   :: (EffectApplicable n, View (NodeTP Layout.Wrapper) n, View [Port] n)
--   => LabelledTree (Rule n)
ruleTree
  :: ( EffectApplicable n
     , View [Port] n
     , forall w . View (NodeTP w) n
     , forall w . View (w (NodeTP w)) n
     )
  => LabelledTree (Rule n)
-- ruleTree
--   :: forall w
--    . ( EffectApplicable (w (NodeTP w))
--      , View [Port] (w (NodeTP w))
--      , View (NodeTP w) (w (NodeTP w))
--      )
--   => LabelledTree (Rule (w (NodeTP w)))
ruleTree = Branch
  "All"
  [ Leaf "Duplicate"   duplicate
  , Leaf "Eliminate"   eliminateDuplicator
  , Leaf "Annihilate"  annihilate
  , Leaf "Erase"       eraser
  , Leaf "Multiplexer" compileShare
  , Branch
    "Effective"
    [ Leaf "Apply Actor"                    applyActor
    , Leaf "Redirect Token"                 redirectToken
    , Leaf "Reflect Token"                  reflectToken
    -- , Leaf "Passthrough Right"              passthroughRight
    , Leaf "Initialize Partial Application" initializeDataPartial
    , Leaf "Apply Partially"                applyDataPartial
    , Leaf "Backpropagate Actor"            backpropagateActor
    , Leaf "Backpropagate Actor 2"          backpropagateActor2
    , Leaf "Backpropagate Uneffectful"      backpropagateUneffectful
    -- , Leaf "Backpropagate Uneffectful2"     backpropagateUneffectful2
    ]
  ]
