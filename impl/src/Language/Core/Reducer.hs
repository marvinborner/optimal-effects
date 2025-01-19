-- Parts were originally written for lambdascope in `Reducer.hs`
--   under the BSD-3-Clause License (as in src/GraphRewriting/LICENSE)
-- Copyright (c) 2010, Jan Rochel
-- Copyright (c) 2024, Marvin Borner

{-# LANGUAGE FlexibleContexts #-}

module Language.Core.Reducer
  ( nf
  , visualize
  ) where

import           Data.Core
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
import           Language.Core.GL
import           Language.Core.Rules

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

-- | Reduce graph to normal form
nf :: Graph NodeLS -> Graph NodeLS
nf term = term

-- | Visualize reduction to normal form
-- TODO: only app should use IO
visualize :: Graph NodeLS -> IO ()
visualize term = do
  (_, _) <- UI.initialise
  let hypergraph  = execGraph (apply $ exhaustive compileShare) term
  let layoutGraph = Layout.wrapGraph hypergraph
  UI.run 50 id layoutStep layoutGraph ruleTree

ruleTree :: (View NodeLS n, View [Port] n) => LabelledTree (Rule n)
ruleTree = Branch
  "All"
  [ Leaf "Beta Reduction" beta
  , Branch
    "All but Beta"
    [ Leaf "Duplicate" duplicate
    , Leaf
      "Eliminate"
      (   eliminateDelimiterEraser
      <|> eliminateDelimiterConstant
      <|> eliminateDuplicator
      )
    , Leaf "Annihilate"        annihilate
    , Leaf "Commute Delimiter" commuteDelimiter
    , Leaf "Erase"             eraser
    , Branch
      "Effective"
      [ Leaf "Constant"         applyConstant
      , Leaf "Apply Effectful"  applyEffectful
      , Leaf "Exec Effectful"   execEffectful
      , Leaf "Reduce Effectful" reduceEffectful
      ]
    ]
  ]
