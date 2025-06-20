{-# LANGUAGE UnicodeSyntax #-}
-- | Most of the functions for graph scrutiny ('GraphRewriting.Graph.Read') and modification ('GraphRewriting.Graph.Write') are defined monadically. This module defines functions for extracting these monadic values and a few non-monadic graph scrutiny/modification functions.
module GraphRewriting.Graph (module GraphRewriting.Graph, module GraphRewriting.Graph.Types, Graph (..)) where

import GraphRewriting.Graph.Types
import GraphRewriting.Graph.Internal
import Control.Monad.State
import qualified Data.IntMap as Map
import qualified Data.IntSet as Set
import Data.Maybe (fromJust)


emptyGraph ∷ Graph n
emptyGraph = Graph {nodeMap = Map.empty, edgeMap = Map.empty, nextKey = 0}

nodes ∷ Graph n → [n]
nodes = Map.elems . nodeMap

-- | Each edge corresponds to the set of nodes it connects
edges ∷ Graph n → [(Edge, [n])]
edges g = map lookupNodes $ Map.assocs (edgeMap g) where
	lookupNodes (e,ns) = (Edge e, map (\n → fromJust $ Map.lookup n $ nodeMap g) $ Set.elems ns)

-- | unsafe, since no checks are performed to ensure that the invariants from
-- "GraphRewriting.Graph.Write" are preserved
unsafeMapNodes ∷ (n → n') → Graph n → Graph n'
unsafeMapNodes f g = g {nodeMap = Map.map f $ nodeMap g}

-- | map that supplies an additional unique key to the mapping function; unsafe
-- in the same way as 'unsafeMapNodes'
unsafeMapNodesUnique ∷ (Int → n → n') → Graph n → Graph n'
unsafeMapNodesUnique f g = g {nodeMap = Map.mapWithKey f $ nodeMap g}

-- | apply a monadic graph modification to a graph
runGraph ∷ Rewrite n a → Graph n → (a, Graph n)
runGraph = runState . rewrite

evalGraph ∷ Rewrite n a → Graph n → a
evalGraph = evalState . rewrite

execGraph ∷ Rewrite n a → Graph n → Graph n
execGraph = execState . rewrite
