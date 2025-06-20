{-# LANGUAGE UnicodeSyntax, FlexibleContexts #-}
-- | Functions for modifying the graph. Although the graph structure is entirely expressed by the graph's node collection, for convenience and efficiency the graph representation also comprises a complementary collection of edges, that has to be synchronised with the node collection. Therefore each of the functions below involves a test for whether the graph structure has been changed, and if so, measures are taken to ensure the graph remains consistent.
--
-- Invariants for graph consistency:
--
--    * Every edge attached to some node points back to that node: ∀n∊N ∀e∊E: n→e ⇔ e→n
--
--    * There are no orphaned edges: ∀e∊E ∃n∊N: e→n

module GraphRewriting.Graph.Write
	(module GraphRewriting.Graph.Write, module GraphRewriting.Graph.Types, module Data.View)
where

import Prelude.Unicode
import GraphRewriting.Graph.Types
import GraphRewriting.Graph.Internal
import GraphRewriting.Graph.Read
import qualified GraphRewriting.Graph.Write.Unsafe as Unsafe
import Control.Monad
import Data.Maybe (catMaybes)
import Data.View
import Data.List
import qualified Data.IntMap as Map
import qualified Data.IntSet as Set


-- | assign new value to given node
writeNode ∷ View [Port] n ⇒ Node → n → Rewrite n ()
writeNode r = modifyNode r . const

-- | modify the node value
modifyNode ∷ View [Port] n ⇒ Node → (n → n) → Rewrite n ()
modifyNode n f = do
	esBefore ← nub <$> inspectNode n
	Unsafe.modifyNode n f
	esAfter ← nub <$> inspectNode n
	Unsafe.register n (esAfter \\ esBefore)
	Unsafe.unregister n (esBefore \\ esAfter)

-- | Wraps 'update' to update aspect @v@ of a node.
updateNode ∷ (View [Port] n, View v n) ⇒ Node → v → Rewrite n ()
updateNode n = adjustNode n . const

-- | Wraps 'adjust' to adjust aspect @v@ of a node.
adjustNode ∷ (View [Port] n, View v n) ⇒ Node → (v → v) → Rewrite n ()
adjustNode n = modifyNode n . adjust

adjustNodeM ∷ (View [Port] n, View v n) ⇒ Node → (v → Rewrite n v) → Rewrite n ()
adjustNodeM n f = updateNode n =<< f =<< inspectNode n

-- | add a new node with value @n@ to the graph
newNode ∷ View [Port] n ⇒ n → Rewrite n Node
newNode v = do
	key ← newRef
	let n = Node key
	modifyNodeMap $ Map.insert key v
	Unsafe.register n (inspect v)
	return n

-- | add a new node with value @n@ to the graph
newNode' ∷ View [Port] n ⇒ n → Rewrite n ()
newNode' v = newNode v >> return ()

-- | Create a new node by cloning another, at the same time updating aspect @v@. When defining rewrites in a context where it is not known what type @n@ the nodes of the graph have, this is the only way to add new nodes to the graph.
copyNode ∷ (View [Port] n, View v n) ⇒ Node → v → Rewrite n Node
copyNode n f = newNode . update f =<< readNode n

-- | Create a new (unconnected) edge. It is expected that the created edge is connected to a port sooner or later. Otherwise the graph will invove unconnected edges.
newEdge ∷ Rewrite n Edge
newEdge = Edge <$> newRef

-- | remove node from the graph
deleteNode ∷ View [Port] n ⇒ Node → Rewrite n ()
deleteNode n = do
	Unsafe.unregister n =<< nub <$> inspectNode n
	modifyNodeMap (Map.delete $ nKey n)

-- | Disconnect ports connected to the given edge by assigning a new (dangling) edge to each of the ports. Then the edge is deleted.
deleteEdge ∷ View [Port] n ⇒ Edge → Rewrite n [Edge]
deleteEdge e = do
	es ← fmap concat $ mapM disconnectPorts =<< attachedNodes e
	modifyEdgeMap $ Map.delete (eKey e)
	return es
	where
	disconnectPorts n = do
		ports ← inspectNode n
		(freshEdges, ports') ← unzip <$> mapM substPort ports
		updateNode n ports'
		return $ catMaybes freshEdges
	substPort p = if p ≡ e
		then do
			p' ← newEdge
			return (Just p', p')
		else return (Nothing, p)

-- | Reconnects the ports connected to the second edge to the first one. Then the second edge is deleted.
mergeEdges ∷ View [Port] n ⇒ Edge → Edge → Rewrite n ()
mergeEdges e1 e2 = when (e1 ≢ e2) $ do
		ns ← attachedNodes e2
		sequence_ [Unsafe.modifyNode n (adjust $ map replacePort) | n ← ns]
		modifyEdgeMap $ Map.adjust (Set.union $ Set.fromList $ map nKey ns) (eKey e1)
		deleteEdge e2 >> return ()
	where replacePort p = if p ≡ e2 then e1 else p
