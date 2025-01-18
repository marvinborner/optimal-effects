{-# LANGUAGE UnicodeSyntax, FlexibleContexts, FlexibleInstances, TypeSynonymInstances, MultiParamTypeClasses, ScopedTypeVariables #-}

-- | Enquiry of the graph structure. Note: In this module the term "node" is often used synonymously to "node reference" and "node value". The two can easily distinguished by their type: the former has type 'Node' the latter usually 'n'.
module GraphRewriting.Graph.Read
	(module Data.View,
	 module GraphRewriting.Graph.Types,
	 module GraphRewriting.Graph.Read)
where

import Prelude.Unicode
import GraphRewriting.Graph.Types
import GraphRewriting.Graph.Internal
import Control.Monad.Reader
import qualified Data.IntMap as Map
import qualified Data.IntSet as Set
import Data.View
import Data.List (nub)


type WithGraph n = Reader (Graph n)

-- | This forces the use of the 'Reader' monad. Wrapping a sequence of monadic read-only operations (such as those defined below) into a read-only block can save much overhead e.g. in the state monad.
readOnly ∷ MonadReader (Graph n) m ⇒ Reader (Graph n) a → m a
readOnly r = liftM (runReader r) ask

existNode ∷ MonadReader (Graph n) m ⇒ Node → m Bool
existNode (Node n) = liftM (Map.member n) (asks nodeMap)

readNode ∷ (MonadReader (Graph n) m, MonadFail m) ⇒ Node → m n
readNode (Node n) = maybe (fail $ "readNode: node with ID " ⧺ show n ⧺ " does not exist") return . readRef n =<< asks nodeMap

-- | a wrapper to 'inspect' the given node
inspectNode ∷ (View v n, MonadReader (Graph n) m, MonadFail m) ⇒ Node → m v
inspectNode = liftM inspect . readNode

-- | a wrapper to 'examine' the given node
examineNode ∷ (View v n, MonadReader (Graph n) m, MonadFail m) ⇒ (v → a) → Node → m a
examineNode f = liftM (examine f) . readNode

-- | all of the graph's nodes
readNodeList ∷ MonadReader (Graph n) m ⇒ m [Node]
readNodeList = liftM (map Node . Map.keys) (asks nodeMap)

-- | all of the graph's edges
readEdgeList ∷ MonadReader (Graph n) m ⇒ m [Edge]
readEdgeList = liftM (map Edge . Map.keys) (asks edgeMap)

-- | edges attached to the given node
attachedEdges ∷ (View [Port] n, MonadReader (Graph n) m, MonadFail m) ⇒ Node → m [Edge]
attachedEdges = liftM nub . inspectNode

-- | non-empty set of nodes attached to the given edge
attachedNodes ∷ (MonadReader (Graph n) m, MonadFail m) ⇒ Edge → m [Node]
attachedNodes = liftM (map Node . Set.elems) . readEdge

-- | amount of ports the given hyperedge is connected to
edgeCardinality ∷ (View [Port] n, MonadReader (Graph n) m, MonadFail m) ⇒ Edge → m Int
edgeCardinality e = liftM (length . filter (e ≡) . concat) (mapM inspectNode =<< attachedNodes e)

-- | list of nodes that are connected to the given node, not including the node itself
neighbours ∷ (View [Port] n, MonadReader (Graph n) m, MonadFail m) ⇒ Node → m [Node]
neighbours n = do
	ports ∷ [Port] ← inspectNode n
	edges ← mapM readEdge ports
	let is = Set.unions edges
	-- TODO: implement in terms of [relatives]
	return $ map Node $ Set.elems $ Set.delete (nKey n) is

-- | list of nodes that are connected to the given node, including the node itself
relatives ∷ (View [Port] n, MonadReader (Graph n) m, MonadFail m) ⇒ Node → m [Node]
relatives n = do
	ports ∷ [Port] ← inspectNode n
	edges ← mapM readEdge ports
	let is = Set.unions edges
	return $ map Node $ Set.elems is

-- | nodes connected to given port of the specified node, not including the node itself
adverseNodes ∷ (MonadReader (Graph n) m, MonadFail m) ⇒ Node → Port → m [Node]
adverseNodes (Node n) p = liftM (map Node . Set.elems . Set.delete n) (readEdge p)

-- | whether two nodes are connected
connected ∷ (View [Port] n, MonadReader (Graph n) m, MonadFail m) ⇒ Node → Node → m Bool
connected n1 n2 = liftM (n2 ∈) (relatives n2)

-- | whether the given ports features a dangling edge
dangling ∷ (View [Port] n, MonadReader (Graph n) m, MonadFail m) ⇒ Port → m Bool
dangling = liftM (≡ 1) . edgeCardinality

-- | Map node-relative enquiry over the nodes of the graph.
withNodes ∷ MonadReader (Graph n) m ⇒ (Node → m a) → m [a]
withNodes p = mapM p =<< readNodeList
