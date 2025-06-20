{-# LANGUAGE UnicodeSyntax, FlexibleContexts #-}
-- | This modules provides variants of the functions in "GraphRewriting.Graph.Write" for transforming the graph, but without checking for changed port assignments, which could lead to an inconsistent state. Therefore these should only be used (for increased efficiency) if the modifications do not change the graph structure (such as in layouting), or you really know what you are doing. Note that the functions provided by this library never change the length of the port list

-- (TODO: use Functor/Traversable/Foldable instead of lists?)
module GraphRewriting.Graph.Write.Unsafe
	(module GraphRewriting.Graph.Write.Unsafe, module GraphRewriting.Graph.Types)
where

import Prelude.Unicode
import Data.Maybe (fromMaybe)
import GraphRewriting.Graph.Types
import GraphRewriting.Graph.Internal
import GraphRewriting.Graph.Read
import qualified Data.IntMap as Map
import qualified Data.IntSet as Set


writeNode ∷ Node → n → Rewrite n ()
writeNode r = modifyNode r . const

modifyNode ∷ Node → (n → n) → Rewrite n ()
modifyNode n f = modifyNodeMap . Map.insert (nKey n) . f =<< readNode n

updateNode ∷ View v n ⇒ Node → v → Rewrite n ()
updateNode n = adjustNode n . const

adjustNode ∷ View v n ⇒ Node → (v → v) → Rewrite n ()
adjustNode n = modifyNode n . adjust

adjustNodeM ∷ (View [Port] n, View v n) ⇒ Node → (v → Rewrite n v) → Rewrite n ()
adjustNodeM n f = updateNode n =<< f =<< inspectNode n

unregister ∷ Node → [Edge] → Rewrite n ()
unregister (Node n) es = modifyEdgeMap $ flip (foldr $ Map.update deleteN) (map eKey es)
	where deleteN ns = if ns ≡ Set.singleton n then Nothing else Just $ Set.delete n ns

register ∷ Node → [Edge] → Rewrite n ()
register (Node n) es = modifyEdgeMap $ flip (foldr $ Map.alter addN) (map eKey es)
	where addN ns = Just (Set.insert n $ fromMaybe Set.empty ns)
