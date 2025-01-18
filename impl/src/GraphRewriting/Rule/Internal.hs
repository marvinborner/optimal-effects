{-# LANGUAGE UnicodeSyntax, FlexibleContexts #-}
module GraphRewriting.Rule.Internal where

import GraphRewriting.Graph.Internal
import GraphRewriting.Graph.Write
import qualified Data.IntSet as Set


type MergeEdges = [Edge]

newtype Replace n a = Replace (Rewrite n (a, [MergeEdges]))

mergeEs :: View [Port] n ⇒ MergeEdges -> Rewrite n ()
mergeEs (e:es) = mapM_ (mergeEdges e) es

type Set = Set.IntSet

joinEdges ∷ [[Edge]] → [[Edge]]
joinEdges = map (map Edge . Set.elems) . join . map (Set.fromList . map eKey)

-- The code below is essentially maintaining equivalence classes. TODO: use a library for that.

-- | Join pairs of sets with a common element until all sets are disjoint.
join ∷ [Set] → [Set]
join = foldr join1 []

-- | Add to a list of disjoint sets a further set and join sets with common elements such that the resulting list again only contains disjoint sets.
join1 ∷ Set → [Set] → [Set]
join1 x [    ] = [x]
join1 x (y:ys) = if Set.null $ Set.intersection x y
	then y : join1 x ys
	else join1 (Set.union x y) ys
