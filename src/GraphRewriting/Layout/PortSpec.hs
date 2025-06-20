{-# LANGUAGE UnicodeSyntax, FlexibleContexts #-}
module GraphRewriting.Layout.PortSpec
	(module GraphRewriting.Layout.PortSpec,
	 module GraphRewriting.Layout.Position,
	 module Data.View)
where

import Prelude.Unicode
import GraphRewriting.Graph
import GraphRewriting.Layout.Position
import Data.View
import Data.Maybe (catMaybes)


-- | Port position relative to the node center, and the direction in which edges should stick out.
class PortSpec n where portSpec ∷ n → [(Vector2, Vector2)]

sameDir ∷ Vector2 → (Vector2, Vector2)
sameDir r = (r,r)

portDir ∷ PortSpec n ⇒ n → [Vector2]
portDir = map snd . portSpec

relPortPos ∷ PortSpec n ⇒ n → [Vector2]
relPortPos = map fst . portSpec

absPortPos ∷ (PortSpec n, View Position n) ⇒ n → [Vector2]
absPortPos n = map (examine position n +) (relPortPos n)

propOfPort ∷ View [Port] n ⇒ (n → [a]) → Edge → n → [a]
propOfPort portProps e n = catMaybes $ zipWith filterE (inspect n) (portProps n)
	where filterE edge portPos = if edge ≡ e then Just portPos else Nothing
