{-# LANGUAGE UnicodeSyntax, FlexibleContexts, MultiParamTypeClasses #-}
-- | Offers an 'activePair' pattern for convenient implementation of interaction nets.
module GraphRewriting.Pattern.InteractionNet where

import Prelude.Unicode
import Data.View
import Data.Functor
import GraphRewriting.Graph.Types
--import GraphRewriting.Graph.Read
import GraphRewriting.Pattern


-- | Index that identifies the principal port within the list of ports
class INet n where principalPort ∷ n → Port

-- | Instead of @(,)@ to save parentheses
data Pair a = a :-: a

instance Functor Pair where fmap f (x :-: y) = f x :-: f y

pair ∷ Pair a → (a,a)
pair (x :-: y) = (x,y)

activePair ∷ (View [Port] n, View v n, INet v) ⇒ Pattern n (Pair v)
activePair = linear $ do
	v1 ← node
	let pp1 = principalPort v1
	v2 ← nodeWith pp1
	require (pp1 ≡ principalPort v2)
	return (v1 :-: v2)
