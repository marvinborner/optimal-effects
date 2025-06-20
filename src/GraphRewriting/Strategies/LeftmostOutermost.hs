{-# LANGUAGE UnicodeSyntax, FlexibleContexts #-}
module GraphRewriting.Strategies.LeftmostOutermost where

import GraphRewriting.Pattern
import GraphRewriting.Graph.Write
import GraphRewriting.Graph.Read
import GraphRewriting.Rule
import GraphRewriting.Strategies.Control
import Data.List (intersect, (\\))


-- | Gives us the the 'left' port for a given node
class LeftmostOutermost n where lmoPort ∷ n → Maybe Port

instance LeftmostOutermost n ⇒ LeftmostOutermost (Wrapper n) where lmoPort = lmoPort . wrapped

getLmoPort ∷ (LeftmostOutermost n) ⇒ Node → Pattern n Port
getLmoPort n = do
	node ← liftReader $ readNode n
	case lmoPort node of
		Nothing → fail "Term is in WHNF"
		Just lo → return lo

-- It does not compile with this type signature, even when IncoherentInstances are given in Control.
moveControl ∷ (View [Port] n, View Control n, LeftmostOutermost n) => Rule n
moveControl = do
	Control {stack = s} ← node
	control ← previous
	lmo1 ← getLmoPort control
	n ← branchNodes =<< liftReader . adverseNodes control =<< getLmoPort control
	return $ do
		updateNode control NoControl
		updateNode n (Control {stack = control : s})

-- It does not compile with this type signature, even when IncoherentInstances are given in Control.
leftmostOutermost ∷ (View Control n, View [Port] n) ⇒ Rule n → Rule n
leftmostOutermost r = do
	rewrite ← r
	ns ← history -- we want the first node of the matching pattern
	let topnode = last ns
	Control {stack = s} ← liftReader $ inspectNode topnode
	return $ do
		updateNode topnode NoControl -- First we set the topnode to not be the control node any more
		oldNodes ← readNodeList
		rewrite -- then we perform the rewrite
		newNodes ← readNodeList
		let s' = intersect s newNodes -- only consider nodes for the control marker that exist
		if null s' -- even the topmost node has been replaced
			then do -- we assign the control marker to one of the newly created nodes
				let addedNodes = newNodes \\ oldNodes
				updateNode (head addedNodes) (Control {stack = []}) -- finally we set the previous node on the stack as the control node
			else do -- set the previous node on the stack as the control node
				updateNode (head s') (Control {stack = tail s'})
