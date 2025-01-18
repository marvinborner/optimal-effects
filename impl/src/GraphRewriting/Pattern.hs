{-# LANGUAGE UnicodeSyntax, FlexibleContexts #-}
-- | Patterns allow monadic scrutiny of the graph (modifications are not possible) while keeping track of matched nodes (history). A 'Pattern' is interpreted by 'runPattern' that returns a result for each position in the graph where the pattern matches. It is allowed to 'fail' inside the 'Pattern' monad, indicating that the pattern does not match, which corresponds to conditional rewriting.
module GraphRewriting.Pattern (module GraphRewriting.Pattern, PatternT, Pattern, Match, (<|>)) where

import Prelude.Unicode
import GraphRewriting.Pattern.Internal
import GraphRewriting.Graph.Read
import Control.Monad.Reader
import Control.Monad.List
import Data.Functor.Identity
import qualified Data.Set as Set (empty, insert, member)
import Control.Applicative


-- | A pattern represents a graph scrutiny that memorises all the scrutinised nodes during matching.
type Pattern n = PatternT n Identity

instance MonadFail Identity where
	fail = error

instance Monad m ⇒ Monad (PatternT n m) where
	return x = PatternT $ \h → return ([],x)
	p >>= f = PatternT $ \h → do
		(m1,x) ← patternT p h
		(m2,y) ← patternT (f x) (reverse m1 ⧺ h)
		return (m1 ⧺ m2, y)

instance MonadFail m ⇒ MonadFail (PatternT n m) where
	fail str = PatternT $ \h → lift (fail str)

instance MonadTrans (PatternT n) where
	lift m = PatternT $ \h → do
		x ← lift $ lift m
		return ([],x)

-- TODO: Change constraint to Functor m if possible
instance Monad m ⇒ Functor (PatternT n m) where fmap = liftM

-- TODO: Change constraint from Monad m if possible
instance Monad m ⇒ Applicative (PatternT n m) where
	pure = return
	f <*> x = do
		f' ← f
		f' <$> x

instance MonadFail m ⇒ Alternative (PatternT n m) where
	empty = mzero
	(<|>) = mplus

instance MonadFail m ⇒ Semigroup (PatternT n m a) where
	(<>) = mplus

instance MonadFail m ⇒ Monoid (PatternT n m a) where
	mempty = mzero
	mappend = mplus

instance MonadFail m ⇒ MonadPlus (PatternT n m) where
	mzero = fail "empty result list"
	mplus p q = PatternT $ \h → do -- TODO: this implements choice. Is mplus the right function for that?
		g ← ask
		lift $ runReaderT (patternT p h) g `mplus` runReaderT (patternT q h) g

runPatternT ∷ PatternT n m a → Graph n → m [(Match,a)]
runPatternT = runPatternT' []

-- | Apply a pattern on a graph returning a result for each matching position in the graph together with the matched nodes.
runPattern ∷ Pattern n a → Graph n → [(Match,a)]
runPattern p = runIdentity . runPatternT p

evalPattern ∷ Pattern n a → Graph n → [a]
evalPattern p = map snd . runPattern p

execPattern ∷ Pattern n a → Graph n → [Match]
execPattern p = map fst . runPattern p

-- combinators ---------------------------------------------------------------

-- | Something like an implicit monadic map
branch ∷ Monad m ⇒ [a] → PatternT n m a -- TODO: express this using Alternative?
branch xs = PatternT $ \h → lift $ ListT $ return [([],x) | x ← xs]

-- | 'branch' on each node, add it to the history, and return it
branchNodes ∷ MonadFail m ⇒ [Node] → PatternT n m Node
branchNodes ns = do -- TODO: express this using Alternative?
	n ← branch ns
	visit n
	return n

-- | Probe whether a pattern matches somewhere on the graph. You might want to combine this with 'amnesia'.
probe ∷ Monad m ⇒ PatternT n m a → PatternT n m Bool
probe p = not . null <$> matches p

-- | probe a pattern returning the matches it has on the graph. You might want to combine this with 'amnesia'.
matches ∷ Monad m ⇒ PatternT n m a → PatternT n m [Match]
matches p = map fst <$> match p

-- TODO: isn't this essentially same as runPatternT?
-- | probe a pattern returning the matches it has on the graph. You might want to combine this with 'amnesia'.
match ∷ Monad m ⇒ PatternT n m a → PatternT n m [(Match, a)]
match p = PatternT $ \h → do
	matches ← liftM (runReaderT $ patternT p h) ask -- list of all possible matches
	let roundup = liftM (\xs → [(concatMap fst xs, xs)]) (runListT matches) -- concatenation into one big match
	lift $ ListT roundup

-- | choice over a list of patterns
anyOf ∷ Alternative f ⇒ [f a] → f a
anyOf = foldr (<|>) empty

-- | conditional rewriting: 'fail' when predicate is not met
require ∷ MonadFail m ⇒ Bool → m ()
require p = unless p $ fail "requirement not met"

-- | 'fail' if given pattern succeeds, succeed if it fails.
requireFailure ∷ MonadFail m ⇒ PatternT n m a → PatternT n m ()
requireFailure p = require . not =<< probe p

-- | 'fail' when monadic predicate is not met
requireM ∷ MonadFail m ⇒ m Bool → m ()
requireM p = p >>= require

-- some base patterns --------------------------------------------------------

-- | Lift a scrutiny from 'Reader' to 'Pattern' leaving the history unchanged.
liftReader ∷ Monad m ⇒ Reader (Graph n) a → PatternT n m a
liftReader r = PatternT $ \h → do
	x ← runReader r `liftM` ask
	return ([],x)

-- | any node anywhere in the graph
node ∷ (MonadFail m, View v n) ⇒ PatternT n m v
node = liftReader . inspectNode =<< branchNodes =<< liftReader readNodeList

-- | A specific node
nodeAt ∷ (Monad m, View v n) ⇒ Node → PatternT n m v
nodeAt ref = do
	n ← liftReader $ inspectNode ref
	PatternT $ \h → lift $ return ([ref],n)

-- | any edge anywhere in the graph
edge ∷ Monad m ⇒ PatternT n m Edge
edge = branch =<< liftReader readEdgeList

-- | node that is connected to given edge
nodeWith ∷ (MonadFail m, View v n) ⇒ Edge → PatternT n m v
nodeWith e = liftReader . inspectNode =<< branchNodes =<< liftReader (attachedNodes e)

-- | edge that is attached to given node
edgeOf ∷ (Monad m, View [Port] n) ⇒ Node → PatternT n m Edge
edgeOf n = branch =<< liftReader (attachedEdges n)

-- | node that is connected to the given node, but not that node itself
neighbour ∷ (MonadFail m) => (View [Port] n, View v n) ⇒ Node → PatternT n m v
neighbour n = liftReader . inspectNode =<< branchNodes =<< liftReader (neighbours n)

-- | node that is connected to the given node, permitting the node itself
relative ∷ (MonadFail m, View [Port] n, View v n) ⇒ Node → PatternT n m v
relative n = liftReader . inspectNode =<< branchNodes =<< liftReader (relatives n)

-- | nodes connected to given port of the specified node, not including the node itself.
-- Consider as an alternative 'linear' combined with 'nodeWith'.
adverse ∷ (MonadFail m, View [Port] n, View v n) ⇒ Port → Node → PatternT n m v
adverse p n = liftReader . inspectNode =<< branchNodes =<< liftReader (adverseNodes n p)

-- controlling history and future --------------------------------------------

-- | A specific node
visit ∷ MonadFail m ⇒ Node → PatternT n m ()
visit n = do
	exists ← liftReader $ existNode n
	if exists
		then PatternT $ \h → lift $ return ([n],())
		else fail $ "visit: node with ID " ⧺ show n ⧺ " does not exist"

-- | Do not remember any of the nodes matched by the supplied pattern
amnesia ∷ Monad m ⇒ PatternT n m a → PatternT n m a
amnesia p = PatternT $ \h → do
	(h',x) ← patternT p h
	return ([],x)

-- | list of nodes matched until now with the most recent node in head position
history ∷ Monad m ⇒ PatternT n m Match
history = PatternT $ \h → return ([],h)

-- | a reference to the lastly matched node
previous ∷ Monad m ⇒ PatternT n m Node
previous = head <$> history

-- | only match nodes in the next pattern that have not been matched before
nextFresh ∷ Monad m ⇒ PatternT n m a → PatternT n m a
nextFresh = restrictOverlap $ \past future → null future ∨ not (head future ∈ past)

-- | only accept the given node in the next match
nextIs ∷ Monad m ⇒ Node → PatternT n m a → PatternT n m a
nextIs next = restrictOverlap $ \past future → not (null future) ∧ head future ≡ next

-- | Restrict a pattern based on the which of nodes have been matched previously and which nodes will be matched in the future. The first parameter of the supplied function is the history with the most recently matched node in head position. The second parameter is the future with the next matched node in head position.
restrictOverlap ∷ Monad m ⇒ (Match → Match → Bool) → PatternT n m a → PatternT n m a
restrictOverlap c p = PatternT $ \h → do
	(h',x) ← patternT p h
	require $ c h h'
	return (h',x)
-- TODO: the check is only done after the whole pattern has matched (maybe do the check more often inbetween?)

-- | Nodes in the future may not be matched more than once.
linear ∷ Monad m ⇒ PatternT n m a → PatternT n m a
linear = restrictOverlap $ \hist future → isLinear Set.empty future where
	isLinear left [] = True
	isLinear left (r:rs) = not (r `Set.member` left) ∧ isLinear (r `Set.insert` left) rs
