{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}
-- | Rewrite rules are represented as nested monads: a 'Rule' is a 'Pattern' that returns a 'Rewrite' the latter directly defining the transformation of the graph.
--
-- For rule construction a few functions a provided: The most primitive one is 'rewrite'. In most cases 'erase', 'rewire', and 'replace' should be more convenient. These functions express rewrites that /replace/ the matched nodes of the 'Pattern', which comes quite close to the @L -> R@ form in which graph rewriting rules are usually expressed.
module GraphRewriting.Rule
  ( Replace
  , module GraphRewriting.Rule
  ) where

import           Prelude.Unicode

import           Control.Applicative
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.List                      ( (\\)
                                                , mapAccumL
                                                , nub
                                                )
import           GraphRewriting.Graph.Read
import           GraphRewriting.Graph.Write
import           GraphRewriting.Pattern
import           GraphRewriting.Rule.Internal
import           System.Random
import           System.Random.Shuffle

import qualified Data.IntMap                   as Map
import qualified Data.IntSet                   as Set

import           Debug.Trace

-- | A rewriting rule is defined as a 'Pattern' that returns a 'Rewrite'
type Rule n = Pattern n (Rewrite n ())

-- | Apply rule at an arbitrary position if applicable
apply :: Rule n -> Rewrite n ()
apply = let void m = m >> return () in void . apply'

-- | Apply rule at an arbitrary position. Return value states whether the rule was applicable.
apply' :: Rule n -> Rewrite n Bool
apply' r = do
  contractions <- evalPattern r <$> ask
  if null contractions then return False else head contractions >> return True

-- rule construction ---------------------------------------------------------

-- | primitive rule construction with the matched nodes of the left hand side as a parameter
rewrite :: (Match -> Rewrite n a) -> Rule n
rewrite r = do
  h <- history
  return $ r h >> return ()

-- | constructs a rule that deletes all of the matched nodes from the graph
erase :: View [Port] n => Rule n
erase = rewrite $ mapM_ deleteNode . nub

-- | Constructs a rule from a list of rewirings. Each rewiring specifies a list of hyperedges that are to be merged into a single hyperedge. All matched nodes of the left-hand side are removed.
rewire :: View [Port] n => [[Edge]] -> Rule n
rewire ess = rewrite $ \hist -> do
  mapM_ mergeEs $ joinEdges ess
  mapM_ deleteNode $ nub hist


instance Monad (Replace n) where
  return x = Replace $ return (x, [])
  Replace r1 >>= f = Replace $ do
    (x1, merges1) <- r1
    let Replace r2 = f x1
    (y, merges2) <- r2
    return (y, merges1 ⧺ merges2)

instance Functor (Replace n) where
  fmap f (Replace r) = Replace $ do
    (x, merges) <- r
    return (f x, merges)

instance Applicative (Replace n) where
  Replace rf <*> Replace rx = Replace $ do
    (f, merges1) <- rf
    (x, merges2) <- rx
    return (f x, merges1 ⧺ merges2)
  pure = return

instance Semigroup (Replace n ()) where
  (<>) = (>>)

instance Monoid (Replace n ()) where
  mempty  = return ()
  mappend = (>>)

replace :: View [Port] n => Replace n () -> Rule n
replace (Replace rhs) = do
  lhs <- nub <$> history
  when (null lhs) (fail "replace: must match at least one node")
  return $ do
    mapM_ mergeEs =<< joinEdges . snd <$> rhs
    mapM_ deleteNode lhs

byNode :: (View [Port] n, View v n) => v -> Replace n ()
byNode v = Replace $ do
  n <- head <$> readNodeList
  _ <- copyNode n v
  return ((), [])

byNewNode :: View [Port] n => n -> Replace n ()
byNewNode n = Replace $ newNode n >> return ((), [])

byEdge :: Replace n Edge
byEdge = Replace $ do
  e <- newEdge
  return (e, [])

byWire :: Edge -> Edge -> Replace n ()
byWire e1 e2 = byConnector [e1, e2]

byConnector :: [Edge] -> Replace n ()
byConnector es = Replace $ return ((), [es])

-- combinators ---------------------------------------------------------------

-- | Apply two rules consecutively. Second rule is only applied if first one succeeds. Fails if (and only if) first rule fails.
(>>>) :: Rule n -> Rule n -> Rule n
r1 >>> r2 = do
  rw1 <- r1
  return $ rw1 >> apply r2

(>&>) :: Rule n -> Rule n -> Rule n
r1 >&> r2 = do
  return $ apply r1
  return $ apply r2
  -- rw1 <- r1
  -- return $ rw1 >> apply r2

-- | Make a rule exhaustive, i.e. such that (when applied) it reduces redexes until no redexes are occur in the graph.
exhaustive :: Rule n -> Rule n
exhaustive = foldr1 (>>>) . repeat

-- | Make a rule parallel, i.e. such that (when applied) all current redexes are contracted one by one. Neither new redexes or destroyed redexes are reduced.
everywhere :: Rule n -> Rule n
everywhere r = do
  ms <- amnesia $ matches r
  exhaustive $ restrictOverlap (\hist future -> future ∈ ms) r

-- | Repeatedly apply the rules from the given list prefering earlier entries.
-- Returns a list of indexes reporting the sequence of rules that has applied.
benchmark :: [Rule n] -> Rewrite n [Int]
benchmark rules = rec where

  rec = do
    contractions <- evalPattern (anyOf indexedRules) <$> ask
    case contractions of
      []          -> return []
      (i, rw) : _ -> fmap (i :) (rw >> rec)

  indexedRules = zipWith addIndex [0 ..] rules
   where
    addIndex i rule = do
      rw <- rule
      return (i, rw)

-- | Repeatedly apply the rules from the given list prefering earlier entries.
-- Returns a list of indexes reporting the sequence of rules that has applied.
benchmarkRandom :: RandomGen gen => gen -> [Rule n] -> Rewrite n [Int]
benchmarkRandom rng rules = rec rng where

  rec rng = do
    let shuffled = shuffle' indexedRules (length indexedRules) rng
    contractions <- evalPattern (anyOf shuffled) <$> ask
    case contractions of
      []          -> return []
      (i, rw) : _ -> trace (show i) $ fmap (i :) (rw >> rec (fst $ split rng))
      -- (i, rw) : _ -> fmap (i :) (rw >> rec (fst $ split rng))

  indexedRules = zipWith addIndex [0 ..] rules
   where
    addIndex i rule = do
      rw <- rule
      return (i, rw)
