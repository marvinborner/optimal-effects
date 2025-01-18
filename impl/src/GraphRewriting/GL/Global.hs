{-# LANGUAGE FlexibleContexts #-}
module GraphRewriting.GL.Global where

import Prelude.Unicode
import Graphics.UI.GLUT (addTimerCallback, Window, postRedisplay)
import GraphRewriting.Graph
import GraphRewriting.Graph.Read
import GraphRewriting.Rule
import GraphRewriting.Pattern
import Data.IORef
import GraphRewriting.Layout.RotPortSpec
import qualified Data.Set as Set
import Data.Set (Set)
import Data.List ((\\))
import Control.Monad (when, unless, replicateM_)
import Data.Foldable
import Data.Functor
import Data.Traversable
import Prelude hiding (concat, concatMap, or, elem, foldr, any, mapM, unzip)


data GlobalVars n = GlobalVars
	{graph        :: Graph n,
	 paused       :: Bool,
	 selectedRule :: Int,
	 highlighted  :: Set Node,
	 layoutStep   :: Node -> Rewrite n (),
	 canvas       :: Window,
	 menu         :: Window,
	 getRules     :: RuleTree n}

data LabelledTree a = Branch String [LabelledTree a] | Leaf String a

data LTZipper a = Root | Child String [LabelledTree a] (LTZipper a) [LabelledTree a]

type LTLoc a = (LabelledTree a, LTZipper a)

-- depth-first traversal
next :: LTLoc a -> Maybe (LTLoc a)
next (Branch b (t:ts), p) = Just (t, Child b [] p ts)
next (Leaf l x, p) = right (Leaf l x, p)
next _ = Nothing

nth :: Int -> LTLoc a -> Maybe (LTLoc a)
nth n l = iterate (>>= next) (Just l) !! n

right :: LTLoc a -> Maybe (LTLoc a)
right (t, Child c ls p (r:rs)) = Just (r, Child c (ls ⧺ [t]) p rs)
right (t, Child c ls p []) = up (t, Child c ls p []) >>= right
right _ = Nothing

up :: LTLoc a -> Maybe (LTLoc a)
up (t, Child c ls p rs) = Just (Branch c (ls ⧺ [t] ⧺ rs), p)
up _ = Nothing

put :: LTLoc a -> LabelledTree a -> LTLoc a
put (_, p) t = (t, p)

top :: LTLoc a -> LTLoc a
top (t, Root) = (t, Root)
top (t, Child c ls p rs) = top (Branch c (ls ⧺ [t] ⧺ rs), p)

root :: LabelledTree a -> LTLoc a
root t = (t, Root)

instance Foldable LabelledTree where
	foldr f y (Leaf l x) = f x y
	foldr f y (Branch l ts) = foldr (flip $ foldr f) y ts

instance Functor LabelledTree where
	fmap f (Leaf l x) = Leaf l (f x)
	fmap f (Branch l ts) = Branch l $ fmap f <$> ts

instance Traversable LabelledTree where
	traverse f (Leaf l x) = Leaf l <$> f x
	traverse f (Branch l ts) = Branch l <$> traverse (traverse f) ts

showRuleTree :: RuleTree n -> String
showRuleTree = showLabelledTree 2 0 (+) . fmap fst

showLabelledTree :: Show a => Int -> a -> (a -> a -> a) -> LabelledTree a -> String
showLabelledTree indentation init combine = snd . rec where

	rec (Leaf l x) = (x, l ⧺ " " ⧺ show x)
	rec (Branch l ts) = (x, l ⧺ " " ⧺ show x ⧺ "\n" ⧺ indent (unlines ls)) where
		x = foldr combine init xs
		(xs, ls) = unzip $ map rec ts

	indent str = unlines $ map (replicate indentation ' ' ⧺) (lines str)

	unlines [] = ""
	unlines [x] = x
	unlines xs = head xs ⧺ "\n" ⧺ unlines (tail xs)

instance Show a => Show (LabelledTree a) where
	show (Leaf   l x) = l ⧺ " " ⧺ show x
	show (Branch l s) = l ⧺ "\n" ⧺ indent (unlines $ map show s) where
			indent str = unlines $ map (replicate 2 ' ' ⧺) (lines str)
			unlines [] = ""
			unlines [x] = x
			unlines xs = head xs ⧺ "\n" ⧺ unlines (tail xs)

redisplay :: Window -> IO ()
redisplay = postRedisplay . Just

readGraph = fmap graph . readIORef
writeGraph g = modifyGraph (const g)

modifyGraph f globalVars = do
	modifyIORef globalVars $ \v -> v {graph = f $ graph v}

applyRule :: Rule n -> IORef (GlobalVars n) -> IO ()
applyRule r globalVars = do
	layout <- layoutStep <$> readIORef globalVars
	g <- readGraph globalVars
	let ns = evalGraph readNodeList g
	-- we don't use the fist element of the tuple and compute newNodes ourselves due to a bug in the graph-rewriting package (It's completely out of my hands!!!!1)
	let (_, g') = runGraph (apply r) g
	let ns' = evalGraph readNodeList g'
	let newNodes = ns' Data.List.\\ ns
	writeGraph (execGraph (replicateM_ 15 $ mapM layout newNodes) g') globalVars
	highlight globalVars

selectRule i globalVars = do
	ruleListLength <- numNodes . getRules <$> readIORef globalVars
	when (0 ≤ i ∧ i < ruleListLength) $ do
		modifyIORef globalVars $ \v -> v {selectedRule = i}
		highlight globalVars

highlight globalVars = do
	gv@GlobalVars {graph = g, getRules = rs, selectedRule = r, highlighted = h, canvas = c} <- readIORef globalVars
	let rule = foldMap snd (subtrees rs !! r)
	let h' = Set.fromList [head match | (match,rewrite) <- runPattern rule g]
	writeIORef globalVars $ gv {highlighted = h'}
	redisplay c

layoutLoop globalVars = do
	gv@GlobalVars {graph = g, paused = p, layoutStep = l, canvas = c} <- readIORef globalVars
	unless p $ do
		examine position (head $ nodes g) `seq` return ()
		writeIORef globalVars $ gv {graph = execGraph (mapM l =<< readNodeList) g} -- TODO: relayout all nodes at once
		redisplay c
		addTimerCallback 40 $ layoutLoop globalVars

pause globalVars = modifyIORef globalVars $ \vs -> vs {paused = True}

resume globalVars = do
	modifyIORef globalVars $ \vs -> vs {paused = False}
	layoutLoop globalVars

subtrees :: LabelledTree a -> [LabelledTree a]
subtrees t = t : case t of
	Leaf _ _ -> []
	Branch l ts -> concatMap subtrees ts

numNodes :: LabelledTree a -> Int
numNodes = length . subtrees

type RuleTree n = LabelledTree (Int, Rule n)

-- | Traverses the rule tree depth-first and executes all leaf rules it encounters. Rules are
-- executed everywhere they match, except if they overlap one of them is chosen at random.
-- So this corresponds to a complete development.
applyLeafRules :: (Rule n -> Rule n) -> Int -> IORef (GlobalVars n) -> IO ()
applyLeafRules restriction idx gvs = do
	g <- readGraph gvs
	comptree <- getRules <$> readIORef gvs
	let pos = nth idx (root comptree)
	case pos of
		Nothing -> return ()
		Just (tree,p) -> do
			let ns = evalGraph readNodeList g
			-- first we mark all redexes
			let rule = restriction $ foldMap snd tree
			-- then we find a non-overlapping subset
			let ms = head $ evalPattern (matches rule) g
			-- then we apply the rules in the leafs while restricting them to that subset
			let ((_, g'), tree') = mapAccumL applyLeafRules' (ms, g) tree
			let ns' = evalGraph readNodeList g'
			let newNodes = ns' Data.List.\\ ns
			layout <- layoutStep <$> readIORef gvs
			writeGraph (execGraph (replicateM_ 15 (mapM layout newNodes)) g') gvs
			modifyIORef gvs $ \x -> x {getRules = fst $ top (tree',p)}

	where

	-- At every leaf apply the rule restricted to the set of predetermined matches, every time removing the
	-- the match from the set updating the graph and the counter.
--	applyLeafRules' :: ([Match], Graph n) -> (Int, Rule n) -> (([Match], Graph n), (Int, Rule n))
	applyLeafRules' (matches, g) (n, r) = let
			ms = runPattern r' g
			r' = restrictOverlap (\past future -> future `elem` matches) (restriction r)
		in if null ms
			then ((matches, g), (n, r))
			else let
					(match, rewrite) = head ms
					g' = execGraph rewrite g
				in applyLeafRules' (filter (not . any (`elem` match)) matches, g') (n + 1, r)
