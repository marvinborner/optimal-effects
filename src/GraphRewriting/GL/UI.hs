{-# LANGUAGE UnicodeSyntax, FlexibleContexts #-}
-- | This module provides an easy-to-use interface to create an interactive, graphical front-end for you graph rewriting system. The controls of the GUI are as follows:
--
-- - Left-click on a menu entry to /select/ a rewriting rule. At all times all redexes with respect to the selected rule are marked red in the graph. Note that the menu is hierarchical, which means that selecting a rule that has subordinate entries has the effect of all these entries being selected.
-- 
-- - Right-click on a menu entry to apply the corresponding rule at every applicable position in the graph simultaneously (in no particular order). Redexes that are destroyed (or created) by prior contractions in this process are not reduced, thus if single applications of the rule terminate, so does its simultaneous application. Right-clicking does /not/ select the rule.
-- 
-- - Right-click on a node of the graph to apply the selected rewriting rule at that position. You know before whether it is a applicable, since all redexes in the graph with respect to the selected rule are marked red. Right-clicking on a non-redex node has no effect. The layouting stops while the right mouse-button is pressed.
-- 
-- - Drag the background of the canvas to scroll around.
-- 
-- - Drag individual nodes of the graph around to manually change the layouting of the graph.
-- 
-- - Use your mouse-wheel to zoom in/out. Make sure to keep the mouse curser in the canvas area and not the menu while zooming.
-- 
-- - Press space to pause/resume layouting. Currently layouting is automatically resumed when the graph is rewritten by right-clicking on an individual node and not when right-clicking on a menu entry. This also requires the mouse cursor to be positioned in the canvas area.
--
-- Please have a look the graph-rewriting-ski package for an example application that makes use of this library.
module GraphRewriting.GL.UI (module GraphRewriting.GL.UI, LabelledTree (..), showLabelledTree) where

import qualified Graphics.UI.GLUT as GL
import Graphics.UI.GLUT (($=), get)
import GraphRewriting.Graph
import GraphRewriting.Graph.Read
import GraphRewriting.Rule
import Data.IORef
import GraphRewriting.GL.Render
import GraphRewriting.GL.Global
import GraphRewriting.GL.HyperEdge
import GraphRewriting.GL.Canvas
import GraphRewriting.GL.Menu
import GraphRewriting.Layout.RotPortSpec
import Data.Set as Set
import Control.Monad

-- | Initialises GLUT. Returns program name and command line arguments.
initialise ∷ IO (String, [String])
initialise = GL.getArgsAndInitialize

run ∷ (View Position n, Render n', View Position n', View Rotation n', PortSpec n', View [Port] n')
    ⇒ Int                  -- ^ The number of initial layout steps to apply before displaying the graph
    → (Graph n → Graph n') -- ^ A projection function that is applied just before displaying the graph
    → (Node → Rewrite n a) -- ^ The monadic graph transformation code for a layout step
    → Graph n
    → LabelledTree (Rule n) -- ^ The rule menu given as a tree of named rules
    → IO ()
run initSteps project layoutStep g rules = do
	globalVars ← newIORef $ GlobalVars
		{graph        = execGraph (replicateM_ initSteps $ mapM layoutStep =<< readNodeList) g,
		 paused       = False,
		 selectedRule = 0,
		 highlighted  = Set.empty,
		 layoutStep   = \n → layoutStep n >> return (),
		 canvas       = undefined,
		 menu         = undefined,
		 getRules     = fmap (\r → (0,r)) rules}


	-- command line benchmarking
--			gvs <- readIORef globalVars
--			finalTree <- reduceAll globalVars (graph gvs) rules
--			putStrLn ("Nr of reductions to normal form:\n" ++ showIndent 0 finalTree) -- (getTopCounter finalTree))
--			putStrLn $ showFlatTabs finalTree
	GL.initialDisplayMode $= [GL.DoubleBuffered, GL.Multisampling]
--	print =<< get GL.sampleBuffers
--	print =<< get GL.samples
--	print =<< get GL.subpixelBits
--
--	GL.initialDisplayCapabilities $= [GL.With GL.DisplayDouble, GL.With GL.DisplaySamples]
--	GL.multisample $= GL.Enabled
	p ← get GL.displayModePossible
	when (not p) $ do
		GL.initialDisplayMode $= [GL.DoubleBuffered]
		p ← get GL.displayModePossible
		when (not p) $ GL.initialDisplayMode $= []
	c ← setupCanvas project star globalVars    -- creates the window, registers callbacks
	modifyIORef globalVars $ \v → v {canvas = c}
	setupMenu globalVars
	layoutLoop globalVars
	GL.mainLoop
	GL.exit
	return ()
