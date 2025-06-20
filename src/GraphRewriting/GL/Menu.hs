module GraphRewriting.GL.Menu (LabelledTree (..), setupMenu) where

import Prelude.Unicode
import Graphics.UI.GLUT as GLUT
import qualified Graphics.Rendering.OpenGL as GL
import GraphRewriting.GL.Global
import Data.IORef
import Control.Monad
import Data.Functor ()
import Control.Applicative ()


menuItemHeight = 20
font = Fixed9By15

setupMenu :: IORef (GlobalVars n) -> IO ()
setupMenu globalVars = do
	c <- canvas <$> readIORef globalVars
	ruleTree <- getRules <$> readIORef globalVars
	charWidth <- stringWidth font "0"
	let cols = fromIntegral $ maximum $ map length $ lines $ showRuleTree ruleTree
	let winWidth = (cols + 1) * min 10 charWidth
	let ruleListLength = numNodes ruleTree
	let winSize = Size winWidth (fromIntegral $ menuItemHeight * ruleListLength)
	menu <- createSubWindow c (GL.Position 0 0) winSize
	modifyIORef globalVars $ \x -> x {menu=menu} -- set the menu subwindow
	clearColor $= (Color4 1 1 1 0 :: Color4 GLclampf)

	GLUT.cursor $= GLUT.LeftArrow
	selectRule 0 globalVars
	displayCallback $= displayMenu globalVars
	keyboardMouseCallback $= Just (inputCallback $ menuClick menu globalVars)
	where

	displayMenu globalVars = do
		gv <- readIORef globalVars
		clear [ColorBuffer]
		color (Color3 0 0 0 :: Color3 GLfloat)
		ruleTree <- getRules <$> readIORef globalVars
		let ruleListLength = numNodes ruleTree
		let displayLine line i = do  -- display one line of the menu
			gv <- readIORef globalVars
			if i ≡ selectedRule gv
				then GL.color (GL.Color3 1 0 0 :: GL.Color3 GL.GLfloat)
				else GL.color (GL.Color3 0 0 0 :: GL.Color3 GL.GLfloat)
			windowPos (Vertex2 0 (fromIntegral $ (ruleListLength - i - 1) * menuItemHeight + 5) :: Vertex2 GLint)
			renderString font line
		zipWithM_ displayLine (lines $ showRuleTree ruleTree) [0..]
		swapBuffers

	inputCallback handler (MouseButton button) Up modifiers (Position x y) = do
		let idx = fromIntegral y `div` menuItemHeight
		handler button idx
	inputCallback _ _ _ _ _ = return ()

	menuClick menu globalVars LeftButton idx = do
		modifyIORef globalVars $ \x -> x {selectedRule = idx}
		gv <- readIORef globalVars
		selectRule idx globalVars
		redisplay menu
	menuClick menu globalVars RightButton idx = do
		gv <- readIORef globalVars
		_ <- applyLeafRules id idx globalVars
		highlight globalVars
	menuClick _ _ _ _  = return ()
