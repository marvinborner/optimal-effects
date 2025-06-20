{-# LANGUAGE UnicodeSyntax, FlexibleContexts #-}
module GraphRewriting.GL.Canvas (setupCanvas) where

import Prelude.Unicode
import qualified Graphics.UI.GLUT as GL
import Graphics.Rendering.OpenGL (($=))
import GraphRewriting.Graph
import GraphRewriting.Graph.Read
import GraphRewriting.Pattern
import qualified GraphRewriting.Graph.Write.Unsafe as Unsafe
import GraphRewriting.GL.Render
import GraphRewriting.GL.Global
import Data.IORef
import GraphRewriting.Layout.Rotation
import GraphRewriting.Layout.Position
import qualified Data.Set as Set
import Data.Functor ()
import Data.Maybe (catMaybes, listToMaybe)
import Data.Vector.Class


setupCanvas ∷ (View Position n, Render n', View Rotation n', View Position n')
            ⇒ (Graph n → Graph n') → (Edge → [n'] → [(Vector2, Vector2)]) → IORef (GlobalVars n) → IO GL.Window
setupCanvas project hyperEdgeToLines globalVars = do
	canvas ← GL.createWindow "Graph"
	GL.clearColor $= (GL.Color4 1 1 1 0 ∷ GL.Color4 GL.GLclampf)
	GL.lineWidth $= 2
	aspect ← newIORef 1
	focus  ← newIORef $ GL.Vector3 0 0 0
	zoom   ← newIORef (1 ∷ GL.GLdouble)
	origLayoutStep ← layoutStep <$> readIORef globalVars
	registerCallbacks origLayoutStep aspect focus zoom project hyperEdgeToLines globalVars
	GL.cursor $= GL.LeftArrow
	return canvas

registerCallbacks origLayoutStep aspect focus zoom project hyperEdgeToLines globalVars = do
	autozoom
	GL.displayCallback $= display
	GL.reshapeCallback $= Just reshape
	GL.keyboardMouseCallback $= Just inputCallback

	where

	zoomBy factor = modifyIORef zoom (* factor) >> readIORef globalVars >>= redisplay . canvas

	inputCallback (GL.MouseButton GL.WheelUp)   _ _ _ = zoomBy 1.1
	inputCallback (GL.MouseButton GL.WheelDown) _ _ _ = zoomBy 0.9

	inputCallback (GL.MouseButton GL.RightButton) GL.Down mod pos = do
		pause globalVars
		node ← nodeAt pos
		case node of
			Nothing → return ()
			Just n  → do
				_ ← (\idx → applyLeafRules (nextIs n) idx globalVars) . selectedRule =<< readIORef globalVars
				highlight globalVars

	inputCallback (GL.MouseButton GL.RightButton) GL.Up mod (GL.Position x y) = resume globalVars

	inputCallback (GL.MouseButton GL.LeftButton) GL.Up mod pos = do
		modifyIORef globalVars $ \v → v {layoutStep = origLayoutStep}
		GL.addTimerCallback 50 $ GL.motionCallback $= Nothing
	inputCallback (GL.MouseButton GL.LeftButton) GL.Down mod from = do
		node ← nodeAt from
		case node of
			Nothing → GL.motionCallback $= Just (scrollCallback from)
			Just n  → do
				let fixN node = if n ≡ node
					then do
						pos ← examineNode position node
						origLayoutStep node
						Unsafe.updateNode node (Position pos)
					else origLayoutStep node
				modifyIORef globalVars $ \v → v {layoutStep = fixN}
				GL.motionCallback $= Just (dragCallback n)
		where
		dragCallback n to = do
			GL.motionCallback $= Nothing
			GL.Vertex3 tx ty _ ← unproject to
			let v = Vector2 (convertGLdouble tx) (convertGLdouble ty)
			modifyGraph (execGraph $ Unsafe.updateNode n (Position v)) globalVars
			GL.addTimerCallback 40 $ GL.motionCallback $= Just (dragCallback n)

		scrollCallback from to = do
			GL.motionCallback $= Nothing
			GL.Vertex3 fx fy _ ← unproject from
			GL.Vertex3 tx ty _ ← unproject to
			modifyIORef focus $ \(GL.Vector3 x y _) → GL.Vector3 (x + tx - fx) (y + ty - fy) 0
			redisplay . canvas =<< readIORef globalVars
			GL.addTimerCallback 40 $ GL.motionCallback $= Just (scrollCallback to)

	inputCallback (GL.Char 'z') GL.Up _ _ = autozoom
	inputCallback (GL.Char ' ') GL.Up _ _ = do
		isPaused ← paused <$> readIORef globalVars
		if isPaused then resume globalVars else pause globalVars
	inputCallback _ _ _ _ = return ()

	autozoom = let margin = 2 in do
		ns ← nodes . graph <$> readIORef globalVars
		let maxDist = maximum $ map abs $ concat [[v2x p, v2y p] | p ← examine position `map` ns]
		writeIORef focus $ GL.Vector3 0 0 0
		writeIORef zoom $ 1 / (convertDouble maxDist + margin)

	display = do
		GL.clear [GL.ColorBuffer]
		GL.loadIdentity
		a ← readIORef aspect
		if a < 1 then GL.ortho2D (-1) 1 (-1/a) (1/a) else GL.ortho2D (-1*a) (1*a) (-1) 1
		z ← readIORef zoom
		GL.scale z z 1
		GL.translate =<< readIORef focus

		GL.color (GL.Color3 0 0 0 ∷ GL.Color3 GL.GLfloat)
		g ← project . graph <$> readIORef globalVars
		mapM_ (uncurry renderLine) (concatMap (uncurry hyperEdgeToLines) (edges g))
		hl ← highlighted <$> readIORef globalVars
		mapM_ (renderNode hl) (evalGraph readNodeList g `zip` nodes g)
		w ← menu <$> readIORef globalVars
		redisplay w -- redisplay the menu subwindow

		GL.swapBuffers

	nodeAt ∷ GL.Position → IO (Maybe Node)
	nodeAt glPos = do
		GL.Vertex3 x y _ ← unproject glPos
		let pos = Vector2 (convertGLdouble x) (convertGLdouble y)
		g ← project . graph <$> readIORef globalVars
		return $ listToMaybe $ catMaybes $ evalGraph (readOnly $ withNodes $ checkPos pos) g
		where checkPos pos n = do
			npos ← examineNode position n
			return $ if vmag (pos - npos) < 1 then Just n else Nothing

	reshape s@(GL.Size w h) = do
		writeIORef aspect newAspect
		GL.viewport $= (GL.Position 0 0, s)
		GL.matrixMode $= GL.Projection
		GL.loadIdentity
		GL.perspective 0 newAspect (-1) 1
		GL.matrixMode $= GL.Modelview 0
		where newAspect = fromIntegral w / fromIntegral (max 1 h)

unproject ∷ GL.Position → IO (GL.Vertex3 GL.GLdouble)
unproject (GL.Position x y) = do
	GL.Size winWidth winHeight ← GL.get GL.windowSize
	let pos = GL.Vertex3 (fromIntegral x) (fromIntegral $ fromIntegral winHeight - y) 0
	modelview  ← getMatrix (GL.Modelview 1)
	projection ← getMatrix GL.Projection
	viewport   ← GL.get GL.viewport
	GL.unProject pos modelview projection viewport

getMatrix ∷ GL.MatrixMode → IO (GL.GLmatrix GL.GLdouble)
getMatrix = GL.get . GL.matrix . Just

renderNode ∷ (Render n, View Position n, View Rotation n) ⇒ Set.Set Node → (Node,n) → IO ()
renderNode highlighted (ref,n) = GL.preservingMatrix $ do
	GL.translate (vector $ examine position n)
	GL.rotate (convertDouble $ examine rotation n * 180 / pi) (GL.Vector3 0 0 1 ∷ GL.Vector3 GL.GLdouble)
	if ref `Set.member` highlighted
		then GL.color (GL.Color3 1 0 0 ∷ GL.Color3 GL.GLfloat)
		else GL.color (GL.Color3 0 0 0 ∷ GL.Color3 GL.GLfloat)
	render n

renderLine ∷ Vector2 → Vector2 → IO ()
renderLine p1 p2 = GL.renderPrimitive GL.Lines $ vertex p1 >> vertex p2
