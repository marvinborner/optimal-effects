{-# LANGUAGE UnicodeSyntax #-}
module GraphRewriting.GL.Render where

import Data.Vector.V2
import Graphics.Rendering.OpenGL (GLdouble)
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLUT as GL
import Unsafe.Coerce


-- | Here the OpenGL code for rendering a node can be given. The node-size is expected to be roughly 2 (radius 1) but this is not a requirement.
class Render a where render ∷ a → IO ()

convertDouble ∷ Double → GLdouble
convertDouble = unsafeCoerce

convertGLdouble ∷ GLdouble → Double
convertGLdouble = unsafeCoerce

vector ∷ Vector2 → GL.Vector3 GLdouble
vector v = GL.Vector3 (convertDouble $ v2x v) (convertDouble $ v2y v) 0

vertex ∷ Vector2 → IO ()
vertex v = GL.vertex $ GL.Vertex2 (convertDouble $ v2x v) (convertDouble $ v2y v)

vector2 ∷ (Double,Double) → GL.Vector3 GLdouble
vector2 (x,y) = GL.Vector3 (convertDouble x) (convertDouble y) 0

vertex2 ∷ (Double,Double) → IO ()
vertex2 (x,y) = GL.vertex $ GL.Vertex2 (convertDouble x) (convertDouble y)

renderString ∷ String → IO ()
renderString label = GL.preservingMatrix $ do
	GL.translate $ vector2 (-0.3,-0.3)
	GL.scale 0.007 0.007 (0 ∷ GL.GLdouble)
	GL.renderString GL.MonoRoman label
