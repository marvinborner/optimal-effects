{-# LANGUAGE UnicodeSyntax #-}
module GraphRewriting.GL.Render where

import           Data.IORef
import           Data.Vector.V2
import           Graphics.Rendering.MiniTypeset
                                               as TS
import           Graphics.Rendering.OpenGL      ( GLdouble )
import qualified Graphics.Rendering.OpenGL     as GL
import qualified Graphics.UI.GLUT              as GL
import           Paths_opteff
import           System.IO.Unsafe               ( unsafePerformIO )
import           Unsafe.Coerce

multiFont :: IORef (MultiFont FontFile FontStyle)
multiFont = unsafePerformIO $ newIORef $ error "multifont not loaded"

data FontFile = TextRegular
  deriving (Eq, Ord, Show)
data FontStyle = UIRegular
  deriving (Eq, Ord, Show)

fontFileMap :: FontFile -> FilePath
fontFileMap TextRegular =
  unsafePerformIO $ getDataFileName "font/LinLibertine_R.otf"

charMap :: FontStyle -> Char -> FontFile
charMap _ _ = TextRegular

styleMap :: BasicStyle -> FontStyle
styleMap Regular = UIRegular
styleMap _       = error "invalid font style"

uiUFC :: UserFontConfig FontFile FontStyle
uiUFC = UserFontConfig { _ufcFontFiles     = fontFileMap
                       , _ufcCharMap       = charMap
                       , _ufcStyleMap      = styleMap
                       , _ufcLineGapFactor = 1.0
                       }


-- | Here the OpenGL code for rendering a node can be given. The node-size is expected to be roughly 2 (radius 1) but this is not a requirement.
class Render a where render ∷ a → IO ()

convertDouble :: Double -> GLdouble
convertDouble = unsafeCoerce

convertGLdouble :: GLdouble -> Double
convertGLdouble = unsafeCoerce

vector :: Vector2 -> GL.Vector3 GLdouble
vector v = GL.Vector3 (convertDouble $ v2x v) (convertDouble $ v2y v) 0

vertex :: Vector2 -> IO ()
vertex v =
  GL.vertex $ GL.Vertex2 (convertDouble $ v2x v) (convertDouble $ v2y v)

vector2 :: (Double, Double) -> GL.Vector3 GLdouble
vector2 (x, y) = GL.Vector3 (convertDouble x) (convertDouble y) 0

vertex2 :: (Double, Double) -> IO ()
vertex2 (x, y) = GL.vertex $ GL.Vertex2 (convertDouble x) (convertDouble y)

renderStringCustom
  :: (Float, Float, Float) -> Int -> Double -> Double -> String -> IO ()
renderStringCustom color height x y label = GL.preservingMatrix $ do
  GL.translate $ vector2 (-0.3, 0.5)
  GL.scale 0.007 0.007 (0 :: GL.GLdouble)
  GL.scale (1.0) (-1.0) (1.0 :: GL.GLfloat)
  let document =
        Identified "str" (WithColor (TS.tripleToCol color) (String label))
  mf     <- readIORef multiFont
  layout <- TS.createLayout mf (Height height) document
  TS.renderLayout layout (Pos x y)

renderString :: Int -> String -> IO ()
renderString height = renderStringCustom (0.0, 0.0, 0.0) height 0 0
