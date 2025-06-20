module GraphRewriting.Layout.Force where

import           Data.Vector.Class
import           Data.Vector.V2
import           GraphRewriting.Layout.Geometry


type Strength = Scalar -> Scalar
type Displacement = Vector2 -> Vector2 -- TODO: should be Position
type Force = Strength -> Displacement

fsum :: [Force] -> Force
fsum [] strength pos = pos
fsum fs strength pos = focalPoint [ f strength pos | f <- fs ]

repulsion :: Vector2 -> Vector2 -> Force
repulsion from pos = force (pos - from)

attraction :: Vector2 -> Vector2 -> Force
attraction towards pos = force (towards - pos)

force :: Vector2 -> Force
force f strength pos = pos + vnormalise f |* strength (vmag f)
