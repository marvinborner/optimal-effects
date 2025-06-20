{-# LANGUAGE UnicodeSyntax #-}
module GraphRewriting.Layout.Geometry where

import Data.Vector.V2


focalPoint ∷ [Vector2] → Vector2
focalPoint [] = error "focalPoint []"
focalPoint xs = average xs

average ∷ Fractional a ⇒ [a] → a
average [] = fromInteger 0
average xs = sum xs / fromIntegral (length xs)
