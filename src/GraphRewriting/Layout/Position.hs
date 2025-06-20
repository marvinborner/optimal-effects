{-# LANGUAGE UnicodeSyntax #-}
module GraphRewriting.Layout.Position
	(module GraphRewriting.Layout.Position,
	 module Data.Vector.V2,
	 module Data.View)
where

import Data.Vector.V2
import Data.View


newtype Position = Position {position ∷ Vector2}
