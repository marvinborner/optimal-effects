{-# LANGUAGE UnicodeSyntax, FlexibleInstances, MultiParamTypeClasses, FlexibleContexts #-}
module GraphRewriting.Layout.Wrapper
	(Wrapper (..), wrapGraph, wrappee, updateWrappee,
	 module Data.View,
	 module GraphRewriting.Graph.Types,
	 module GraphRewriting.Layout.Position,
	 module GraphRewriting.Layout.PortSpec,
	 module GraphRewriting.Layout.Rotation,
	 module GraphRewriting.Layout.RotPortSpec,
	 module Data.Vector.V2)
where

import Data.View
import GraphRewriting.Graph
import GraphRewriting.Graph.Types
import GraphRewriting.Layout.Position
import GraphRewriting.Layout.PortSpec
import GraphRewriting.Layout.Rotation
import GraphRewriting.Layout.RotPortSpec
import Data.Vector.Class
import Data.Vector.V2


-- | Wraps a value of type @w@, augmenting it with layout information
data Wrapper w = Wrapper {wRot ∷ Rotation, wPos ∷ Position, wrappee ∷ w}

instance {-# INCOHERENT #-} View v n ⇒ View v (Wrapper n) where
	inspect = inspect . wrappee
	adjust f w = w {wrappee = adjust f $ wrappee w}

instance PortSpec n ⇒ PortSpec (Wrapper n) where portSpec = portSpec . wrappee

instance {-# OVERLAPPING #-} View Rotation (Wrapper n) where
	inspect = wRot
	update v w = w {wRot = v}

instance {-# OVERLAPPING #-} View Position (Wrapper n) where
	inspect = wPos
	update v w = w {wPos = v}

updateWrappee v n = n {wrappee = v}

-- | Wraps the nodes of a graph, augmenting them with layout information
wrapGraph ∷ Graph n → Graph (Wrapper n)
wrapGraph = unsafeMapNodesUnique wrapNode where
	wrapNode k n = Wrapper {wRot = Rotation 0, wPos = Position $ genPos k, wrappee = n}
	genPos k = rotate (fromIntegral k) (vpromote f)
		where f = fromIntegral $ k `mod` 3
