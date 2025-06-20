{-# LANGUAGE UnicodeSyntax, FlexibleContexts #-}
module GraphRewriting.Layout.RotPortSpec
	(module GraphRewriting.Layout.RotPortSpec,
	 module GraphRewriting.Layout.Position,
	 module GraphRewriting.Layout.Rotation,
	 module GraphRewriting.Layout.PortSpec,
	 module GraphRewriting.Layout.Geometry,
	 module Data.Vector.V2,
	 module Data.View)
where


import GraphRewriting.Graph
import GraphRewriting.Graph.Read
import GraphRewriting.Layout.Position
import GraphRewriting.Layout.Rotation
import GraphRewriting.Layout.PortSpec
import GraphRewriting.Layout.Geometry
import GraphRewriting.Pattern ()
import Data.Vector.V2
import Data.View
import Control.Monad


rotPortSpec ∷ (PortSpec n, View Rotation n) ⇒ n → [(Vector2, Vector2)]
rotPortSpec n = map (\(v, u) → (rot v, rot u)) (portSpec n)
	where rot = rotate (examine rotation n)

relRotPortPos ∷ (PortSpec n, View Rotation n) ⇒ n → [Vector2]
relRotPortPos = map fst . rotPortSpec

absRotPortSpec ∷ (PortSpec n, View Position n, View Rotation n) ⇒ n → [(Vector2, Vector2)]
absRotPortSpec n = map (\(p,d) → (examine position n + p, d)) $ rotPortSpec n

absRotPortPos ∷ (PortSpec n, View Position n, View Rotation n) ⇒ n → [Vector2]
absRotPortPos = map fst . absRotPortSpec

angularMomentum ∷ (View Position n, PortSpec n, View Rotation n, View [Port] n) ⇒ Node → WithGraph n Momentum
angularMomentum n = do
	nv ← readNode n
	let npos = examine position nv
	let edgePortions e = do
		let nps = propOfPort portDir e nv
		ns ← mapM readNode =<< adverseNodes n e
		let nsps = concatMap (propOfPort absRotPortPos e) ns
		let angles = [angle np (nsp - npos) | np ← nps, nsp ← nsps]
		return $ map (approach . Rotation) angles
	liftM (momSum . concat) (mapM edgePortions =<< attachedEdges n)
