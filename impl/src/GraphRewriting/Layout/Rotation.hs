{-# LANGUAGE UnicodeSyntax #-}
module GraphRewriting.Layout.Rotation
	(module GraphRewriting.Layout.Rotation,
	 module Data.View)
where

import Prelude.Unicode
import Data.Vector.Class
import Data.Vector.V2
import Data.View
import GraphRewriting.Layout.Geometry


newtype Rotation = Rotation {rotation ∷ Angle}

type Angle = Double -- ∊ [-π,π]
type Impulse = Angle → Angle
type Momentum = Impulse → Rotation → Rotation

v01 = Vector2 0 1

meanAngle ∷ [Angle] → Angle
meanAngle as = angle v01 $ focalPoint [rotate a $ v01 | a ← as]

momSum ∷ [Momentum] → Momentum
momSum [] impulse r = r
momSum as impulse r = Rotation $ meanAngle [rotation (a impulse r) | a ← as]

approach ∷ Rotation → Momentum
approach target impulse current = momentum (angle currentV targetV) impulse current where
	targetV = rotate (rotation target) v01
	currentV = rotate (rotation current) v01

momentum ∷ Angle → Momentum
momentum a impulse = Rotation . (+) (signum a * impulse (abs a)) . rotation

angle ∷ Vector2 → Vector2 → Angle
angle u v = sign ⋅ (acos $ bound $ vdot a b) where
	sign = signum $ v2y b ⋅ v2x a - v2y a ⋅ v2x b
	a = vnormalise u
	b = vnormalise v
	bound x = max (-1) (min 1 x) -- due to a rounding error in (vdot a b), acos might yield NaN

rotate ∷ Angle → Vector2 → Vector2
rotate a (Vector2 x y) = Vector2 (x ⋅ cos a - y ⋅ sin a) (x ⋅ sin a + y ⋅ cos a)
