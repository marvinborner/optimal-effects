-- Copyright (c) 2010, Jan Rochel
-- Released under BSD-3-Clause License

{-# LANGUAGE UnicodeSyntax, MultiParamTypeClasses, FlexibleInstances #-}
-- | The multi-parameter type-class 'View' provides an abstraction @View v n@ of a type @n@ that exposes a value of type @v@. It allows both to 'inspect' and 'update' the value, while hiding the internal structure of the value type (@n@).
module Data.View where

-- TODO: Better?
--class Has v n where
--	inspect ∷ n → v
--
--class Has v n ⇒ View v n where

-- | Minimal complete definition: @inspect@ and one of {@update@, @adjust@}
class View v n where
	inspect ∷ n → v
	update  ∷ v → n → n
	adjust  ∷ (v → v) → n → n

	update v = adjust (const v)
	adjust f n = update (f $ inspect n) n

instance View n n where
	inspect = id
	update = const

instance (View v1 n, View v2 n) ⇒ View (v1,v2) n where
	inspect n = (inspect n, inspect n)
	update (v1,v2) = update v1 . update v2

instance (View v1 n, View v2 n, View v3 n) ⇒ View (v1,v2,v3) n where
	inspect n = (inspect n, inspect n, inspect n)
	update (v1,v2,v3) = update v1 . update v2 . update v3

instance (View v1 n, View v2 n, View v3 n, View v4 n) ⇒ View (v1,v2,v3,v4) n where
	inspect n = (inspect n, inspect n, inspect n, inspect n)
	update (v1,v2,v3,v4) = update v1 . update v2 . update v3 . update v4

-- | convenience function that can be used to access record fields of the exposed type
examine ∷ View v n ⇒ (v → field) → n → field
examine field = field . inspect

adjustM ∷ (Monad m, View v n) ⇒ (v → m v) → n → m n
adjustM f n = do
	n' ← f $ inspect n
	return $ update n' n
