-- MIT License, Copyright (c) 2024 Marvin Borner
-- Slightly modified version of Tromp's AIT/Lambda.lhs reducer implementation

module Language.Lambda.Reducer
  ( nf
  ) where

import           Data.Lambda                    ( Term(..) )

data HTerm = HIdx Int | HAbs (HTerm -> HTerm) | HApp HTerm HTerm
data NTerm = NVar Int | NAbs Int NTerm | NApp NTerm NTerm

(!?) :: [a] -> Int -> Maybe a
(!?) []       _ = Nothing
(!?) (x : _ ) 0 = Just x
(!?) (_ : xs) i = xs !? (i - 1)

app :: HTerm -> HTerm -> HTerm
app (HAbs f) = f
app f        = HApp f

eval :: Term -> HTerm
eval = go []
 where
  go env (Idx x) = case env !? x of
    Just v -> v
    _      -> HIdx x
  go env (Abs e    ) = HAbs $ \x -> go (x : env) e
  go env (App e1 e2) = app (go env e1) (go env e2)
  go _   _           = error "invalid"

toNamedTerm :: HTerm -> NTerm
toNamedTerm = go 0
 where
  go _ (HIdx i    ) = NVar i
  go d (HAbs f    ) = NAbs d $ go (d + 1) (f (HIdx d))
  go d (HApp e1 e2) = NApp (go d e1) (go d e2)

resolveTerm :: NTerm -> Term
resolveTerm = resolve []
 where
  resolve vs (NVar i) = Idx $ case vs !? i of
    Just v -> v
    _      -> i
  resolve vs (NAbs v t) = Abs $ resolve (v : vs) t
  resolve vs (NApp l r) = App (resolve vs l) (resolve vs r)

nf :: Term -> Term
nf = resolveTerm . toNamedTerm . eval
