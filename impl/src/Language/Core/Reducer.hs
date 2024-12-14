-- MIT License, Copyright (c) 2024 Marvin Borner

module Language.Core.Reducer
  ( nf
  ) where

import           Data.Core                      ( Nat(..)
                                                , Term(..)
                                                , shift
                                                )

data Singleton = TermTag Term | RecTag Term Term Term Term

-- TODO: There will only ever be one substitution, so don't iterate the entire tree!
-- WARNING: This function also shifts the substituted term and the levels of the parent!
-- TODO: replace with foldM?
-- In compilation this won't be necessary since Abs can have a direct pointer to the variable
subst :: Int -> Int -> Term -> Term -> Term
subst c l (Abs d m) s = Abs (d - 1) $ subst (c + 1) l m s
subst c l (App a b) s = App (subst c l a s) (subst c l b s)
subst c l (Lvl i) s | l == i    = shift c s
                    | otherwise = Lvl $ i - 1
subst _ _ (Num Z          ) _ = Num Z
subst c l (Num (S m)      ) s = Num $ S $ subst c l m s
subst c l (Rec t1 t2 u v w) s = Rec (subst c l t1 s)
                                    (subst c l t2 s)
                                    (subst c l u s)
                                    (subst c l v s)
                                    (subst c l w s)

machine :: Term -> [Singleton] -> Term
machine (App a b        ) s                    = machine a (TermTag b : s)
machine (Abs l u        ) (TermTag t : s)      = machine (subst 0 l u t) s
machine (Rec t1 t2 u v w) s = machine t1 (RecTag t2 u v w : s)
machine (Num Z          ) (RecTag _ u _ _ : s) = machine u s
machine (Num (S t1)) ((RecTag t2 u v w) : s) =
  machine v (TermTag (Rec (App w t1) (App w t2) u v w) : s)
machine (Num (S t)) s = Num $ S $ machine t s -- TODO: ??
machine t           _ = t

-- | Reduce term to normal form
nf :: Term -> Term
nf t = machine t []
