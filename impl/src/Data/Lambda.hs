-- MIT License, Copyright (c) 2024 Marvin Borner

module Data.Lambda
  ( Term(..)
  , fold
  , shift
  ) where

import           Prelude                 hiding ( abs )

data Term = Abs Int Term                 -- | Abstraction with level
          | App Term Term                -- | Application
          | Idx Int                      -- | de Bruijn Index

instance Show Term where
  showsPrec _ (Abs l m) =
    showString "(\\" . shows l . showString "." . shows m . showString ")"
  showsPrec _ (App m n) =
    showString "(" . shows m . showString " " . shows n . showString ")"
  showsPrec _ (Idx i) = shows i

fold
  :: (Int -> Term -> Term)
  -> (Term -> Term -> Term)
  -> (Int -> Term)
  -> Term
  -> Term
fold abs app idx (Abs l m) = abs l $ fold abs app idx m
fold abs app idx (App a b) = app (fold abs app idx a) (fold abs app idx b)
fold _   _   idx (Idx n  ) = idx n

shift :: Int -> Term -> Term
shift 0 = id
shift n = fold (\l m -> Abs (l + n) m) App (\l -> Idx $ l + n)
