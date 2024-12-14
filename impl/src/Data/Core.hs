-- MIT License, Copyright (c) 2024 Marvin Borner

module Data.Core
  ( Term(..)
  , Nat(..)
  , fold
  , shift
  ) where

import           Data.Functor.Identity          ( runIdentity )
import           Prelude                 hiding ( abs
                                                , min
                                                )

data Nat = Z | S Term

data Term = Abs Int Term                 -- | Abstraction with level
          | App Term Term                -- | Application
          | Lvl Int                      -- | de Bruijn level
          | Num Nat                      -- | Peano numeral
          | Rec Term Term Term Term Term -- | Unbounded iteration

instance Show Nat where
  show Z     = "Z"
  show (S t) = "S(" <> show t <> ")"

instance Show Term where
  showsPrec _ (Abs l m) =
    showString "(\\" . shows l . showString "." . shows m . showString ")"
  showsPrec _ (App m n) =
    showString "(" . shows m . showString " " . shows n . showString ")"
  showsPrec _ (Lvl i) = shows i
  showsPrec _ (Num n) = shows n
  showsPrec _ (Rec t1 t2 u v w) =
    showString "REC ("
      . shows t1
      . showString ", "
      . shows t2
      . showString "), "
      . shows u
      . showString ", "
      . shows v
      . showString ", "
      . shows w

fold
  :: (Int -> Term -> Term)
  -> (Term -> Term -> Term)
  -> (Int -> Term)
  -> (Nat -> Term)
  -> (Term -> Term -> Term -> Term -> Term -> Term)
  -> Term
  -> Term
fold abs app lvl num rec (Abs l m) = abs l $ fold abs app lvl num rec m
fold abs app lvl num rec (App a b) =
  app (fold abs app lvl num rec a) (fold abs app lvl num rec b)
fold _   _   lvl _   _   (Lvl n          ) = lvl n
fold _   _   _   num _   (Num Z          ) = num Z
fold abs app lvl num rec (Num (S t)) = num $ S $ fold abs app lvl num rec t
fold abs app lvl num rec (Rec t1 t2 u v w) = rec
  (fold abs app lvl num rec t1)
  (fold abs app lvl num rec t2)
  (fold abs app lvl num rec u)
  (fold abs app lvl num rec v)
  (fold abs app lvl num rec w)

shift :: Int -> Term -> Term
shift 0 = id
shift n = fold (\l m -> Abs (l + n) m) App (\l -> Lvl $ l + n) Num Rec
