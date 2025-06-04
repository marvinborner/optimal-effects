-- MIT License, Copyright (c) 2025 Marvin Borner
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Lambda
  ( lam
  , app
  , idx
  , act
  , dat
  , Term
  , TermF(..)
  , unwrap
  ) where

import           Data.Effects                   ( EffectData )
import           Data.Fix                       ( Fix(..) )
import qualified Data.Text                     as T
import           Text.Show.Deriving

data TermF t = Lam t
            | App t t
            | Idx Int
            | Tok
            | Cot
            | Act T.Text Int
            | Dat EffectData
            deriving Functor

instance Show t => Show (TermF t) where
  showsPrec _ (Lam m) = showString "[" . shows m . showString "]"
  showsPrec _ (App m n) =
    showString "(" . shows m . showString " " . shows n . showString ")"
  showsPrec _ (Idx i)   = shows i
  showsPrec _ Tok       = showString "<"
  showsPrec _ Cot       = showString ">"
  showsPrec _ (Act n _) = shows n
  showsPrec _ (Dat d  ) = shows d

deriveShow1 ''TermF

type Term = Fix TermF

lam :: Term -> Term
lam body = Fix $ Lam body

app :: Term -> Term -> Term
app func arg = Fix $ App func arg

idx :: Int -> Term
idx n = Fix $ Idx n

act :: T.Text -> Int -> Term
act name arity = Fix $ Act name arity

dat :: EffectData -> Term
dat d = Fix $ Dat d

unwrap :: Term -> TermF Term
unwrap (Fix t) = t
