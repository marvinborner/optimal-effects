-- MIT License, Copyright (c) 2025 Marvin Borner
{-# LANGUAGE DeriveFunctor, FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Lambda
  ( lam
  , app
  , rec
  , idx
  , act
  , dat
  , bnd
  , eta
  , tok
  , Term
  , TermF(..)
  , para
  , unwrap
  ) where

import           Data.Effects                   ( EffectData )
import           Data.Fix                       ( Fix(..) )
import qualified Data.Text                     as T
import           Text.Show.Deriving

data TermF t = Lam t
            | App t t
            | Idx Int
            | Rec t t
            | Tok
            | Cot
            | Act T.Text Int
            | Dat EffectData
            | Bnd t t
            | Eta t
            deriving Functor

instance Show t => Show (TermF t) where
  showsPrec _ (Lam m) = showString "[" . shows m . showString "]"
  showsPrec _ (App m n) =
    showString "(" . shows m . showString " " . shows n . showString ")"
  showsPrec _ (Idx i)   = shows i
  showsPrec _ Tok       = showString "<"
  showsPrec _ Cot       = showString ">"
  showsPrec _ (Rec t _) = showString "([" <> shows t <> showString "] BOX)"
  showsPrec _ (Act n _) = showString $ T.unpack n
  showsPrec _ (Dat d  ) = shows d
  showsPrec _ (Bnd t n) = shows t . showString " >>= " . shows n
  showsPrec _ (Eta t  ) = showString "(unit " . shows t . showString ")"

deriveShow1 ''TermF

type Term = Fix TermF

-- ugly hack
instance {-# OVERLAPPING #-} Show Term where
  show (Fix t) = show t

para :: Functor f => (f (Fix f, a) -> a) -> Fix f -> a
para f (Fix fx) = f (fmap (\x -> (x, para f x)) fx)

lam :: Term -> Term
lam body = Fix $ Lam body

app :: Term -> Term -> Term
app func arg = Fix $ App func arg

idx :: Int -> Term
idx n = Fix $ Idx n

-- | an application to an implicit abstraction but to a boxed term with its closure
-- | Rec a b === Abb (Abs a) <b>
rec :: Term -> Term -> Term
rec t c = Fix $ Rec t c

act :: T.Text -> Int -> Term
act name arity = Fix $ Act name arity

dat :: EffectData -> Term
dat d = Fix $ Dat d

bnd :: Term -> Term -> Term
bnd term next = Fix $ Bnd term next

eta :: Term -> Term
eta t = Fix $ Eta t

tok :: Term
tok = Fix Tok

unwrap :: Term -> TermF Term
unwrap (Fix t) = t
