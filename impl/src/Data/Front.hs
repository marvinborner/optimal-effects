-- MIT License, Copyright (c) 2024 Marvin Borner

module Data.Front
  ( Term(..)
  , Action(..)
  , Identifier
  ) where

import           Data.List                      ( intercalate )
import           Data.Text                      ( Text )
import qualified Data.Text                     as T

type Identifier = Text

data Action = Unit Term | Bind Identifier Term Action

instance Show Action where
  show (Unit t    ) = show t
  show (Bind n t a) = show n <> " <- " <> show t <> "; " <> show a

data Term = Definition Identifier [Identifier] Term Term
          | If Term Term Term
          | Var Identifier
          | App Term Term
          | Num Int
          | Do Action

instance Show Term where
  show (Definition n params body next) =
    T.unpack n
      <> " "
      <> intercalate " " (T.unpack <$> params)
      <> " = "
      <> show body
      <> "\n"
      <> show next
  show (If clause true false) =
    "if (" <> show clause <> ") " <> show true <> " else " <> show false
  show (Var n  ) = T.unpack n
  show (App a b) = "(" <> show a <> " " <> show b <> ")"
  show (Num n  ) = show n
  show (Do  as ) = "do (" <> show as <> ")"
