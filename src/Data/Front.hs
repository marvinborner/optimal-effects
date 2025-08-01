-- MIT License, Copyright (c) 2024 Marvin Borner

module Data.Front
  ( Term(..)
  , ForkType(..)
  , Identifier
  ) where

import           Data.List                      ( intercalate )
import           Data.Text                      ( Text )
import qualified Data.Text                     as T

type Identifier = Text

data ForkType = Conjunctive | Disjunctive

-- we parse singleton expression as `Def _ [] e next`!
data Term = Def Identifier [Identifier] Term Term
          | Rec Identifier Term Term -- only used temporarily in compilation phase
          | If Term Term Term
          | Fork ForkType Term Term
          | Var Identifier
          | Idx Int
          | App Term Term
          | Abs Identifier Term
          | Num Int
          | Str String
          | UnitV
          | Act Identifier Int
          | Do Term
          | Unit Term
          | Bind Identifier Term Term
          | Token

instance Show Term where
  show (Def n params body next) =
    T.unpack n
      <> " "
      <> intercalate " " (T.unpack <$> params)
      <> " = "
      <> show body
      <> "\n"
      <> show next
  show (Rec n rec body) = "REC(" <> T.unpack n <> ")"
  show (If clause true false) =
    "if (" <> show clause <> ") then " <> show true <> " else " <> show false
  show (Fork Conjunctive a b) = "∧ (" <> show a <> ") (" <> show b <> ")"
  show (Fork Disjunctive a b) = "∨ (" <> show a <> ") (" <> show b <> ")"
  show (Var n               ) = T.unpack n
  show (Idx n               ) = "$" <> show n
  show (App a b             ) = "(" <> show a <> " " <> show b <> ")"
  show (Abs n b             ) = "λ" <> T.unpack n <> "." <> show b
  show (Num n               ) = show n
  show (Str s               ) = show s
  show (UnitV               ) = "<>"
  show (Act n _             ) = T.unpack n
  show (Do   as             ) = "do (" <> show as <> ")"
  show (Unit t              ) = "(return " <> show t <> ")"
  show (Bind n t a          ) = show n <> " <- " <> show t <> "; " <> show a
  show Token                  = "!"
