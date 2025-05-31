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

data Action = Unit Term | Bind Identifier Term Action -- TODO: | Prim Term

instance Show Action where
  show (Unit t    ) = show t
  show (Bind n t a) = show n <> " <- " <> show t <> "; " <> show a

-- we parse singleton expression as `Def _ [] e next`!
data Term = Def Identifier [Identifier] Term Term
          | Rec Identifier Term Term -- only used temporarily in compilation phase
          | If Term Term Term
          | Var Identifier
          | App Term Term
          | Abs Identifier Term
          | Num Int
          | UnitV
          | Fork Term Term
          | Eff Int Identifier
          | Do Action
          | Pure Term
          | Strict Term

instance Show Term where
  show (Def n params body next) =
    T.unpack n
      <> " "
      <> intercalate " " (T.unpack <$> params)
      <> " = "
      <> show body
      <> "\n"
      <> show next
  show (Rec n rec body) = "REC(" <> T.unpack n <> ")" <> show body
  show (If clause true false) =
    "if (" <> show clause <> ") then " <> show true <> " else " <> show false
  show (Var n    ) = T.unpack n
  show (App a b  ) = "(" <> show a <> " " <> show b <> ")"
  show (Abs n b  ) = "λ" <> T.unpack n <> "." <> show b
  show (Num n    ) = show n
  show (UnitV    ) = "<>"
  show (Eff _ n  ) = T.unpack n
  show (Do     as) = "do (" <> show as <> ")"
  show (Pure   t ) = "pure (" <> show t <> ")"
  show (Strict t ) = "strict (" <> show t <> ")"
