-- MIT License, Copyright (c) 2024 Marvin Borner

module Data.Front
  ( Term(..)
  , Action(..)
  ) where

import           Data.Text                      ( Text )

type Identifier = Text

data Action = Unit Term | Bind Identifier Term
  deriving Show

data Term = Definition Identifier [Identifier] Term
          | Block [Term] Term
          | If Term Term Term
          | Var Identifier
          | App Term Term
          | Num Int
          | Mixfix Identifier Term Term
          | Do [Action]
          deriving Show
