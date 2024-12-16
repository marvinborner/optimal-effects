-- MIT License, Copyright (c) 2024 Marvin Borner

{-# LANGUAGE OverloadedStrings #-}

module Language.Lambda.Parser
  ( parseProgram
  ) where

import           Control.Monad                  ( void )
import           Control.Monad.State
import           Data.HashMap.Strict            ( HashMap )
import qualified Data.HashMap.Strict           as M
import           Data.Lambda                    ( Term(..)
                                                , shift
                                                )
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           Data.Void
import           Prelude                 hiding ( abs
                                                , min
                                                )
import           Text.Megaparsec         hiding ( State )
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer    as L

type Parser = ParsecT Void Text (State ParserState)
data ParserState = PS
  { _map   :: HashMap Text (Int, Term)
  , _depth :: Int
  }

-- TODO: add preprocessor commands?
program :: Parser Term
program = program

parseProgram :: Text -> Either String Term
parseProgram s = prettify $ evalState (runParserT (program <* eof) "" s)
                                      PS { _map = M.empty, _depth = 0 }
 where
  prettify (Right t  ) = Right t
  prettify (Left  err) = Left $ errorBundlePretty err
