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

-- | single line comment
lineComment :: Parser ()
lineComment = L.skipLineComment "--"

-- | multiline comment
blockComment :: Parser ()
blockComment = L.skipBlockComment "{-" "-}"

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

-- | space consumer including comments
spaceConsumer :: String -> Parser ()
spaceConsumer s = L.space (void $ oneOf s) lineComment blockComment

-- | multiple spaces with comments
spaces :: Parser ()
spaces = spaceConsumer (" \t" :: String)

-- | multiple spaces and newlines with comments
anySpaces :: Parser ()
anySpaces = spaceConsumer (" \t\r\n" :: String)

-- | arbitrary symbol consumer
symbol :: Text -> Parser Text
symbol = string

-- | symbol consumer with arbitrary spaces behind
startSymbol :: Text -> Parser Text
startSymbol t = symbol t <* anySpaces

-- | abstraction, entering increases the abstraction depth
abs :: Parser Term
abs = between (startSymbol "[")
              (symbol "]")
              (gets (Abs <$> _depth) <*> (inc *> block <* dec))
 where
  inc = modify $ \r@(PS { _depth = d }) -> r { _depth = d + 1 }
  dec = modify $ \r@(PS { _depth = d }) -> r { _depth = d - 1 }

-- | de Bruijn index, parsed to de Bruijn level
idx :: Parser Term
idx = Idx <$> L.decimal

-- | single identifier, directly parsed to corresponding term
def :: Parser Term
def = do
  name                             <- identifier
  PS { _map = names, _depth = d1 } <- get
  case M.lookup name names of
    Just (d2, t) -> pure $ shift (d1 - d2) t
    Nothing      -> fail $ T.unpack name <> " is not in scope"

-- | single lambda term, potentially a left application fold of many
term :: Parser Term
term = try chain <|> once
 where
  once  = abs <|> idx <|> def <|> parens block
  chain = foldl1 App <$> sepEndBy1 (try once) (char ' ')

-- | single identifier, a-z
identifier :: Parser Text
identifier = T.pack <$> some lowerChar

-- | single definition, <identifier> = <term>
definition :: Parser ()
definition = do
  name <- identifier
  _    <- spaces
  _    <- startSymbol "="
  _    <- spaces
  body <- term
  modify $ \r@(PS { _map = m, _depth = d }) ->
    r { _map = M.insert name (d, body) m }

-- | many definitions, seperated by newline or semicolon
definitions :: Parser [()]
definitions = endBy (try definition) (oneOf (";\n" :: String) <* anySpaces)

-- | single "let..in" block: many definitions before a single term
block :: Parser Term
block = do
  (PS { _map = m }) <- get -- backup
  b                 <- definitions *> term
  _                 <- anySpaces
  modify $ \r -> r { _map = m } -- restore
  pure b

-- TODO: add preprocessor commands?
program :: Parser Term
program = block

parseProgram :: Text -> Either String Term
parseProgram s = prettify $ evalState
  (runParserT (anySpaces *> program <* anySpaces <* eof) "" s)
  PS { _map = M.empty, _depth = 0 }
 where
  prettify (Right t  ) = Right t
  prettify (Left  err) = Left $ errorBundlePretty err
