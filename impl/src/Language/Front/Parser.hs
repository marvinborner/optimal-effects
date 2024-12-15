-- MIT License, Copyright (c) 2024 Marvin Borner

{-# LANGUAGE OverloadedStrings #-}

module Language.Front.Parser
  ( parseProgram
  ) where

import           Control.Monad                  ( void )
import           Data.Front                     ( Action(..)
                                                , Term(..)
                                                )
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           Data.Void
import           Text.Megaparsec         hiding ( State )
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer    as L

type Parser = Parsec Void Text

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

seperator :: Parser ()
seperator = (some (oneOf (";\r\n" :: String)) <* anySpaces) >> pure ()

-- | symbol consumer with arbitrary spaces behind
symbol :: Text -> Parser Text
symbol t = string t <* anySpaces

-- | single identifier, a-z
identifier :: Parser Text
identifier = T.pack <$> some (lowerChar <|> upperChar)

-- | single mixfix operator
operator :: Parser Text
operator = T.pack <$> some (oneOf ("+-*/<>=" :: String))

-- | infix function: <singleton> <operator> <singleton>
-- TODO: make mixier
mixfix :: Parser Term
mixfix = do
  l  <- singleton
  _  <- spaces
  op <- operator
  _  <- spaces
  r  <- singleton
  return $ Mixfix op l r

-- | if expression: if (<term>) <term> else <term>
ifElse :: Parser Term
ifElse = do
  _      <- symbol "if"
  clause <- parens term
  _      <- anySpaces
  true   <- term
  _      <- anySpaces
  _      <- symbol "else"
  false  <- term
  return $ If clause true false

-- | do action: bind | unit
-- | bind: <identifier> <- <term>
-- | unit: <term>
doAction :: Parser Action
doAction = try bind <|> unit
 where
  bind = do
    name <- identifier
    _    <- spaces
    _    <- symbol "<-"
    t    <- term
    return $ Bind name t
  unit = Unit <$> term

-- | do block: do ( <doAction>+ )
doBlock :: Parser Term
doBlock = do
  _       <- symbol "do"
  actions <- parens $ doAction `sepEndBy1` seperator
  return $ Do actions

-- | single decimal number
number :: Parser Term
number = Num <$> L.decimal

-- | single identifier (function / parameter binding)
var :: Parser Term
var = Var <$> identifier

singleton :: Parser Term
singleton = ifElse <|> doBlock <|> number <|> var <|> parens block

-- | single term, potentially a left application fold of many
term :: Parser Term
term = try chain <|> once
 where
  once  = try mixfix <|> singleton
  chain = foldl1 App <$> sepEndBy1 (try once) (char ' ')

-- | single definition: <identifier> <identifier>* = <term>
definition :: Parser Term
definition = do
  _      <- anySpaces
  name   <- identifier
  _      <- spaces
  params <- identifier `sepEndBy` spaces
  _      <- symbol "="
  body   <- term
  return $ Definition name params body

-- | many definitions, seperated by newline or semicolon
definitions :: Parser [Term]
definitions = sepEndBy (try definition) seperator

-- | single "let..in" block: many definitions before a single term
block :: Parser Term
block = do
  _  <- anySpaces
  ds <- definitions
  _  <- anySpaces
  t  <- term
  _  <- anySpaces
  return $ Block ds t

-- TODO: add preprocessor commands?
program :: Parser Term
program = block

parseProgram :: Text -> Either String Term
parseProgram s = prettify
  $ runParser (anySpaces *> program <* anySpaces <* eof) "" s
 where
  prettify (Right t  ) = Right t
  prettify (Left  err) = Left $ errorBundlePretty err
