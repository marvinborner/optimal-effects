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
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer    as L

type Parser = Parsec Void Text

-- | single line comment
lineComment :: Parser ()
lineComment = L.skipLineComment "--"

-- | multiline comment
blockComment :: Parser ()
blockComment = L.skipBlockComment "{-" "-}"

-- | space consumer including comments and newlines
scn :: Parser ()
scn = L.space space1 lineComment blockComment

-- | space consumer including comments without newlines
sc :: Parser ()
sc = L.space (void $ some (oneOf (" \t" :: String))) lineComment blockComment

-- | parenthesized parser allowing newlines: ( <a> )
parens :: Parser a -> Parser a
parens = between (symbol "(" <* scn) (scn *> symbol ")")

-- | lexeme consumer without newline
lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

-- | lexeme consumer with newline
lexemeN :: Parser a -> Parser a
lexemeN = L.lexeme scn

-- | symbol consumer without newline
symbol :: Text -> Parser Text
symbol = L.symbol sc

-- | symbol consumer with newline
symbolN :: Text -> Parser Text
symbolN = L.symbol scn

-- | single identifier, a-z
identifier :: Parser Text
identifier = T.pack <$> some (lowerChar <|> upperChar)

-- | single mixfix operator
operator :: Parser Text
operator = T.pack <$> some (oneOf ("+-*/<>=?!" :: String))

-- | infix function: <singleton> <operator> <singleton>
-- TODO: make mixier
mixfix :: Parser Term
mixfix = do
  l  <- lexeme singleton
  op <- lexeme operator
  r  <- lexeme singleton
  return $ Mixfix op l r

-- | if expression: if (<term>) <term> else <term>
ifElse :: Parser Term
ifElse = do
  _      <- symbol "if"
  clause <- lexeme $ parens term
  true   <- lexemeN singleton
  _      <- symbol "else"
  false  <- lexeme singleton
  return $ If clause true false

-- | do action: bind | unit
-- | bind: <identifier> <- <term>
-- | unit: <term>
doAction :: Parser Action
doAction = try bind <|> unit
 where
  bind = do
    name <- lexeme identifier
    _    <- symbol "<-"
    t    <- lexemeN term
    return $ Bind name t
  unit = Unit <$> lexemeN term

-- | do block: do ( <doAction>+ )
doBlock :: Parser Term
doBlock = do
  _       <- symbol "do"
  actions <- lexeme $ parens $ some doAction
  return $ Do actions

-- | single decimal number
number :: Parser Term
number = Num <$> lexeme L.decimal

-- | single identifier (function / parameter binding)
var :: Parser Term
var = Var <$> lexeme identifier

singleton :: Parser Term
singleton = ifElse <|> doBlock <|> number <|> var <|> parens block

-- | single term, potentially a left application fold of many
term :: Parser Term
term = foldl1 App <$> some (lexeme $ try mixfix <|> singleton)

mixDefinition :: Parser Term
mixDefinition = do
  l    <- lexeme identifier
  op   <- lexeme (parens operator)
  r    <- lexeme identifier
  _    <- symbolN "="
  body <- lexemeN term
  return $ Definition op [l, r] body

-- | single definition: <identifier> <identifier>* = <term>
definition :: Parser Term
definition = do
  name   <- lexeme identifier
  params <- many $ lexeme identifier
  _      <- symbolN "="
  body   <- lexemeN term
  return $ Definition name params body

-- | many definitions, seperated by newline or semicolon
definitions :: Parser [Term]
definitions = many (scn *> (try mixDefinition <|> try definition))

-- | single "let..in" block: many definitions before a single term
block :: Parser Term
block = do
  ds <- lexemeN definitions
  t  <- lexemeN term
  return $ Block ds t

-- TODO: add preprocessor commands?
program :: Parser Term
program = lexeme block

parseProgram :: Text -> Either String Term
parseProgram s = prettify $ runParser (program <* eof) "" s
 where
  prettify (Right t  ) = Right t
  prettify (Left  err) = Left $ errorBundlePretty err
