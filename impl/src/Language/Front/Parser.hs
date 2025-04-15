-- MIT License, Copyright (c) 2024 Marvin Borner

{-# LANGUAGE OverloadedStrings #-}

module Language.Front.Parser
  ( parseProgram
  ) where

import           Control.Monad                  ( void )
import           Data.Front                     ( Action(..)
                                                , Identifier
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
identifier :: Parser Identifier
identifier = T.pack <$> some (lowerChar <|> upperChar)

-- | single mixfix operator
operator :: Parser Identifier
operator = T.pack <$> some (oneOf ("+-*/<>=?!&|" :: String))

-- | infix function: <singleton> <operator> <singleton>
-- TODO: make mixier
mixfix :: Parser Term
mixfix = do
  l  <- lexeme singleton
  op <- lexeme operator
  r  <- lexeme singleton
  return $ App (App (Var op) l) r

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
    return $ Bind name t (Unit (Var "foo"))
  unit = Unit <$> lexemeN term

-- | do block: do ( <doAction>+ )
doBlock :: Parser Term
doBlock = do
  _      <- symbol "do"
  action <- lexeme $ parens $ doAction -- TODO
  return $ Do action

-- | pure effect
pureTerm :: Parser Term
pureTerm = Pure <$> (lexeme (string "pure") *> parens term)

-- | pure effect
strictTerm :: Parser Term
strictTerm = Strict <$> (lexeme (string "strict") *> parens term)

-- | single decimal number
number :: Parser Term
number = Num <$> lexeme L.decimal

-- | single identifier (function / parameter binding)
var :: Parser Term
var = Var <$> lexeme identifier

-- | side effect (TODO: temporary!)
eff :: Parser Term
eff = Eff <$> (lexeme (string "readInt") <|> lexeme (string "writeInt"))

singleton :: Parser Term
singleton =
  pureTerm
    <|> strictTerm
    <|> ifElse
    <|> doBlock
    <|> number
    <|> try eff
    <|> var
    <|> parens block

-- | single term, potentially a left application fold of many
term :: Parser Term
term = foldl1 App <$> some (lexeme $ try mixfix <|> singleton)

-- | single term in definition-chain that's desugared to an empty definition
-- | or just the term if there's nothing next
toplevelTerm :: Parser Term
toplevelTerm = do
  t <- term
  try (Def "_" [] t <$> block) <|> return t

-- | single infix definition: <identifier> <operator> <identifier> = <term>
-- TODO: make mixier
mixDefinition :: Parser Term
mixDefinition = do
  l    <- lexeme identifier
  op   <- lexeme operator
  r    <- lexeme identifier
  _    <- symbolN "="
  body <- lexemeN term
  next <- block
  return $ Def op [l, r] body next

-- | single definition: <identifier> <identifier>* = <term>
definition :: Parser Term
definition = do
  name   <- lexeme identifier
  params <- many $ lexeme identifier
  _      <- symbolN "="
  body   <- lexemeN term
  next   <- block
  return $ Def name params body next

-- | single "let..in" block: many definitions before a single term
block :: Parser Term
block = scn *> (try mixDefinition <|> try definition <|> toplevelTerm) <* scn

-- TODO: add preprocessor commands?
program :: Parser Term
program = lexeme block

parseProgram :: Text -> Either String Term
parseProgram s = prettify $ runParser (program <* eof) "" s
 where
  prettify (Right t  ) = Right t
  prettify (Left  err) = Left $ errorBundlePretty err
