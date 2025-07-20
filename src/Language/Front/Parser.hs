-- MIT License, Copyright (c) 2024 Marvin Borner

{-# LANGUAGE OverloadedStrings #-}

module Language.Front.Parser
  ( parseProgram
  ) where

import           Control.Monad                  ( void )
import           Data.Effects                   ( actionArity
                                                , builtinActions
                                                )
import           Data.Front                     ( Action(..)
                                                , ForkType(..)
                                                , Identifier
                                                , Term(..)
                                                )
import           Data.Functor                   ( ($>) )
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           Data.Void
import           Text.Megaparsec         hiding ( token )
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
identifier = T.pack <$> some (alphaNumChar <|> upperChar <|> char '_')

-- | synchronous if expression: if (<term>) then <term> else <term>
ifElseSync :: Parser Term
ifElseSync = do
  _      <- symbol "if "
  clause <- lexemeN $ parens term
  _      <- symbol "then"
  true   <- lexemeN singleton
  _      <- symbol "else"
  false  <- lexeme singleton
  -- TODO: this does not shift the de Bruijn indices! :-(
  return $ App (If clause (Abs "_" true) (Abs "_" false)) UnitV

-- | asynchronous if expression: if! (<term>) then <term> else <term>
ifElseAsync :: Parser Term
ifElseAsync = do
  _      <- symbol "if! "
  clause <- lexemeN $ parens term
  _      <- symbol "then"
  true   <- lexemeN singleton
  _      <- symbol "else"
  false  <- lexeme singleton
  return $ If clause true false

-- | fork with multiple branches: <join|race> (<branch1>) .. (<branchN>)
-- | folded directly into right-associative binary forks
fork :: Parser Term
fork = do
  forkType <- conjunctive <|> disjunctive
  branches <- some $ lexemeN $ parens term
  return $ foldr1 forkType branches
 where
  conjunctive = symbolN "join" *> pure (Fork Conjunctive)
  disjunctive = symbolN "race" *> pure (Fork Disjunctive)

-- | do action: bind | unit
-- | bind: <identifier> <- <term>
-- | unit: <term>
doAction :: Parser Action
doAction = try bind <|> try unit <|> prim
 where
  bind = do
    name <- lexeme identifier
    _    <- symbol "<-"
    t    <- lexemeN term
    next <- doAction
    return $ Bind name t next
  unit = do
    _ <- symbol "return"
    t <- lexeme term
    return $ Unit t
  prim = Prim <$> lexemeN term

-- | do block: do ( <doAction>+ )
doBlock :: Parser Term
doBlock = do
  _      <- lexeme "do "
  action <- lexeme $ parens $ doAction -- TODO
  return $ Do action

-- | single decimal number
number :: Parser Term
number = Num <$> lexeme L.decimal

-- | quoted string
stringt :: Parser Term
stringt = Str <$> (char '"' >> manyTill L.charLiteral (char '"'))

-- | Unit value
unitV :: Parser Term
unitV = lexeme (string "<>") $> UnitV

-- | single identifier (function / parameter binding)
var :: Parser Term
var = Var <$> lexeme identifier

anonymous :: Parser Term
anonymous = Abs "_" <$> absed block
  where absed = between (symbol "[" <* scn) (scn *> symbol "]")

deBruijn :: Parser Term
deBruijn = Idx <$> lexeme (char '$' *> L.decimal)

-- | effectful term
action :: Parser Term
action =
  (\x -> Act x (actionArity x)) <$> (foldl1 (<|>) (symbol <$> builtinActions))

token :: Parser Term
token = symbol "!" >> pure Token

singleton :: Parser Term
singleton =
  ifElseAsync
    <|> ifElseSync
    <|> fork
    <|> doBlock
    <|> deBruijn
    <|> stringt
    <|> number
    <|> unitV
    <|> try action
    <|> var
    <|> token
    <|> anonymous
    <|> parens block

-- | single term, potentially a left application fold of many
term :: Parser Term
term = foldl1 App <$> some (lexeme singleton)

-- | single term in definition-chain that's desugared to an empty definition
-- | or just the term if there's nothing next
toplevelTerm :: Parser Term
toplevelTerm = do
  t <- term
  try (Def "_" [] t <$> block) <|> return t

-- | single definition: let <identifier> <identifier>* = <term>
definition :: Parser Term
definition = do
  _      <- lexeme "let "
  name   <- lexeme identifier
  params <- many $ lexeme identifier
  _      <- symbolN "="
  body   <- lexemeN term
  next   <- block
  return $ Def name params body next

-- | single "let..in" block: many definitions before a single term
block :: Parser Term
block = scn *> (try definition <|> toplevelTerm) <* scn

-- TODO: add preprocessor commands?
program :: Parser Term
program = lexeme block

parseProgram :: Text -> Either String Term
parseProgram s = prettify $ runParser (program <* eof) "" s
 where
  prettify (Right t  ) = Right t
  prettify (Left  err) = Left $ errorBundlePretty err
