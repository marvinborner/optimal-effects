-- MIT License, Copyright (c) 2024 Marvin Borner

module Language.Front.Transformer.Lambda
  ( transformLambda
  ) where


import           Control.Monad.Except
import           Control.Monad.State
import           Data.Effects
import           Data.Front
import qualified Data.Lambda                   as Lambda
import           Data.List                      ( elemIndex )
import qualified Data.Text                     as T

-- | Transformation monad
-- | passes a stack of identifiers for de Bruijn translation
type TransM = ExceptT String (State [Identifier])

-- | True if n is used recursively in body
isRecursive :: Identifier -> Term -> Bool
isRecursive n = \case
  Def n' params body next
    | n `elem` params -> isRecursive n next
    | n == n'         -> isRecursive n body
    | otherwise       -> isRecursive n body || isRecursive n next
  Abs n' t | n == n'   -> False
           | otherwise -> isRecursive n t
  If clause true false ->
    isRecursive n clause || isRecursive n true || isRecursive n false
  Var n' | n == n'   -> True
         | otherwise -> False
  App a b     -> isRecursive n a || isRecursive n b
  UnitV       -> False
  Num _       -> False
  Act _ _     -> False
  Do (Unit t) -> isRecursive n t
  Do (Bind n' t a) | n == n'   -> isRecursive n t
                   | otherwise -> isRecursive n t || isRecursive n (Do a)
  e -> error $ show e

-- | Wraps term in n abstractions
wrap :: Int -> Lambda.Term -> Lambda.Term
wrap 0 t = t
wrap n t = Lambda.lam $ wrap (n - 1) t

transform :: Term -> TransM Lambda.Term
transform = \case
  Def n params body next
    | isRecursive n body -> do
      error ""
    |
      -- s <- get
      -- put (reverse (n : params) ++ s)
      -- b <- transform body
      -- put (n : s)
      -- d <- transform next
      -- let wrapped = wrap (length params) b
      -- return $ Lambda.app (Lambda.lam d)
      --                     (Lambda.app yCombinator (Lambda.lam wrapped))
      otherwise -> do
      s <- get
      put (reverse params ++ s)
      b <- transform body
      put (n : s)
      d <- transform next
      let wrapped = wrap (length params) b
      return $ Lambda.app (Lambda.lam d) wrapped
  If clause true false -> do
    clause' <- transform clause
    true'   <- transform true
    false'  <- transform false
    return $ Lambda.app (Lambda.app clause' true') false'
  Var n -> do
    s <- get
    let maybeIdx = elemIndex n s
    case maybeIdx of
      Nothing  -> throwError $ "Identifier not found: " <> T.unpack n
      Just idx -> return $ Lambda.idx idx
  App a b -> do
    a' <- transform a
    b' <- transform b
    return $ Lambda.app a' b'
  Num n   -> return $ Lambda.dat (NumberData n)
  UnitV   -> return $ Lambda.dat UnitData
  Act a n -> return $ Lambda.act a n
  t       -> error $ show t

transformLambda :: Term -> Either String Lambda.Term
transformLambda t = evalState (runExceptT $ transform t) []
