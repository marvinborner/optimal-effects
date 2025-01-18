-- MIT License, Copyright (c) 2024 Marvin Borner

module Language.Front.Transformer.Lambda
  ( transformLambda
  ) where


import           Control.Monad.Except           ( ExceptT
                                                , runExceptT
                                                , throwError
                                                )
import           Control.Monad.State            ( State
                                                , evalState
                                                , get
                                                , put
                                                )
import           Data.Front                     ( Action(..)
                                                , Identifier
                                                , Term(..)
                                                )
import qualified Data.Lambda                   as Lambda
                                                ( Term(..) )
import           Data.List                      ( elemIndex )
import qualified Data.Text                     as T

-- | Transformation monad
-- | passes a stack of identifiers for de Bruijn translation
type TransM = ExceptT String (State [Identifier])

toChurch :: Int -> Lambda.Term
toChurch = Lambda.Abs . Lambda.Abs . go
 where
  go 0 = Lambda.Idx 0
  go n = Lambda.App (Lambda.Idx 1) (go (n - 1))

-- TODO: Consider different recursion transformation
yCombinator :: Lambda.Term
yCombinator = Lambda.Abs
  (Lambda.App
    (Lambda.Abs (Lambda.App (Lambda.Idx 0) (Lambda.Idx 0))) -- [0 0]
    (Lambda.Abs -- [1 (0 0)]
      (Lambda.App (Lambda.Idx 1) (Lambda.App (Lambda.Idx 0) (Lambda.Idx 0)))
    )
  )

-- | True if n is used recursively in body
isRecursive :: Identifier -> Term -> Bool
isRecursive n (Def n' params body next)
  | n `elem` params = isRecursive n next
  | n == n'         = isRecursive n body
  | otherwise       = isRecursive n body || isRecursive n next
isRecursive n (Abs n' t) | n == n'   = False
                         | otherwise = isRecursive n t
isRecursive n (If clause true false) =
  isRecursive n clause || isRecursive n true || isRecursive n false
isRecursive n (Var n') | n == n'   = True
                       | otherwise = False
isRecursive n (App a b     ) = isRecursive n a || isRecursive n b
isRecursive _ (Num _       ) = False
isRecursive n (Do  (Unit t)) = isRecursive n t
isRecursive n (Do (Bind n' t a))
  | n == n'   = isRecursive n t
  | otherwise = isRecursive n t || isRecursive n (Do a)

-- | Wraps term in n abstractions
wrap :: Int -> Lambda.Term -> Lambda.Term
wrap 0 t = t
wrap n t = Lambda.Abs $ wrap (n - 1) t

transform :: Term -> TransM Lambda.Term
transform (Def n params body next)
  | isRecursive n body = do
    s <- get
    put (reverse (n : params) ++ s)
    b <- transform body
    put (n : s)
    d <- transform next
    let wrapped = wrap (length params) b
    return $ Lambda.App (Lambda.Abs d)
                        (Lambda.App yCombinator (Lambda.Abs wrapped))
  | otherwise = do
    s <- get
    put (reverse params ++ s)
    b <- transform body
    put (n : s)
    d <- transform next
    let wrapped = wrap (length params) b
    return $ Lambda.App (Lambda.Abs d) wrapped
transform (If clause true false) = do
  clause' <- transform clause
  true'   <- transform true
  false'  <- transform false
  return $ Lambda.App (Lambda.App clause' true') false'
transform (Var n) = do
  s <- get
  let maybeIdx = elemIndex n s
  case maybeIdx of
    Nothing  -> throwError $ "Identifier not found: " <> T.unpack n
    Just idx -> return $ Lambda.Idx idx
transform (App a b) = do
  a' <- transform a
  b' <- transform b
  return $ Lambda.App a' b'
transform (Num n) = return $ toChurch n
transform t       = error $ show t
-- transform (Do as) = 

transformLambda :: Term -> Either String Lambda.Term
transformLambda t = evalState (runExceptT $ transform t) []
